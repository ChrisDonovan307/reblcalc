box::use(
  shiny[NS, tagList, div, HTML, radioButtons, conditionalPanel, fileInput, uiOutput, moduleServer, reactive, req, renderUI, selectInput, showModal, modalDialog, modalButton],
  readr[read_csv],
  readxl[read_excel],
  dplyr[mutate, across, all_of, select],
  tools[file_ext]
)

# Load REBL items data
load('app/data/rebl_items.rda')


#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(

    # Data source selection
    div(
      class = 'button-box',

      radioButtons(
        ns('data_source'),
        label = HTML('<b>Choose data source:</b>'),
        choices = list(
          'Upload your own data' = 'upload',
          'Use example data' = 'example'
        ),
        selected = 'upload',
        width = '100%'
        # inline = TRUE
      ),

      # Conditional UI for file upload
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        ns = ns,
        div(
          style = 'padding: 0px 10px 0px 10px;',
          fileInput(
            ns('file'),
            HTML('<b>Upload .csv or .xlsx file:</b>'),
            accept = c('.csv', '.xlsx', '.xls')
          )
        )
      ),

      # Show info about example data when selected
      conditionalPanel(
        condition = "input.data_source == 'example'",
        ns = ns,
        div(
          style = 'padding: 0;',
          HTML(
            "<b>Example Dataset:</b> This is a simulated dataset with 500
            responses to the 24 original REBL items. You can analyze it with
            the options provided below to test out the app."
          )
        )
      )

    ), # end button-box div

    # Select respondent ID column
    uiOutput(ns('dropdown_columns'))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Import File reactive - handles both uploaded files and example data
    rval_df <- reactive({

      if (input$data_source == 'example') {
        # Load example data
        load('app/data/example.rda')
        df <- example
      } else {
        # Make sure file is uploaded for upload option
        req(input$file)

        # Read data frame from uploaded file
        if (tools::file_ext(input$file$name) == "csv") {
          df <- read_csv(input$file$datapath)
        } else if (tools::file_ext(input$file$name) %in% c("xlsx", "xls")) {
          df <- read_excel(input$file$datapath)
        } else {
          stop("Unsupported file format.")
        }
      }

      # Make sure rebl items are numeric
      df <- df %>%
        mutate(across(all_of(rebl_items), as.numeric))

      # Make sure all REBL items are in DF
      if (!all(rebl_items %in% names(df))) {
        stop(
          'File must contain all 24 REBL items by name. Reload app and check
            Info tab for a list of items.'
        )
      }

      # Make sure no one answered either all 1 or all 0
      bad_patterns <- which(rowSums(select(df, all_of(rebl_items)), na.rm = TRUE) %in% c(0, 24))
      if (length(bad_patterns) > 0) {
        stop(
          'Cannot calculate REBL Score for person(s) ',
          paste0(bad_patterns, collapse = ', '),
          ' due to unacceptable pattern of responses. Check to make sure they ',
          'answered at least one question "yes" and at least one question "no".'
        )
      }

      # Notify them if there is a lot of missing data
      if (all(rebl_items %in% names(df))) {
        n_miss <- df %>%
          select(all_of(rebl_items)) %>%
          is.na() %>%
          sum()
        if (n_miss >= 2000) {
          showModal(modalDialog(
            title = 'Warning',
            paste0(
              'There are ',
              format(n_miss, big.mark = ','),
              ' missing values in the REBL item columns of your dataset. Be
              aware that whether or not you decide to impute these data, the
              run time of your analysis could increase substantially. Explore
              your data in the "Skim" tab to see where the missing values are.'
            ),
            easyClose = TRUE,
            footer = modalButton('OK')
          ))
        }
      }

      return(df)
    })

    # Get column names to send back to UI to choose respondent ID
    output$dropdown_columns <- renderUI({
      req(rval_df())
      column_names <- colnames(rval_df())
      div(
        class = 'button-box',
        tagList(
          selectInput(
            ns('respondent_id'),
            HTML('<b>Select Respondent ID Column</b>'),
            choices = column_names
          )
        )
      )
    })

    # Return reactive values
    return(list(
      rval_df = rval_df,
      file_input = reactive(input$file),
      respondent_id = reactive(input$respondent_id),
      data_source = reactive(input$data_source)
    ))

  })
}
