box::use(
  shiny,
  readr[read_csv],
  readxl[read_excel],
  dplyr[mutate, across, all_of, select],
  tools[file_ext]
)

# Load REBL items data
load('app/data/rebl_items.rda')


#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(

    # Data source selection
    shiny$div(
      class = 'button-box',

      shiny$radioButtons(
        ns('data_source'),
        label = shiny$HTML('<b>Choose data source:</b>'),
        choices = list(
          'Upload your own data' = 'upload',
          'Use example data' = 'example'
        ),
        selected = 'upload',
        width = '100%'
        # inline = TRUE
      ),

      # Conditional UI for file upload
      shiny$conditionalPanel(
        condition = "input.data_source == 'upload'",
        ns = ns,
        shiny$div(
          style = 'padding: 0px 10px 0px 10px;',
          shiny$fileInput(
            ns('file'),
            shiny$HTML('<b>Upload .csv or .xlsx file:</b>'),
            accept = c('.csv', '.xlsx', '.xls')
          )
        )
      ),

      # Show info about example data when selected
      shiny$conditionalPanel(
        condition = "input.data_source == 'example'",
        ns = ns,
        shiny$div(
          style = 'padding: 0;',
          shiny$HTML(
            "<b>Example Dataset:</b> This is a simulated dataset with 500
            responses to the 24 original REBL items. You can analyze it with
            the options provided below to test out the app."
          )
        )
      )

    ), # end button-box div

    # Select respondent ID column
    shiny$uiOutput(ns('dropdown_columns'))
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Import File reactive - handles both uploaded files and example data
    rval_df <- shiny$reactive({

      if (input$data_source == 'example') {
        # Load example data
        load('app/data/example.rda')
        df <- example
      } else {
        # Make sure file is uploaded for upload option
        shiny$req(input$file)

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
          shiny$showModal(shiny$modalDialog(
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
            footer = shiny$modalButton('OK')
          ))
        }
      }

      return(df)
    })

    # Get column names to send back to UI to choose respondent ID
    output$dropdown_columns <- shiny$renderUI({
      shiny$req(rval_df())
      column_names <- colnames(rval_df())
      shiny$div(
        class = 'button-box',
        shiny$tagList(
          shiny$selectInput(
            ns('respondent_id'),
            shiny$HTML('<b>Select Respondent ID Column</b>'),
            choices = column_names
          )
        )
      )
    })

    # Return reactive values
    return(list(
      rval_df = rval_df,
      file_input = shiny$reactive(input$file),
      respondent_id = shiny$reactive(input$respondent_id),
      data_source = shiny$reactive(input$data_source)
    ))

  })
}
