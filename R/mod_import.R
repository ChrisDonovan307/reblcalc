# Import Module for REBL Score Calculator

# UI for file import section
import_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Upload data
    fileInput(
      ns('file'),
      'Upload .csv or .xlsx file',
      accept = c('.csv', '.xlsx', '.xls')
    ),

    # Select respondent ID column
    uiOutput(ns('dropdown_columns'))
  )
}

# Server logic for file import
import_server <- function(id, rebl_items) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Import File reactive
    rval_df <- eventReactive(input$file, {

      # Make sure file is uploaded
      req(input$file)

      # Read data frame from uploaded file
      if (tools::file_ext(input$file$name) == "csv") {
        df <- read_csv(input$file$datapath)
      } else if (tools::file_ext(input$file$name) %in% c("xlsx", "xls")) {
        df <- read_excel(input$file$datapath)
      } else {
        stop("Unsupported file format.")
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
      tagList(
        selectInput(
          ns('respondent_id'),
          'Select Person ID Column',
          choices = column_names
        )
      )
    })

    # Return reactive values
    return(list(
      rval_df = rval_df,
      file_input = reactive(input$file),
      respondent_id = reactive(input$respondent_id)
    ))
  })
}