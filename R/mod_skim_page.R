# Module Skim Page

skim_page_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("skim_page"))
}

skim_page_server <- function(id, rval_df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$skim_page <- renderUI({
      req(rval_df())
      tagList(
        fluidRow(
          column(12, uiOutput(ns('skim_title'))),
          column(12, verbatimTextOutput(ns('skim')))
        )
      )
    })

    output$skim_title <- renderUI({
      req(rval_df())
      HTML(
        '<h3 style="color: #2F4F4F; font-weight: bold;">Skim</h3>
        <p>Explore summary statistics and missing values from your dataset.</p>'
      )
    })

    output$skim <- renderPrint({
      req(rval_df())
      skim(rval_df(), .data_name = 'Your_Data') %>%
        select(-ends_with('hist'))
    })

  })
}
