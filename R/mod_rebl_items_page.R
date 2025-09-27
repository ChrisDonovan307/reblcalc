# REBL Item Page

rebl_items_page_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("rebl_items_page"))
}

rebl_items_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Title and information
    output$rebl_items_title <- renderUI({
      HTML(
        '<h3 class="body-header-3">REBL Items</h3>
        <p>Below is a list of all 24 REBL Items. They are organized by category
        and include behaviors related to food and drink, home energy use,
        packaging, purchasing, social behaviors, and water use. This set of items
        was designed to include a broad range of difficulties, limit recall bias,
        and to be fast and easy to administer.</p>'
      )
    })

    # Table of REBL item table
    output$rebl_items_table <- renderReactable({
      rebl_text %>%
        dplyr::mutate('#' = row_number(), .before = 1) %>%
        get_reactable(
          searchable = TRUE,
          columns = list(
            '#' = colDef(minWidth = 20),
            `REBL Item` = colDef(minWidth = 100),
            `In the last week, have you...` = colDef(minWidth = 250)
          )
        )
    })

    # Combine into single page output
    output$rebl_items_page <- renderUI({
      tagList(
        fluidRow(
          column(12, uiOutput(ns('rebl_items_title'))),
          column(12, reactableOutput(ns('rebl_items_table')))
        )
      )
    })

  })
}
