# REBL Item Page


rebl_items_page_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("rebl_items_page"))
}

rebl_items_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$rebl_items_page <- renderUI({
      tagList(
        fluidRow(
          column(12, uiOutput(ns('rebl_items_title'))),
          column(12, tableOutput(ns('rebl_items_text')))
        )
      )
    })

    output$rebl_items_title <- renderUI({
      HTML(
        '<h3 style="font-weight: bold; color: #2F4F4F">REBL Items</h3>
        <p>Below is a list of all 24 REBL Items. They are organized by category
        and include behaviors related to food and drink, home energy use,
        packaging, purchasing, social behaviors, and water use. This set of items
        was designed to include a broad range of difficulties, limit recall bias,
        and to be fast and easy to administer.</p>'
      )
    })

    # Add REBL Items table
    output$rebl_items_text <- renderTable({
        rebl_text
    })

  })
}
