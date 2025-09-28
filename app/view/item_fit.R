box::use(
  shiny[NS, uiOutput, moduleServer, reactive, renderUI, HTML, renderText],
  reactable[renderReactable, reactableOutput]
)

# Load REBL items data
load('app/data/rebl_items.rda')

# Mod Item Fit

ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns('item_fit'))
}

server <- function(id,
                   rval_model,
                   rval_df_clean,
                   analysis_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    item_fit_data <- reactive({
      rval_model() %>%
        get_item_fit(rval_df_clean(), rebl_items)
    })

    output$item_fit_title <- renderUI({
      HTML('<h3 class="body-header-3">Item Fit</h3>')
    })

    output$item_fit_exp <- renderText({
      if (analysis_state()) {
        HTML(
          '<ul>
            <li><b>prop</b> is the proportion of people who performed the
              pro-environmental behavior in the last week.</li>
            <li><b>i.fit</b> is the item fit, as measured by the standardized
            squared residuals between model implied response patterns and observed
            response patterns.</li>
            <li><b>outfit</b> is an outlier-sensitive fit. It weights items and
              respondents that are farther away from one another in the latent
              trait.</li>
            <li><b>infit</b> is the information-weighted fit, which weights nearby
              items and respondents more than those that are farther away.</li>
            <li><b>MSQ</b> is the mean square error. 1.0 is the expected value.
              Smaller values are more predictable, while larger values are less
              predictable.</li>
            <li><b>Z</b> is the z-score of the test of fit. 0.0 is expected, while
             values over +/-2 are misfitting.</li>
            <li><b>eta.se</b> is the standard error of the item difficulty.</li>
          </ul>'
        )
      }
    })

    output$item_fit_data <- renderReactable({
      req(analysis_state(), item_fit_data())
      get_reactable(item_fit_data())
    })

    output$item_fit <- renderUI({
      req(rval_model())
      tagList(
        fluidRow(
          column(12, uiOutput(ns('item_fit_title'))),
          column(12, uiOutput(ns('item_fit_exp'))),
          column(12, reactable::reactableOutput(ns('item_fit_data')))
        )
      )
    })

    return(item_fit_data)

  })
}
