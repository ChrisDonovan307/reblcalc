box::use(
  shiny,
  reactable[renderReactable, reactableOutput],
  dplyr[`%>%`]
)

box::use(
  app/logic/get_item_fit[get_item_fit],
  app/logic/get_reactable[get_reactable],
  app/logic/show_placeholder[show_placeholder],
)

# Load REBL items data
load('app/data/rebl_items.rda')

# Mod Item Fit

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$uiOutput(ns('item_fit'))
}

#' @export
server <- function(id,
                   rval_model,
                   rval_df_clean,
                   analysis_state) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    item_fit_data <- shiny$reactive({
      rval_model() %>%
        get_item_fit(rval_df_clean(), rebl_items)
    })

    output$item_fit_title <- shiny$renderUI({
      shiny$HTML('<h3 class="body-header-3">Item Fit</h3>')
    })

    output$item_fit_exp <- shiny$renderText({
      if (analysis_state()) {
        shiny$HTML(
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
      shiny$req(analysis_state(), item_fit_data())
      get_reactable(item_fit_data())
    })

    output$item_fit <- shiny$renderUI({
      if (!analysis_state()) {
        return(show_placeholder())
      }

      shiny$req(rval_model())
      shiny$tagList(
        shiny$fluidRow(
          shiny$column(12, shiny$uiOutput(ns('item_fit_title'))),
          shiny$column(12, shiny$uiOutput(ns('item_fit_exp'))),
          shiny$column(12, reactable::reactableOutput(ns('item_fit_data')))
        )
      )
    })

    return(item_fit_data)

  })
}
