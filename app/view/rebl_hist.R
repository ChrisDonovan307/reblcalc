box::use(
  shiny,
  ggplot2[ggplot, aes, geom_histogram, theme_classic, labs, scale_x_continuous, theme, element_text],
  dplyr[`%>%`]
)

# REBL Histogram Module

ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$uiOutput(ns('rebl_hist_page'))
}

server <- function(id, person_fit_data, rval_model) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rval_rebl_hist <- shiny$reactive({
      shiny$req(person_fit_data())
      person_fit_data() %>%
        ggplot(aes(x = rebl_score)) +
        geom_histogram(
          binwidth = 0.3,
          fill = 'grey',
          color = 'black'
        ) +
        theme_classic() +
        labs(
          x = 'REBL Score',
          y = 'Count'
        ) +
        scale_x_continuous(breaks = seq(-5, 3, 1)) +
        theme(
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 18)
        )
    })

    output$rebl_hist_title <- shiny$renderText({
      shiny$HTML('<h3 class="body-header-3">REBL Score Histogram</h3>')
    })

    output$rebl_hist <- shiny$renderPlot({
      shiny$req(rval_rebl_hist())
      rval_rebl_hist()
    })

    output$rebl_hist_exp <- shiny$renderText({
      shiny$HTML(
        '<p>This histogram shows REBL Scores for your sample. If you chose to
        rescale your scores based on our baseline model, the rescaled scores will
        be shown here. Note that the CML estimation method used in the eRm package
        does not assume assume a perfectly normal distribution of scores, so it is
        okay if the scores are not centered on zero.</p><br><br><br>'
      )
    })

    output$rebl_hist_page <- shiny$renderUI({
      shiny$req(rval_model())
      shiny$tagList(shiny$fluidRow(
        shiny$column(12, shiny$uiOutput(ns('rebl_hist_title'))),
        shiny$column(
          12,
          shiny$div(
            style = 'display: flex; justify-content: center;',
            shiny$plotOutput(ns('rebl_hist'), width = '750px')
          )
        ),
        shiny$column(12, shiny$uiOutput(ns('rebl_hist_exp')))
      ))
    })

    return(rval_rebl_hist)
  })
}
