# REBL Histogram Module

rebl_hist_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns('rebl_hist_page'))
}

rebl_hist_server <- function(id, person_fit_data, rval_model) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rval_rebl_hist <- reactive({

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
        scale_x_continuous(breaks = seq(-5, 3, 1))
    })

    output$hist_title <- renderText({
      HTML('<h3 style="color: #2F4F4F; font-weight: bold;">REBL Score Histogram</h3>')
    })

    output$rebl_hist <- renderPlot({
      rval_rebl_hist() +
        theme(
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 18)
        )
    })

    output$rebl_hist_exp <- renderText({
      HTML(
        '<p>This histogram shows REBL Scores for your sample. If you chose to
        rescale your scores based on our baseline model, the rescaled scores will
        be shown here. Note that the CML estimation method used in the eRm package
        does not assume assume a perfectly normal distribution of scores, so it is
        okay if the scores are not centered on zero.</p><br><br><br>'
      )
    })

    output$rebl_hist_page <- renderUI({
      req(rval_model())
      tagList(fluidRow(
        column(12, uiOutput(ns('hist_title'))),
        column(
          12,
          div(
            style = 'display: flex; justify-content: center;',
            plotOutput(ns('rebl_hist'), width = '750px')
          )
        ),
        column(12, uiOutput(ns('hist_exp')))
      ))
    })

    return(rval_rebl_hist)
  })
}
