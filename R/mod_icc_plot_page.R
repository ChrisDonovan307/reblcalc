# ICC Plot Page

icc_plot_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns('icc_plot'))
}

icc_plot_server <- function(id, rval_model) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rval_icc_plot <- reactive({

      # A temp file to save the output. It will be deleted after renderImage
      # sends it, because deleteFile=TRUE.
      outfile <- tempfile(fileext = '.png')

      png(
        outfile,
        width = 6,
        height = 6,
        units = 'in',
        res = 100
      )
      plotjointICC(
        rval_model(),
        legend = TRUE,
        legpos = 'bottomright',
        lwd = 2,
        main = 'Joint ICC Plot',
        xlab = 'REBL Score',
        ylab = 'Probability to Perform REBL Item',
        xlim = c(-5, 6)
      )
      dev.off()

      # Return a list for some reason
      list(
        src = outfile,
        alt = "This is alternate text"
      )
    })

    output$icc_plot <- renderUI({
      req(rval_model())
      tagList(
        fluidRow(
          column(12, uiOutput(ns('icc_title'))),
          div(
            style = "width: 100%; text-align: center;",
            plotOutput(ns('icc'), width = '80%', height = '100%')
          ),
          column(12, uiOutput(ns('icc_exp')))
        )
      )
    })

    output$icc_title <- renderUI({
      HTML(
      '<h3 style="color: #2F4F4F; font-weight: bold;">Item Characteristic Curves</h3>'
      )
    })

    output$icc <- renderImage({
      rval_icc_plot()
    }, deleteFile = FALSE)

    # Explanation
    output$icc_exp <- renderUI({
      HTML(
        'This plot shows the Item Characteristic Curves of all the REBL items.
        A person of a given REBL Score (x-axis) is estimated to have a certain
        probability of performing each PEB (y-axis).
        <br><br><br>'
      )
    })

    return(rval_icc_plot)

  })
}

