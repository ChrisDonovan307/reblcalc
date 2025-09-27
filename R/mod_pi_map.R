# Mod PI Map

pi_map_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns('pi_map_page'))
}

pi_map_server <- function(id, rval_model) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rval_pi_map <- reactive({

      # A temp file to save the output. It will be deleted after renderImage
      # sends it, because deleteFile=TRUE.
      outfile <- tempfile(fileext = '.png')

      png(
        outfile,
        width = 8,
        height = 6,
        units = 'in',
        res = 100
      )
      plotPImap2(
        rval_model(),
        sorted = TRUE,
        main = NA_character_,
        latdim = 'Latent Trait',
        pplabel = 'Latent Trait\nDistribution',
        cex.gen = 0.8,
        margins = c(3, 11, 0, 1)
      )
      dev.off()

      # Return a list for some reason
      list(
        src = outfile,
        alt = "This is alternate text"
      )
    })

    # PI Map Title
    output$pi_map_title <- renderText({
      HTML('<h3 style="color: #2F4F4F; font-weight: bold;">Person Item Map</h3>')
    })

    # PI Map image for tab output
    output$pi_map <- renderImage({
      rval_pi_map()
    }, deleteFile = FALSE)

    # Explanation
    output$pi_map_exp <- renderUI({
      HTML(
        'The lower plot of the Person Item Map shows the REBL items and item
        difficulties in order of easiest to hardest. THe distribution of REBL
        scores is shown in the histogram on top. This is a way to see how well
        the item difficulties match the person abilities.
        <br><br><br>'
      )
    })

    output$pi_map_page <- renderUI({
      req(rval_model())
      tagList(
        fluidRow(
          column(12, uiOutput(ns('pi_map_title'))),
          div(
            style = "width: 100%; text-align: center;",
            plotOutput(ns('pi_map'), width = '80%', height = '100%')
          ),
          column(12, uiOutput(ns('pi_map_exp')))
        )
      )
    })

    return(rval_pi_map)

  })
}
