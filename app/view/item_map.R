box::use(
  shiny[NS, uiOutput, moduleServer, reactive, renderUI, req, tagList, fluidRow, column, div, HTML, renderImage, plotOutput],
  eRm[plotPWmap],
  grDevices[png, dev.off]
)

# Mod Item Map

ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns('item_map'))
}

server <- function(id, rval_model) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rval_item_map <- reactive({

      # A temp file to save the output. It will be deleted after renderImage
      # sends it, because deleteFile=TRUE.
      outfile <- tempfile(fileext = '.png')

      png(
        outfile,
        width = 8,
        height = 8,
        units = 'in',
        # pointsize = 16,
        res = 72
      )
      plotPWmap(
        rval_model(),
        imap = TRUE,
        latdim = 'Item Difficulty',
        tlab = 'Infit t Statistic',
        cex.gen = 1.1,
        cex.pch = 1.1
      )
      dev.off()

      # Return a list for some reason
      list(src = outfile)
    })

    output$item_map_title <- renderUI({
      HTML('<h3 class="body-header-3">Item Map</h3>')
    })

    output$item_map_plot <- renderImage({
      rval_item_map()
    }, deleteFile = FALSE)

    output$item_map_exp <- renderUI({
      HTML(
        '<p>The Item Map shows the item difficulties (y-axis) against item fit
        (x-axis). Infit is the information-weighted fit statistic, which weights
        nearby respondents more than those farther away on the latent dimension.
        The vertical green lines are at +/- 2 standard deviations from the mean.
        Items outside of these green lines are poorly fitting.</p><br><br><br>'
      )
    })

    output$item_map <- renderUI({
      req(rval_model())
      tagList(
        fluidRow(
          column(12, uiOutput(ns('item_map_title'))),
          div(
            style = "width: 100%; text-align: center;",
            plotOutput(ns('item_map_plot'), width = '80%', height = '100%'),
          ),
          column(12, uiOutput(ns('item_map_exp')))
        )
      )
    })

    return(rval_item_map)

  })
}


