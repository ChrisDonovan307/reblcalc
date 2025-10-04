box::use(
  shiny,
  eRm[plotjointICC],
  grDevices[png, dev.off]
)

box::use(
  app / logic / show_placeholder[show_placeholder],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$uiOutput(ns("icc_plot"))
}

#' @export
server <- function(id, rval_model, analysis_state) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rval_icc_plot <- shiny$reactive({
      # A temp file to save the output. It will be deleted after renderImage
      # sends it, because deleteFile=TRUE.
      outfile <- tempfile(fileext = ".png")

      png(
        outfile,
        width = 6,
        height = 6,
        units = "in",
        res = 100
      )
      plotjointICC(
        rval_model(),
        legend = TRUE,
        legpos = "bottomright",
        lwd = 2,
        main = "Joint ICC Plot",
        xlab = "REBL Score",
        ylab = "Probability to Perform REBL Item",
        xlim = c(-5, 6)
      )
      dev.off()

      # Return a list for some reason
      list(
        src = outfile,
        alt = "This is alternate text"
      )
    })

    output$icc_plot <- shiny$renderUI({
      if (!analysis_state()) {
        return(show_placeholder())
      }

      shiny$req(rval_model())
      shiny$tagList(
        shiny$fluidRow(
          shiny$column(12, shiny$uiOutput(ns("icc_title"))),
          shiny$div(
            style = "width: 100%; text-align: center;",
            shiny$plotOutput(ns("icc"), width = "80%", height = "100%")
          ),
          shiny$column(12, shiny$uiOutput(ns("icc_exp")))
        )
      )
    })

    output$icc_title <- shiny$renderUI({
      shiny$HTML(
        '<h3 class="body-header-3">Item Characteristic Curves</h3>'
      )
    })

    output$icc <- shiny$renderImage(
      {
        rval_icc_plot()
      },
      deleteFile = FALSE
    )

    # Explanation
    output$icc_exp <- shiny$renderUI({
      shiny$HTML(
        "This plot shows the Item Characteristic Curves of all the REBL items.
        A person of a given REBL Score (x-axis) is estimated to have a certain
        probability of performing each PEB (y-axis).
        <br><br><br>"
      )
    })

    return(rval_icc_plot)
  })
}
