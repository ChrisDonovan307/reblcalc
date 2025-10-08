box::use(
  shiny,
  markdown
)

# Landing Page

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$uiOutput(ns("landing"))
}

#' @export
server <- function(id, analysis_state) {
  shiny$moduleServer(id, function(input, output, session) {
    output$landing <- shiny$renderUI({
      shiny$req(!analysis_state())
      md_file <- system.file("view", "landing.md", package = ".", mustWork = FALSE)
      if (md_file == "") {
        md_file <- "app/view/landing.md"
      }
      shiny$HTML(markdown$markdownToHTML(md_file, fragment.only = TRUE))
    })
  })
}
