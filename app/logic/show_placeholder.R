box::use(
  shiny[tagList, tags, div, p, validate, need],
  htmltools[HTML],
)

#' Show placeholder message when analysis hasn't been run
#'
#' @param condition Logical value indicating whether analysis has been run
#' @param message Character string for the placeholder message
#'
#' @export
show_placeholder <- function(message = "Choose a dataset and run the analysis before viewing results here.") {
  tagList(
    tags$br(),
    tags$p(message)
  )
}
