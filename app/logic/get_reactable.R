box::use(
  assertthat[assert_that],
  reactable[reactable],
)

#' Create a customized reactable table
#'
#' This function creates a reactable table with predefined styling and
#' functionality options. It serves as a wrapper around the reactable function
#' with common default settings for interactive data tables.
#'
#' @param df A data frame to display in the reactable table.
#' @param sortable Logical indicating whether columns should be sortable.
#'   Default is TRUE.
#' @param resizable Logical indicating whether columns should be resizable.
#'   Default is TRUE.
#' @param filterable Logical indicating whether columns should be filterable.
#'   Default is TRUE.
#' @param searchable Logical indicating whether the table should have a global
#'   search box. Default is TRUE.
#' @param pagination Logical indicating whether to enable pagination.
#'   Default is TRUE.
#' @param bordered Logical indicating whether to show table borders.
#'   Default is TRUE.
#' @param wrap Logical indicating whether to wrap text in cells.
#'   Default is TRUE.
#' @param rownames Logical indicating whether to show row names.
#'   Default is FALSE.
#' @param onClick Character string specifying click behavior. Options include
#'   'expand', 'select', or NULL. Default is 'select'.
#' @param striped Logical indicating whether to show striped rows.
#'   Default is TRUE.
#' @param defaultPageSize Numeric specifying the default number of rows per page.
#'   Default is 10.
#' @param showPageSizeOptions Logical indicating whether to show page size options.
#'   Default is TRUE.
#' @param highlight Logical indicating whether to highlight rows on hover.
#'   Default is TRUE.
#' @param style List of CSS styles to apply to the table. Default is
#'   list(fontSize = "14px").
#' @param compact Logical indicating whether to use compact styling.
#'   Default is TRUE.
#' @param fullWidth Logical indicating whether the table should take full width.
#'   Default is TRUE.
#' @param ... Additional arguments passed to reactable().
#'
#' @return A reactable htmlwidget object that can be displayed in HTML output.
#'
#' @details This function provides a convenient way to create consistently
#'   styled reactable tables across an application. It sets up common
#'   interactive features like sorting, filtering, searching, and pagination
#'   with sensible defaults.
#'
#' @importFrom reactable reactable
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' get_reactable(mtcars)
#'
#' # Customize options
#' get_reactable(
#'   iris,
#'   defaultPageSize = 5,
#'   searchable = FALSE,
#'   striped = FALSE
#' )
#'
#' # With additional reactable arguments
#' get_reactable(
#'   df,
#'   columns = list(
#'     name = colDef(name = "Full Name"),
#'     age = colDef(name = "Age (years)")
#'   )
#' )
#' }
#'
get_reactable <- function(df,
                          sortable = TRUE,
                          resizable = TRUE,
                          filterable = TRUE,
                          searchable = FALSE,
                          pagination = TRUE,
                          bordered = TRUE,
                          wrap = TRUE,
                          rownames = FALSE,
                          onClick = "select",
                          striped = TRUE,
                          defaultPageSize = 10,
                          showPageSizeOptions = TRUE,
                          highlight = TRUE,
                          style = list(fontSize = "16px"),
                          compact = TRUE,
                          fullWidth = TRUE,
                          ...) {
  assert_that("data.frame" %in% class(df), msg = "The df object must be a data.frame")
  assert_that(nrow(df) > 0, msg = "The df must have at least 1 row")

  reactable::reactable(
    df,
    sortable = sortable,
    resizable = resizable,
    filterable = filterable,
    searchable = searchable,
    pagination = pagination,
    bordered = bordered,
    wrap = wrap,
    rownames = rownames,
    onClick = onClick,
    striped = striped,
    defaultPageSize = defaultPageSize,
    showPageSizeOptions = showPageSizeOptions,
    highlight = highlight,
    style = style,
    compact = compact,
    fullWidth = fullWidth,
    ...
  )
}
