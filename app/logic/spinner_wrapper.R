box::use(shinycssloaders[showPageSpinner])

spinner_wrapper <- function(df = rval_df,
                                 caption = NULL,
                                 type = 6,
                                 color = '#2F4F4F',
                                 ...) {
  showPageSpinner(
    df,
    caption = caption,
    type = type,
    color = color,
    ...
  )
}
