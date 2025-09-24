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

# pcar_obj <- reactive({
#   req(rval_model())
#   showPageSpinner(
#     rval_model() %>%
#       test_uni_pcar(),
#     type = 6,
#     color = '#2F4F4F',
#     caption = HTML(
#       'Calculating test of unidimensionality...'
#     )
#   )
# })
