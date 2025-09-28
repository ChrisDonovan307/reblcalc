# Server

server <- function(input, output, session) {

  ## analysis_state ----
  # Set state to false. when true, download button appears, and landing page
  # becomes model test page
  analysis_state <- reactiveVal(FALSE)
  button_values <- buttons_server('buttons')
  import_values <- button_values$import_values
  observeEvent(button_values$run_analysis(), {
    analysis_state(TRUE)
  })

  ## Server calls ----
  landing_page_server('landing_page', analysis_state)
  rebl_items_page_server('rebl_items_page')
  skim_page_server('skim_page', import_values$rval_df)
  imp_values <- imputation_page_server(
    id = 'imputation_page',
    import_values = import_values,
    analysis_state = analysis_state,
    impute_option = button_values$impute_option,
    run_analysis = button_values$run_analysis
  )
  rval_model <- rasch_server(
    'rasch',
    run_analysis = button_values$run_analysis,
    imp_values,
    import_values
  )
  rval_rescaled_scores <- linking_server(
    'linking',
    button_values,
    rval_model
  )
  valid_values <- validation_server(
    'validation',
    rval_model = rval_model,
    analysis_state = analysis_state
  )
  person_fit_data <- person_fit_server(
    'person_fit',
    button_values$link_option,
    import_values$respondent_id,
    imp_values$rval_df_clean,
    rval_model,
    analysis_state,
    rval_rescaled_scores
  )
  item_fit_data <- item_fit_server(
    'item_fit',
    rval_model,
    imp_values$rval_df_clean,
    analysis_state
  )
  rval_item_map <- item_map_server(
    'item_map',
    rval_model
  )
  rval_icc_plot <- icc_plot_server(
    'icc_plot_page',
    rval_model
  )
  rval_pi_map <- pi_map_server(
    'pi_map',
    rval_model
  )
  rval_rebl_hist <- rebl_hist_server(
    'rebl_hist',
    person_fit_data,
    rval_model
  )

  ## Reset analysis_state ----
  observeEvent(button_values$impute_option() | button_values$link_option(), {
    analysis_state(FALSE)
  }, ignoreNULL = FALSE)

  ## download_server ----
  download_server(
    'download',
    rval_model,
    person_fit_data,
    item_fit_data,
    analysis_state,
    rval_item_map,
    rval_icc_plot,
    rval_pi_map,
    rval_imp_out,
    import_values$rval_df,
    rval_rebl_hist,
    valid_values$gof_obj,
    valid_values$lr_obj,
    valid_values$pcar_obj,
    import_values,
    impute_option = button_values$impute_option
  )

}

shinyApp(ui = ui, server = server)
