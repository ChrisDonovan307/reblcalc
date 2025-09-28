suppressPackageStartupMessages(if (!requireNamespace('pacman')) {
  install.packages('pacman', dependencies = TRUE)
})

pacman::p_load(
  shiny,
  dplyr,
  eRm,
  shinyWidgets,
  shinythemes,
  readr,
  readxl,
  ggplot2,
  tibble,
  purrr,
  stringr,
  bslib,
  shinyjs,
  zip,
  missForest,
  shinycssloaders,
  psych,
  skimr,
  conflicted,
  reactable
)

suppressMessages(
  conflicts_prefer(
    eRm::personfit,
    eRm::itemfit,
    dplyr::select,
    dplyr::filter,
    .quiet = TRUE
  )
)

# set spinner options
options(
  spinner.type = 6,
  spinner.color = '#2f4f4f',
  spinner.proxy.height = '100px',
  spinner.hide.ui = TRUE
)

devtools::load_all()


# ui ----------------------------------------------------------------------


ui <- fluidPage(

  theme = bslib::bs_theme(
    preset = 'lumen',
    info = '#2f4f4f',
    primary = '#2f4f4f',
    font_scale = 1
  ),

  # includeCSS("../www/styles.css"),
  useShinyjs(),

  ## Header image ----
  fluidRow(
    column(12, div(
      style = "text-align: center; padding: 4px;",
      img(src = "forest3.jpg", width = '100%')
    ))
  ),

  sidebarLayout(

    ## Sidebar panel -----------------------------------------------------------
    sidebarPanel(
      width = 3,
      HTML("<h2 class='body-header-1'>REBL Score Calculator</h2>"),
      buttons_ui('buttons'),
      download_ui('download')
    ),


    ## Main Panel --------------------------------------------------------------

    mainPanel(
      width = 9,

      tabsetPanel(
        id = 'tabset',
        tabPanel(
          'Info',
          fluidRow(
            landing_page_ui('landing_page'),
            validation_ui('validation')
          )
        ),
        tabPanel('REBL Items', withSpinner(rebl_items_page_ui('rebl_items_page'))),
        tabPanel('Skim', withSpinner(skim_page_ui('skim_page'))),
        tabPanel('Imputation', withSpinner(imputation_page_ui('imputation_page'))),
        tabPanel('Item Map', withSpinner(item_map_ui('item_map'))),
        tabPanel('ICC Plot', withSpinner(icc_plot_ui('icc_plot_page'))),
        tabPanel('PI Map', withSpinner(pi_map_ui('pi_map'))),
        tabPanel('Histogram', withSpinner(rebl_hist_ui('rebl_hist'))),
        tabPanel('Person Fit', withSpinner(person_fit_ui('person_fit'))),
        tabPanel('Item Fit', withSpinner(item_fit_ui('item_fit')))
      )
    )
  )
)


# Server ------------------------------------------------------------------

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
