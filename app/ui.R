# UI

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

  includeCSS("www/styles.css"),
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

