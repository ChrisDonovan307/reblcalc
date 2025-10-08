box::use(
  shiny,
  bslib[bs_theme],
  shinyjs[useShinyjs],
  missRanger[missRanger],
  shinycssloaders[withSpinner]
)

box::use(
  app / view / buttons,
  app / view / landing,
  app / view / rebl_items,
  app / view / skim,
  app / view / imputation,
  app / view / rasch,
  app / view / linking,
  app / view / validation,
  app / view / person_fit,
  app / view / item_fit,
  app / view / item_map,
  app / view / icc_plot,
  app / view / pi_map,
  app / view / rebl_hist,
  app / view / download,
  app / logic / show_placeholder[show_placeholder],
)

# set spinner options
options(
  spinner.type = 6,
  spinner.color = "#2f4f4f",
  spinner.proxy.height = "100px",
  spinner.hide.ui = TRUE
)


# ui ----------------------------------------------------------------------

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$fluidPage(
    theme = bs_theme(
      preset = "lumen",
      info = "#2f4f4f",
      primary = "#2f4f4f",
      font_scale = 1
    ),
    shiny$includeCSS("app/styles/styles.css"),
    useShinyjs(),

    ## Header image ----
    shiny$fluidRow(
      shiny$column(12, shiny$div(
        style = "text-align: center; padding: 4px;",
        shiny$img(src = "static/images/forest3.jpg", width = "100%")
      ))
    ),
    shiny$sidebarLayout(

      ## Sidebar panel -----------------------------------------------------------
      shiny$sidebarPanel(
        width = 3,
        shiny$HTML("<h1 class='body-header-1'>REBL Score Calculator</h1>"),
        buttons$ui("buttons"),
        download$ui("download")
      ),


      ## Main Panel --------------------------------------------------------------

      shiny$mainPanel(
        width = 9,
        shiny$tabsetPanel(
          id = "tabset",
          shiny$tabPanel(
            "Info",
            shiny$fluidRow(
              landing$ui("landing"),
              validation$ui("validation")
            )
          ),
          shiny$tabPanel("REBL Items", withSpinner(rebl_items$ui("rebl_items"))),
          shiny$tabPanel("Skim", withSpinner(skim$ui("skim"))),

          # shiny$observeEvent(import_values$analysis_state(), {
          #   if (import_values$analysis_state()) {
          #     shiny$insertTab(
          #       'tabset',
          #       shiny$tabPanel('Imputation'),
          #       target = 'Skim',
          #       positino = 'after'
          #     )
          #   }
          # }),

          shiny$tabPanel("Imputation", withSpinner(imputation$ui("imputation"))),
          shiny$tabPanel("Item Map", withSpinner(item_map$ui("item_map"))),
          shiny$tabPanel("ICC Plot", withSpinner(icc_plot$ui("icc_plot"))),
          shiny$tabPanel("PI Map", withSpinner(pi_map$ui("pi_map"))),
          shiny$tabPanel("Histogram", withSpinner(rebl_hist$ui("rebl_hist"))),
          shiny$tabPanel("Person Fit", withSpinner(person_fit$ui("person_fit"))),
          shiny$tabPanel("Item Fit", withSpinner(item_fit$ui("item_fit")))
        )
      )
    )
  )
}


# Server ------------------------------------------------------------------

#' @export
server <- function(input, output, session) {
  ## analysis_state ----
  # Set state to false. when true, download button appears, and landing page
  # becomes model test page
  analysis_state <- shiny$reactiveVal(FALSE)
  button_values <- buttons$server("buttons")
  import_values <- button_values$import_values
  shiny$observeEvent(button_values$run_analysis(), {
    analysis_state(TRUE)
  })

  ## Server calls ----
  landing$server("landing", analysis_state)
  rebl_items$server("rebl_items")
  skim$server("skim", import_values$rval_df)
  imp_values <- imputation$server(
    id = "imputation",
    import_values = import_values,
    analysis_state = analysis_state,
    impute_option = button_values$impute_option,
    run_analysis = button_values$run_analysis
  )
  rval_model <- rasch$server(
    "rasch",
    run_analysis = button_values$run_analysis,
    imp_values,
    import_values
  )
  rval_rescaled_scores <- linking$server(
    "linking",
    button_values,
    rval_model
  )
  valid_values <- validation$server(
    "validation",
    rval_model = rval_model,
    analysis_state = analysis_state
  )
  person_fit_data <- person_fit$server(
    "person_fit",
    button_values$link_option,
    import_values$respondent_id,
    imp_values$rval_df_clean,
    rval_model,
    analysis_state,
    rval_rescaled_scores
  )
  item_fit_data <- item_fit$server(
    "item_fit",
    rval_model,
    imp_values$rval_df_clean,
    analysis_state
  )
  rval_item_map <- item_map$server(
    "item_map",
    rval_model,
    analysis_state
  )
  rval_icc_plot <- icc_plot$server(
    "icc_plot",
    rval_model,
    analysis_state
  )
  rval_pi_map <- pi_map$server(
    "pi_map",
    rval_model,
    analysis_state
  )
  rval_rebl_hist <- rebl_hist$server(
    "rebl_hist",
    person_fit_data,
    rval_model,
    analysis_state
  )

  ## Reset analysis_state ----
  shiny$observeEvent(button_values$impute_option() | button_values$link_option(),
    {
      analysis_state(FALSE)
    },
    ignoreNULL = FALSE
  )

  ## download_server ----
  download$server(
    "download",
    rval_model,
    person_fit_data,
    item_fit_data,
    analysis_state,
    rval_item_map,
    rval_icc_plot,
    rval_pi_map,
    imp_values$rval_imp_out,
    import_values$rval_df,
    rval_rebl_hist,
    valid_values$gof_obj,
    valid_values$lr_obj,
    valid_values$pcar_obj,
    import_values,
    impute_option = button_values$impute_option
  )
}
