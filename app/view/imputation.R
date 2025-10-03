box::use(
  shiny,
  dplyr[select, all_of, mutate, across, bind_cols, arrange, `%>%`],
  shinycssloaders[showPageSpinner],
  missRanger[missRanger],
  tibble[rownames_to_column],
  reactable[renderReactable, colDef, reactableOutput],
  stats[setNames],
)

box::use(
  app/logic/get_reactable[get_reactable],
  app/logic/show_placeholder[show_placeholder],
)

# Load REBL items data
load('app/data/rebl_items.rda')

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$uiOutput(ns('imputation'))
}

#' @export
server <- function(id,
                   import_values,
                   analysis_state,
                   impute_option,
                   run_analysis) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Impute ----
    # Impute data if option is selected
    rval_imp_out <- shiny$eventReactive(run_analysis(), {

      # First reorder the DF to put respondent id first, then rebl items in order
      uid <- import_values$respondent_id()
      df <- import_values$rval_df() %>%
        select(all_of(c(uid, rebl_items)))

      if (impute_option() == FALSE) {
        return(df)
      } else {
        if (sum(is.na(select(import_values$rval_df(), all_of(rebl_items)))) == 0) {
          shiny$showModal(shiny$modalDialog(
            title = 'Error',
            'The "impute missing data" button was checked, but there is no
            missing data in your dataset. Please remove it before continuing. You
            may also explore your dataset in the "Skim" tab.',
            easyClose = TRUE
          ))
          stop()

        } else {

          showPageSpinner({
            imp_out <- df %>%
              dplyr::select(all_of(rebl_items)) %>%
              dplyr::mutate(across(everything(), as.factor)) %>%
              as.data.frame() %>%
              missRanger(
                seed = 42,
                data_only = FALSE
              )
          },
          type = 6,
          color = '#2F4F4F',
          caption = shiny$HTML(
            'Imputing data. Note that this can take a minute<br>
            if there are lots of missing data.'
          ))

          return(imp_out)
        }
      }
    })

    # Clean DF ----
    # If imputed, bind imputed df back to original that has respondent id
    rval_df_clean <- shiny$eventReactive(run_analysis(), {
      if (impute_option() == FALSE) {
        rval_imp_out()
      } else if (impute_option() == TRUE) {
        rval_imp_out()$data %>%
          mutate(across(everything(), ~ as.numeric(.) - 1)) %>%
          bind_cols(import_values$rval_df() %>% select(-all_of(rebl_items)))
      }
    })

    # OOB ----
    # Also save imputation error stats every time imputation happens
    rval_imp_oob <- shiny$eventReactive(run_analysis(), {
      shiny$req(rval_imp_out())
      rval_imp_out()$pred_errors[rval_imp_out()$best_iter, ] %>%
        as.data.frame() %>%
        tibble::rownames_to_column() %>%
        setNames(c('rebl_item', 'oob')) %>%
        mutate(oob = round(oob, 3))
    })

    output$imp_title <- shiny$renderUI({
      shiny$HTML(
        '<h3 class="body-header-3">Imputation</h3>'
      )
    })

    output$no_imp <- shiny$renderUI({
      if (impute_option() == FALSE && analysis_state() == TRUE)
        shiny$HTML(
          '<p>No imputation performed.</p>'
        )
    })

    # Add imputation output, only if performed though
    output$imp_exp <- shiny$renderText({
      if (impute_option() == TRUE && analysis_state() == TRUE) {
        shiny$HTML(
          '<p>Out-of-bag (OOB) errors are an estimate of the imputation
          error from the missForest algorithm. For categorical data, they
          represent the proportion of falsely classified values (PFC). A value of
          0 would represent perfect prediction.</p>'
        )
      }
    })

    output$oob_table <- renderReactable({
      if (impute_option() == TRUE && analysis_state() == TRUE) {
        shiny$req(rval_imp_oob())
        rval_imp_oob() %>%
          arrange(oob) %>%
          get_reactable(
            fullWidth = FALSE,
            columns = list(
              oob = colDef(minWidth = 50)
            )
          )
      }
    })

    # Imputation page ----
    output$imputation <- shiny$renderUI({
      if (!analysis_state()) {
        return(show_placeholder())
      }
      shiny$fluidRow(
        shiny$column(12, shiny$uiOutput(ns('imp_title'))),
        shiny$column(12, shiny$uiOutput(ns('imp_exp'))),
        shiny$column(12, reactableOutput(ns('oob_table'))),
        shiny$column(12, shiny$uiOutput(ns('no_imp')))
      )
    })

    # Return reactive values
    return(list(
      rval_df_clean = rval_df_clean,
      rval_imp_out = rval_imp_out,
      rval_imp_oob = rval_imp_oob
    ))

  })
}
