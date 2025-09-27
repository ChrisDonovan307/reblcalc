# Imputation Module for REBL Score Calculator

# UI for imputation page
imputation_page_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("imputation_page"))
}

# Server logic for imputation page
imputation_page_server <- function(id,
                                   import_values,
                                   analysis_state,
                                   impute_option,
                                   run_analysis) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Impute data if option is selected
    rval_imp_out <- eventReactive(run_analysis(), {

      # First reorder the DF to put respondent id first, then rebl items in order
      uid <- import_values$respondent_id()
      df <- import_values$rval_df() %>%
        select(all_of(c(uid, rebl_items)))

      if (impute_option() == FALSE) {
        return(df)
      } else {
        if (sum(is.na(select(import_values$rval_df(), all_of(rebl_items)))) == 0) {
          showModal(modalDialog(
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
              select(all_of(rebl_items)) %>%
              mutate(across(everything(), as.factor)) %>%
              as.data.frame() %>%
              missForest(variablewise = TRUE)
          },
          type = 6,
          color = '#2F4F4F',
          caption = HTML(
            'Imputing data. Note that this can take a minute<br>
            if there are lots of missing data.'
          ))

          return(imp_out)
        }
      }
    })

    rval_df_clean <- eventReactive(run_analysis(), {
      if (impute_option() == FALSE) {
        rval_imp_out()
      } else if (impute_option() == TRUE) {
        rval_imp_out()$ximp %>%
          mutate(across(everything(), ~ as.numeric(.) - 1)) %>%
          bind_cols(import_values$rval_df() %>% select(-all_of(rebl_items)))
      }
    })

    # Also save imputation error stats every time imputation happens
    rval_imp_oob <- eventReactive(run_analysis(), {
      req(rval_imp_out())
      rval_imp_out()$OOB
    })

    # tabPanel for imputation
    output$imputation_page <- renderUI({
      req(analysis_state()) # Only show when analysis has been run
      tagList(
        fluidRow(
          column(12, uiOutput(ns('imp_title'))),
          column(12, uiOutput(ns('imp_exp'))),
          column(12, tableOutput(ns('oob_print'))),
          column(12, uiOutput(ns('no_imp')))
        )
      )
    })

    output$imp_title <- renderUI({
      HTML(
        '<h3 style="color: #2F4F4F; font-weight: bold;">Imputation</h3>'
      )
    })

    output$no_imp <- renderUI({
      if (impute_option() == FALSE && analysis_state() == TRUE)
        HTML(
          '<p>No imputation performed.</p>'
        )
    })

    # Add imputation output, only if performed though
    output$imp_exp <- renderText({
      if (impute_option() == TRUE && analysis_state() == TRUE) {
        HTML(
          '<p>Out-of-bag (OOB) errors are an estimate of the imputation
          error from the missForest algorithm. For categorical data, they
          represent the proportion of falsely classified values (PFC). A value of
          0 would represent perfect prediction.</p>'
        )
      }
    })

    output$oob_print <- renderTable({
      if (impute_option() == TRUE && analysis_state() == TRUE) {
        req(rval_imp_out())
        df <- data.frame(
          names(rval_imp_out()$ximp),
          round(rval_imp_oob(), 3)
        ) %>%
          setNames(c('REBL Item', 'PFC')) %>%
          arrange(PFC)
        df
      }
    })

    # Return reactive values
    return(list(
      rval_df_clean = rval_df_clean,
      rval_imp_out = rval_imp_out,
      rval_imp_oob = rval_imp_oob
    ))

  })
}
