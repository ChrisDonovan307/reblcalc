box::use(
  shiny[NS, moduleServer, eventReactive, req, HTML],
  dplyr[select, all_of],
  eRm[RM],
  shinycssloaders[showPageSpinner]
)

# Load REBL items data
load('app/data/rebl_items.rda')

# Rasch Module

rasch_ui <- function(id) {
  ns <- NS(id)
}

rasch_server <- function(id,
                         run_analysis,
                         imp_values,
                         import_values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Run Rasch model
    rval_model <- eventReactive(run_analysis(), {
      req(imp_values$rval_df_clean())

      # Check for unique respondent ID column
      if (any(duplicated(import_values$rval_df()[[import_values$respondent_id()]]))) {
        stop(
          'The respondent ID column that you have selected does not uniquely',
          ' identify respondents. Please choose another column.'
        )
      } else {

        # Run Rasch model
        showPageSpinner({
          imp_values$rval_df_clean() %>%
            select(all_of(rebl_items)) %>%
            select(order(colnames(.))) %>%
            RM()
        },
        type = 6,
        color = '#2F4F4F',
        caption = HTML(
          'Running Rasch model. Note that this can take a minute<br>or two if there
          are lots of missing data.'
        ))
      }
    })

    return(rval_model)

  })
}
