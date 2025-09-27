# Module Skim Page

skim_page_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("skim_page"))
}

skim_page_server <- function(id, rval_df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Skim page ----
    output$skim_page <- renderUI({
      req(rval_df(), rval_skim_df())

      # Check if there is anything to show
      has_numeric <- any(rval_skim_df()$skim_type == "numeric")
      has_factors <- any(rval_skim_df()$skim_type == "factor")

      # If nothing, return NULL (nothing rendered)
      if (!has_numeric && !has_factors) return(NULL)

      tagList(
        fluidRow(
          column(12, uiOutput(ns('skim_title')))
        ),

        # Numeric table
        if (has_numeric) {
          fluidRow(
            column(12, renderUI(HTML('<h4 class="body-header-4">Numeric</h4>'))),
            column(12, reactableOutput(ns('skim_numeric')))
          )
        },

        # Factor table
        if (has_factors) {
          fluidRow(
            column(12, renderUI(HTML('<h4 class="body-header-4">Factors</h4>'))),
            column(12, reactableOutput(ns('skim_factor')))
          )
        }
      )
    })

    # Title ----
    output$skim_title <- renderUI({
      req(rval_df())
      HTML(
        '<h3 class="body-header-3">Skim</h3>
        <p>Explore summary statistics and missing values from your dataset.</p>'
      )
    })

    # Skim df ----
    rval_skim_df <- reactive({
      req(rval_df())
      rval_df() %>%
        skimr::skim() %>%
        as.data.frame()
    })

    # Skim numeric ----
    output$skim_numeric <- renderReactable({
      req(rval_df(), rval_skim_df())
      rval_skim_df() %>%
        dplyr::filter(skim_type == 'numeric') %>%
        dplyr::select(-c(starts_with('factor'), skim_type)) %>%
        setNames(c(
          names(.) %>%
            str_remove('numeric\\.') %>%
            str_remove('skim_') %>%
            str_remove('ing$') %>%
            str_remove('^complete_')
        )) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))) %>%
        get_reactable(
          defaultColDef = colDef(minWidth = 50),
          columns = list(
            variable = colDef(minWidth = 150),
            hist = colDef(minWidth = 75)
          )
        )
    })

    # Skim factors ----
    output$skim_factor <- renderReactable({
      req(rval_skim_df())
      df <- rval_skim_df() %>%
        dplyr::filter(skim_type == 'factor')

      if (nrow(df) == 0) {
        return(NULL)
      } else {
        df %>%
          dplyr::filter(skim_type == 'factor') %>%
          dplyr::select(-c(starts_with('numeric'), skim_type)) %>%
          setNames(c(
            names(.) %>%
              str_remove('factor\\.') %>%
              str_remove('skim_') %>%
              str_remove('ing$') %>%
              str_remove('^complete_')
          )) %>%
          dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))) %>%
          get_reactable(
            defaultColDef = colDef(minWidth = 50),
            columns = list(
              variable = colDef(minWidth = 150),
              hist = colDef(minWidth = 75)
            )
          )

      }
    })

  })
}
