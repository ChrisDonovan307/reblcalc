box::use(
  shiny,
  skimr[skim],
  dplyr[filter, select, mutate, any_of, `%>%`, starts_with],
  reactable[renderReactable, colDef, reactableOutput],
  stringr[str_remove],
  stats[setNames],
  htmltools[tags, tagList],
)

box::use(
  app / logic / get_reactable[get_reactable],
  app / logic / show_placeholder[show_placeholder],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$uiOutput(ns("skim"))
}

#' @export
server <- function(id, rval_df) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Skim page ----
    output$skim <- shiny$renderUI({
      shiny$req(rval_df(), rval_skim_df())

      # Check if there is anything to show
      has_numeric <- any(rval_skim_df()$skim_type == "numeric")
      has_factors <- any(rval_skim_df()$skim_type == "factor")

      # If nothing, return NULL (nothing rendered)
      if (!has_numeric && !has_factors) {
        return(NULL)
      }

      shiny$tagList(
        shiny$fluidRow(
          shiny$column(12, shiny$uiOutput(ns("skim_title")))
        ),

        # Numeric table
        if (has_numeric) {
          shiny$fluidRow(
            shiny$column(12, shiny$renderUI(shiny$HTML('<h4 class="body-header-4">Numeric</h4>'))),
            shiny$column(12, reactableOutput(ns("skim_numeric")))
          )
        },

        # Factor table
        if (has_factors) {
          shiny$fluidRow(
            shiny$column(12, shiny$renderUI(shiny$HTML('<h4 class="body-header-4">Factors</h4>'))),
            shiny$column(12, reactableOutput(ns("skim_factor")))
          )
        }
      )
    })

    # Title ----
    output$skim_title <- shiny$renderUI({
      shiny$req(rval_df())
      shiny$HTML(
        '<h3 class="body-header-3">Skim</h3>
        <p>Explore summary statistics and missing values from your dataset.</p>'
      )
    })

    # Skim df ----
    rval_skim_df <- shiny$reactive({
      shiny$req(rval_df())
      rval_df() %>%
        skimr::skim() %>%
        as.data.frame()
    })

    # Skim numeric ----
    output$skim_numeric <- renderReactable({
      shiny$req(rval_df(), rval_skim_df())
      rval_skim_df() %>%
        dplyr::filter(skim_type == "numeric") %>%
        dplyr::select(-c(starts_with("factor"), skim_type)) %>%
        setNames(c(
          names(.) %>%
            str_remove("numeric\\.") %>%
            str_remove("skim_") %>%
            str_remove("ing$") %>%
            str_remove("^complete_")
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
      shiny$req(rval_skim_df())
      df <- rval_skim_df() %>%
        dplyr::filter(skim_type == "factor")

      if (nrow(df) == 0) {
        return(NULL)
      } else {
        df %>%
          dplyr::filter(skim_type == "factor") %>%
          dplyr::select(-c(starts_with("numeric"), skim_type)) %>%
          setNames(c(
            names(.) %>%
              str_remove("factor\\.") %>%
              str_remove("skim_") %>%
              str_remove("ing$") %>%
              str_remove("^complete_")
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
