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

      # Janky workaround to check for existence of rval_df()
      df <- tryCatch(
        {
          rval_df()
        },
        error = function(e) NULL
      )

      # Show placeholder message if no rval_df()
      if (is.null(df)) {
        return(show_placeholder(
          'Upload a dataset or use the example to explore summary statistics.'
        ))
      }

      # Try to get skim results
      skim_df <- tryCatch(
        {
          rval_skim_df()
        },
        error = function(e) NULL
      )

      if (is.null(skim_df) || nrow(skim_df) == 0) {
        return(show_placeholder())
      }

      # Check if there is anything to show
      has_numeric <- any(skim_df$skim_type == "numeric")
      has_factors <- any(skim_df$skim_type == "factor")

      # If nothing, return NULL (nothing rendered)
      if (!has_numeric && !has_factors) {
        return(show_placeholder())
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
      shiny$req(rval_df())
      skim_df <- rval_skim_df()
      shiny$req(skim_df)
      skim_df %>%
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
      skim_df <- rval_skim_df()
      shiny$req(skim_df)
      df <- skim_df %>%
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
