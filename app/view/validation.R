box::use(
  shiny,
  eRm[person.parameter, gofIRT, LRtest],
  shinycssloaders[showPageSpinner],
  dplyr[`%>%`]
)

box::use(
  app / logic / spinner_wrapper[spinner_wrapper],
  app / logic / test_uni_pcar[test_uni_pcar],
)

# Model Validation Module
#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$uiOutput(ns("validation"))
}

#' @export
server <- function(id, rval_model, analysis_state) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Goodness of fit tests
    gof_obj <- shiny$reactive({
      shiny$req(rval_model())
      rval_model() %>%
        person.parameter() %>%
        gofIRT() %>%
        spinner_wrapper(shiny$HTML("Calculating Goodness of Fit..."))
    })

    # LR test of invariance
    lr_obj <- shiny$reactive({
      shiny$req(rval_model())
      showPageSpinner(
        rval_model() %>%
          LRtest() %>%
          spinner_wrapper(shiny$HTML("Calculating test of invariance..."))
      )
    })

    # PCAR test of unidimensionality
    pcar_obj <- shiny$reactive({
      shiny$req(rval_model())
      rval_model() %>%
        test_uni_pcar() %>%
        spinner_wrapper(shiny$HTML("Calculating test of unidimensionality..."))
    })

    output$model_test_header <- shiny$renderUI({
      shiny$HTML(
        '<h2 class="body-header-2">Results</h2>
        <p>Analysis is complete! On this page, you
          can find fit statistics, the likelihood-ratio test of invariance, as
          well as a test of unidimensionality using principal components
          analysis.</p>'
      )
    })

    output$gof_title <- shiny$renderUI({
      shiny$HTML(
        '<h3 class="body-header-3">Goodness of Fit</h3>'
      )
    })

    output$gof <- shiny$renderPrint({
      gof_obj() %>%
        summary()
    })

    output$gof_exp_lr_title <- shiny$renderUI({
      shiny$HTML(
        '<p>Here we have a slew of goodness of fit statistics for our Rasch model.
        For details on interpretation, see
        <a href="https://escholarship.org/content/qt1m46j62q/qt1m46j62q.pdf">
        Mair et al. (2008).</a></p>
        <h3 class="body-header-3">Invariance</h3>
        '
      )
    })

    # LR Outputs
    output$lr <- shiny$renderPrint({
      lr_obj()
    })

    # LR exp and PCAR title
    output$lr_exp_pcar_title <- shiny$renderUI({
      shiny$HTML(
        '<p>The LR test of measurement invariance splits respondents into two
        groups by median score and tests whether there is a significant change
        in likelihood of the model when item parameters are constrained to
        equality across groups. If the p-value is < 0.05, we reject the null
        hypothesis of measurement invariance. Otherwise, we fail to reject the
        null and retain the null hypothesis of measurement invariance.</p>
        <h3 class="body-header-3">Unidimensionality</h3>
        <p>Here we test that the scale is measuring a single dimension by running
        a Principal Components Analysis (PCA).</p>'
      )
    })

    output$pcar_sum <- shiny$renderPrint({
      pcar_obj() %>%
        summary()
    })

    output$pcar_loadings <- shiny$renderPrint({
      pcar_obj()$Vaccounted
    })

    output$pcar_exp <- shiny$renderUI({
      shiny$HTML(
        "<p>The top output shows the summary from the PCA model. The bottom output
        shows the loadings of the principal components (SS loadings). If the
        highest SS loading of any component is less than 2.0, thet test shows
        demonstrates unidimensionality.</p>"
      )
    })

    # Now put all UI outputs together
    output$validation <- shiny$renderUI({
      shiny$req(analysis_state())

      shiny$tagList(
        shiny$fluidRow(
          shiny$column(12, shiny$uiOutput(ns("model_test_header"))),
          shiny$column(12, shiny$uiOutput(ns("gof_title"))),
          shiny$column(12, shiny$verbatimTextOutput(ns("gof"))),
          shiny$column(12, shiny$uiOutput(ns("gof_exp_lr_title"))),
          shiny$column(12, shiny$verbatimTextOutput(ns("lr"))),
          shiny$column(12, shiny$uiOutput(ns("lr_exp_pcar_title"))),
          shiny$column(12, shiny$verbatimTextOutput(ns("pcar_sum"))),
          shiny$column(12, shiny$verbatimTextOutput(ns("pcar_loadings"))),
          shiny$column(12, shiny$uiOutput(ns("pcar_exp")))
        )
      )
    })

    return(list(
      gof_obj = gof_obj,
      lr_obj = lr_obj,
      pcar_obj = pcar_obj
    ))
  })
}
