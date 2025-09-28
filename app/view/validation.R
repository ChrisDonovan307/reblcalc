box::use(
  shiny[NS, uiOutput, moduleServer, reactive, req, renderUI, HTML, tagList, fluidRow, column, renderText],
  eRm[person.parameter, gofIRT, LRtest],
  shinycssloaders[showPageSpinner]
)

# Model Validation Module
ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("validation"))
}

server <- function(id, rval_model, analysis_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Goodness of fit tests
    gof_obj <- reactive({
      req(rval_model())
      rval_model() %>%
        person.parameter() %>%
        gofIRT() %>%
        spinner_wrapper(HTML('Calculating Goodness of Fit...'))
    })

    # LR test of invariance
    lr_obj <- reactive({
      req(rval_model())
      showPageSpinner(
        rval_model() %>%
          LRtest() %>%
          spinner_wrapper(HTML('Calculating test of invariance...'))
      )
    })

    # PCAR test of unidimensionality
    pcar_obj <- reactive({
      req(rval_model())
      rval_model() %>%
        test_uni_pcar() %>%
        spinner_wrapper(HTML('Calculating test of unidimensionality...'))
    })

    output$model_test_header <- renderUI({
      HTML(
        '<h2 class="body-header-2">Results</h2>
        <p>Analysis is complete! On this page, you
          can find fit statistics, the likelihood-ratio test of invariance, as
          well as a test of unidimensionality using principal components
          analysis.</p>'
      )
    })

    output$gof_title <- renderUI({
      HTML(
        '<h3 class="body-header-3">Goodness of Fit</h3>'
      )
    })

    output$gof <- renderPrint({
        gof_obj() %>%
          summary()
    })

    output$gof_exp_lr_title <- renderUI({
      HTML(
        '<p>Here we have a slew of goodness of fit statistics for our Rasch model.
        For details on interpretation, see
        <a href="https://escholarship.org/content/qt1m46j62q/qt1m46j62q.pdf">
        Mair et al. (2008).</a></p>
        <h3 class="body-header-3">Invariance</h3>
        '
      )
    })

    # LR Outputs
    output$lr <- renderPrint({
      lr_obj()
    })

    # LR exp and PCAR title
    output$lr_exp_pcar_title <- renderUI({
      HTML(
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

    output$pcar_sum <- renderPrint({
      pcar_obj() %>%
        summary()
    })

    output$pcar_loadings <- renderPrint({
      pcar_obj()$Vaccounted
    })

    output$pcar_exp <- renderUI({
      HTML(
        '<p>The top output shows the summary from the PCA model. The bottom output
        shows the loadings of the principal components (SS loadings). If the
        highest SS loading of any component is less than 2.0, thet test shows
        demonstrates unidimensionality.</p>'
      )
    })

    # Now put all UI outputs together
    output$validation <- renderUI({
      req(analysis_state())

      tagList(
        fluidRow(
          column(12, uiOutput(ns('model_test_header'))),
          column(12, uiOutput(ns('gof_title'))),
          column(12, verbatimTextOutput(ns('gof'))),
          column(12, uiOutput(ns('gof_exp_lr_title'))),
          column(12, verbatimTextOutput(ns('lr'))),
          column(12, uiOutput(ns('lr_exp_pcar_title'))),
          column(12, verbatimTextOutput(ns('pcar_sum'))),
          column(12, verbatimTextOutput(ns('pcar_loadings'))),
          column(12, uiOutput(ns('pcar_exp')))
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
