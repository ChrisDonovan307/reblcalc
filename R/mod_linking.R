# Linking Module
linking_ui <- function(id) {
  ns <- NS(id)
}

linking_server <- function(id, button_values, rval_model) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # If link_option == true, get new rescaled thetas and add them to the outputs
    # use these for graphs and such also

    # Pulling whole plink out, which includes constants and such
    rval_plink <- reactive({
      req(button_values$link_option())
      rval_model() %>%
        link_tests(baseline_model, method = 'SL')
    })

    # Pull rescaled scores for new model only
    rval_rescaled_scores <- reactive({
      req(rval_plink())
      plink::link.ability(rval_plink())[[2]] %>%
        round(3)
    })

    return(rval_rescaled_scores)

  })
}
