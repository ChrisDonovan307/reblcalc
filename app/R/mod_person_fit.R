# Mod Person Fit

person_fit_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("person_fit_page"))
}

person_fit_server <- function(id,
                              link_option,
                              respondent_id,
                              rval_df_clean,
                              rval_model,
                              analysis_state,
                              rval_rescaled_scores) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    person_fit_data <- reactive({
      req(rval_model())
      out <- rval_model() %>%
        get_person_fit(rval_df_clean(), respondent_id())

      if (!is.null(rval_rescaled_scores) && link_option() == TRUE) {
        req(rval_rescaled_scores())
        out <- out %>%
          rename(rebl_unscaled = rebl_score) %>%
          mutate(rebl_score = rval_rescaled_scores()) %>%
          select(respondent_id(), rebl_score, rebl_unscaled, everything())
      }
      return(out)
    })

    output$person_fit_exp <- renderText({
      if (analysis_state()) {
        HTML(
          '<h3 class="body-header-3">REBL Scores and Person Fit</h3>
          <p>The table below shows your respondent ID, REBL Scores, and a series of
            person fit statistics. If you chose to link and rescale scores, the scaled
            scores will be in the <b>rebl_score</b> field while the raw scores will be
            shown as <b>rebl_score_unscaled</b>. Otherwise, the raw scores will be the
            only score shown.</p>
          <ul>
            <li><b>rebl_score</b> is the Rasch-modeled metric for pro-environmental
            behavior on a logistic interval scale. Higher numbers represent a
            greater latent preference for pro-environmental behavior. </li>
            <li><b>p.fit</b> is the person fit as measured the standardized
            squared residuals between observed responses and model-expected
            responses.</li>
            <li><b>outfit</b> is an outlier-sensitive fit. It weights items and
              respondents that are farther away from one another in the latent
              trait.</li>
            <li><b>infit</b> is the information-weighted fit, which weights nearby
              items and respondents more than those that are farther away.</li>
            <li><b>MSQ</b> is the mean square error. 1.0 is the expected value.
              Smaller values are more predictable, while larger values are less
              predictable.</li>
            <li><b>Z</b> is the z-score of the test of fit. 0.0 is expected, while
            values over +/-2 are misfitting.</li>
          </ul>
          '
        )
      }
    })

    output$person_fit_table <- reactable::renderReactable({
      req(analysis_state(), person_fit_data())
      get_reactable(person_fit_data())
    })

    # Put outputs together on page
    output$person_fit_page <- renderUI({
      req(rval_model(), person_fit_data())
      tagList(
        column(12, uiOutput(ns('person_fit_exp'))),
        column(12, reactable::reactableOutput(ns('person_fit_table')))
      )
    })

    return(person_fit_data)

  })
}
