box::use(
  shiny[NS, tagList, div, HTML, uiOutput, moduleServer, renderUI, observeEvent, req, reactiveVal, actionButton, reactive],
  shinyWidgets[awesomeCheckbox]
)

box::use(
  app/view/import
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    import$ui(ns('import')),
    div(
      # style = 'padding: 0px 10px 0px 10px;',
      class = 'button-box',
      HTML('<b>Analysis Options</b>'),
      uiOutput(ns('impute_button')),
      uiOutput(ns('link_button')),
    ),
    uiOutput(ns('analysis_button'))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    import_values <- import$server('import')

    output$impute_button <- renderUI({
      awesomeCheckbox(
        inputId = ns("impute_option"),
        label = "Impute missing data",
        value = FALSE
      )
    })

    output$link_button <- renderUI({
      awesomeCheckbox(
        inputId = ns("link_option"),
        label = "Link and rescale scores",
        value = FALSE
      )
    })

    output$analysis_button <- renderUI({
      req(import_values$rval_df())
      actionButton(
        ns('run_analysis'),
        'Run Analysis',
        width = '100%',
        style =
          "color: #fff;
           background-color: #243f3f;
           border-color: #243f3f;
           border-radius: 10px;
           border-width: 2px"
      )
    })

    # Return values for main app
    return(list(
      import_values = import_values,
      impute_option = reactive(input$impute_option),
      link_option = reactive(input$link_option),
      run_analysis = reactive(input$run_analysis)
    ))

  })
}

# Other buttons that we never finished
  # output$model_button <- renderui({
  #   req(input$file)
  #   pickerinput(
  #     inputid = "model_selection",
  #     label = "select model",
  #     choices = c(
  #       'rasch model (cml)',
  #       'rasch model (mml)',
  #       'two-parameter logistic',
  #       'three-parameter birnbaum'
  #     ),
  #     choicesopt = list(
  #       subtext = c(
  #         'erm::rm()',
  #         'ltm::rasch()',
  #         'ltm::ltm()',
  #         'ltm::tpm()'
  #       ),
  #       style =
  #         # "color: #fff;"
  #         # 'background-color: #243f3f;'
  #         'border-color: #243f3f;
  #         border-width: 5px;'
  #       # border-radius: 10px;
  #       # width: 100%;"
  #     ),
  #     options = pickeroptions(
  #       list(
  #         container = "body",
  #         style =
  #           "color: #fff;
  #           width: 100%;"
  #         # background-color: #243f3f;
  #         # border-color: #243f3f;
  #         # border-radius: 10px;
  #         # border-width: 2px;
  #       )
  #     )
  #   )
  # })

  # output$model_dropdown <- renderui({
  #   req(input$file)
  #   radiogroupbuttons(
  #     inputid = "model_dropdown",
  #     label = "select model",
  #     choices = c(
  #       "rasch model (cml)",
  #       'rasch model (mml)',
  #       'two parameter logistic (mml)',
  #       'three parameter birnbaum (mml)'
  #     ),
  #     # status = 'info',
  #     justified = true,
  #     direction = 'vertical',
  #     width = '100%',
  #     checkicon = list(
  #       yes = icon("ok",
  #                  lib = "glyphicon"))
  #   )
  #
  # })
