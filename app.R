suppressPackageStartupMessages(if (!requireNamespace('pacman')) {
  install.packages('pacman', dependencies = TRUE)
})

pacman::p_load(
  shiny,
  dplyr,
  eRm,
  shinyWidgets,
  shinythemes,
  readr,
  ggplot2,
  tibble,
  purrr,
  stringr,
  bslib,
  shinyjs,
  zip,
  missForest,
  shinycssloaders,
  psych,
  skimr,
  conflicted
)

suppressMessages(
  conflicts_prefer(
    eRm::personfit,
    eRm::itemfit,
    dplyr::select,
    dplyr::filter,
    .quiet = TRUE
  )
)

# Set spinner options
options(
  spinner.type = 6,
  spinner.color = '#2F4F4F',
  spinner.proxy.height = '100px',
  spinner.hide.ui = TRUE
)

# rebl_items <- readRDS('data/rebl_items.rds')
# rebl_text <- readRDS('data/rebl_text.rds')
data(rebl_text)
# baseline_model <- readRDS('data/baseline_model.rds')

# source('R/get_person_fit.R')
# source('R/get_item_fit.R')
# source('R/plotPImap2.R')
# source('R/link_tests.R')
# source('R/read_erm.R')
# source('R/test_uni_pcar.R')

# Load modules
source('R/mod_landing_page.R')
source('R/mod_rebl_items_page.R')



# UI ----------------------------------------------------------------------


ui <- fluidPage(

  theme = bslib::bs_theme(
    preset = 'lumen',
    info = '#2F4F4F',
    primary = '#2F4F4F',
    font_scale = 1
  ),

  useShinyjs(),

  fluidRow(
    column(12, div(
      style = "text-align: center; padding: 4px;",
      img(src = "forest3.jpg", width = '100%')
    ))
  ),



  sidebarLayout(

    ## Sidebar Panel -----------------------------------------------------------

    sidebarPanel(

      div(
        style = "text-align: center;",
        h1(
          "REBL Scale Calculator",
          style = "color: #2F4F4F; font-weight: bold;"
        )
      ),

      # Upload data
      fileInput(
        'file',
        'Upload .csv or .xlsx file',
        accept = c('.csv', '.xlsx', '.xls')
      ),

      # Select respondent ID column
      uiOutput('dropdown_columns'),

      # User options
      uiOutput('impute_button'),
      uiOutput('link_button'),

      # Run analysis
      uiOutput('analysis_button'),

      # Download zip of files
      uiOutput('download_button')

    ),


    ## Main Panel --------------------------------------------------------------

    mainPanel(

      tabsetPanel(
        id = 'tabset',
        tabPanel(
          'Info',
          fluidRow(
            landing_page_ui('landing_page'),
            div(uiOutput('model_tests'))
          )
        ),
        # tabPanel('REBL Items', withSpinner(uiOutput('rebl_items_page'))),
        tabPanel('REBL Items', withSpinner(rebl_items_page_ui('rebl_items_page'))),

        tabPanel('Skim', withSpinner(uiOutput('skim_page'))),
        tabPanel('Imputation', withSpinner(tableOutput('imp_page'))),
        tabPanel('Item Map', withSpinner(uiOutput('item_map_page'))),
        tabPanel('ICC Plot', withSpinner(uiOutput('icc_page'))),
        tabPanel('PI Map', withSpinner(uiOutput('pi_map_page'))),
        tabPanel('Histogram', withSpinner(uiOutput('hist_page'))),
        tabPanel('Person Fit', withSpinner(uiOutput('person_fit_page'))),
        tabPanel('Item Fit', withSpinner(uiOutput('item_fit_page')))
      )
    )
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {

  # Housekeeping ------------------------------------------------------------

  # Set state to false. When true, download button appears, and landing page
  # becomes model test page
  analysis_state <- reactiveVal(FALSE)

  # Landing page server. Only appears if analysis state is true
  landing_page_server('landing_page', analysis_state)
  rebl_items_page_server('rebl_items_page')


  # Import File ------------------------------------------------------------

  rval_df <- eventReactive(input$file, {

    # Make sure file is uploaded
    req(input$file)

    # Read data frame from uploaded file
    if (tools::file_ext(input$file$name) == "csv") {
      df <- read_csv(input$file$datapath)
    } else if (tools::file_ext(input$file$name) %in% c("xlsx", "xls")) {
      df <- read_excel(input$file$datapath)
    } else {
      stop("Unsupported file format.")
    }

    # Make sure rebl items are numeric
    df <- df %>%
      mutate(across(all_of(rebl_items), as.numeric))

    # Make sure all REBL items are in DF
    if (!all(rebl_items %in% names(df))) {
      stop(
        'File must contain all 24 REBL items by name. Reload app and check
          Info tab for a list of items.'
      )
    }

    # Make sure no one answered either all 1 or all 0
    bad_patterns <- which(rowSums(select(df, all_of(rebl_items)), na.rm = TRUE) %in% c(0, 24))
    if (length(bad_patterns) > 0) {
      stop(
        'Cannot calculate REBL Score for person(s) ',
        paste0(bad_patterns, collapse = ', '),
        ' due to unacceptable pattern of responses. Check to make sure they ',
        'answered at least one question "yes" and at least one question "no".'
      )
    }

    # Notify them if there is a lot of missing data
    if (all(rebl_items %in% names(df))) {
      n_miss <- df %>%
        select(all_of(rebl_items)) %>%
        is.na() %>%
        sum()
      if (n_miss >= 2000) {
        showModal(modalDialog(
          title = 'Warning',
          paste0(
            'There are ',
            format(n_miss, big.mark = ','),
            ' missing values in the REBL item columns of your dataset. Be
            aware that whether or not you decide to impute these data, the
            run time of your analysis could increase substantially. Explore
            your data in the "Skim" tab to see where the missing values are.'
          ),
          easyClose = TRUE,
          footer = modalButton('OK')
        ))
      }
    }

  return(df)
})

  # Get column names to send back to UI to choose respondent ID
  output$dropdown_columns <- renderUI({
    column_names <- colnames(rval_df())
    tagList(
      selectInput('respondent_id', 'Select Person ID Column', choices = column_names)
    )
  })


  # User Options ------------------------------------------------------------

  output$impute_button <- renderUI({
    req(input$file)
    awesomeCheckbox(
      inputId = "impute_option",
      label = "Impute missing data",
      value = FALSE
    )
  })

  output$link_button <- renderUI({
    req(input$file)
    awesomeCheckbox(
      inputId = "link_option",
      label = "Link and rescale scores",
      value = FALSE
    )
  })

  output$lr_button <- renderUI({
    req(input$file)
    awesomeCheckbox(
      inputId = "lr_option",
      label = "LR test of invariance",
      value = TRUE
    )
  })

  output$pcar_button <- renderUI({
    req(input$file)
    awesomeCheckbox(
      inputId = "pcar_option",
      label = "PCAR test of unidimensionality",
      value = TRUE
    )
  })

  # output$model_button <- renderUI({
  #   req(input$file)
  #   pickerInput(
  #     inputId = "model_selection",
  #     label = "Select Model",
  #     choices = c(
  #       'Rasch Model (CML)',
  #       'Rasch Model (MML)',
  #       'Two-Parameter Logistic',
  #       'Three-Parameter Birnbaum'
  #     ),
  #     choicesOpt = list(
  #       subtext = c(
  #         'eRm::RM()',
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
  #     options = pickerOptions(
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

  # output$model_dropdown <- renderUI({
  #   req(input$file)
  #   radioGroupButtons(
  #     inputId = "model_dropdown",
  #     label = "Select Model",
  #     choices = c(
  #       "Rasch Model (CML)",
  #       'Rasch Model (MML)',
  #       'Two Parameter Logistic (MML)',
  #       'Three Parameter Birnbaum (MML)'
  #     ),
  #     # status = 'info',
  #     justified = TRUE,
  #     direction = 'vertical',
  #     width = '100%',
  #     checkIcon = list(
  #       yes = icon("ok",
  #                  lib = "glyphicon"))
  #   )
  #
  # })

  # Reset analysis state to FALSE if any of the buttons are pushed
  observeEvent(input$impute_option | input$link_option, {
    analysis_state(FALSE)
  }, ignoreNULL = FALSE)


  # Imputation --------------------------------------------------------------

  # Impute data if option is selected
  rval_imp_out <- eventReactive(input$run_analysis, {

    # First reorder the DF to put respondent id first, then rebl items in order
    df <- rval_df() %>%
      select(input$respondent_id, all_of(rebl_items))

    if (input$impute_option == FALSE) {
      return(df)
    } else {
      if (sum(is.na(select(rval_df(), all_of(rebl_items)))) == 0) {
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

  rval_df_clean <- eventReactive(input$run_analysis, {
    if (input$impute_option == FALSE) {
      rval_imp_out()
    } else if (input$impute_option == TRUE) {
      rval_imp_out()$ximp %>%
        mutate(across(everything(), ~ as.numeric(.) - 1)) %>%
        bind_cols(rval_df() %>% select(-all_of(rebl_items)))
    }
  })

  # Also save imputation error stats every time imputation happens
  rval_imp_oob <- eventReactive(input$run_analysis, {
    req(rval_imp_out())
    rval_imp_out()$OOB
  })

  # tabPanel for imputation
  output$imp_page <- renderUI({
    req(rval_model())
    tagList(
      fluidRow(
        column(12, uiOutput('imp_title')),
        column(12, uiOutput('imp_exp')),
        column(12, tableOutput('oob_print')),
        column(12, uiOutput('no_imp'))
      )
    )
  })
0
  output$imp_title <- renderUI({
    HTML(
      '<h3 style="color: #2F4F4F; font-weight: bold;">Imputation</h3>'
    )
  })

  output$no_imp <- renderUI({
    req(rval_model)
    if (input$impute_option == FALSE && analysis_state() == TRUE)
      HTML(
        '<p>No imputation performed.</p>'
      )
  })

  # Add imputation output, only if performed though
  output$imp_exp <- renderText({
    if (input$impute_option == TRUE && analysis_state() == TRUE) {
      HTML(
        '<p>Out-of-bag (OOB) errors are an estimate of the imputation
        error from the missForest algorithm. For categorical data, they
        represent the proportion of falsely classified values (PFC). A value of
        0 would represent perfect prediction.</p>'
      )
    }
  })

  output$oob_print <- renderTable({
    if (input$impute_option == TRUE && analysis_state() == TRUE) {
      df <- data.frame(
        names(rval_imp_out()$ximp),
        round(rval_imp_oob(), 3)
      ) %>%
        setNames(c('REBL Item', 'PFC')) %>%
        arrange(PFC)
      df
    }
  })


  # Run Rasch Model ---------------------------------------------------------

  # Show run analysis button only once file is input
  output$analysis_button <- renderUI({
    req(input$file)
    actionButton(
      'run_analysis',
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

  # Run Rasch model
  rval_model <- eventReactive(input$run_analysis, {
    req(rval_df_clean())

    # Check for unique respondent ID column
    if (any(duplicated(rval_df()[[input$respondent_id]]))) {
      stop(
        'The respondent ID column that you have selected does not uniquely',
        ' identify respondents. Please choose another column.'
      )
    } else {

      # Run Rasch model
      showPageSpinner({
        rval_df_clean() %>%
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


  ## GoF, LR, PCAR -----------------------------------------------------------


  # Putting all the model tests together into one output

  # First make objects outside of renderUI function. Use these to save later
  gof_obj <- reactive({
    req(rval_model())
    showPageSpinner(
      rval_model() %>%
        person.parameter() %>%
        gofIRT(),
      type = 6,
      color = '#2F4F4F',
      caption = HTML(
        'Calculating Goodness of Fit...'
      )
    )
  })

  lr_obj <- reactive({
    req(rval_model())
    showPageSpinner(
      rval_model() %>%
        LRtest(),
      type = 6,
      color = '#2F4F4F',
      caption = HTML(
        'Calculating test of invariance...'
      )
    )
  })

  pcar_obj <- reactive({
    req(rval_model())
    showPageSpinner(
      rval_model() %>%
        test_uni_pcar(),
      type = 6,
      color = '#2F4F4F',
      caption = HTML(
        'Calculating test of unidimensionality...'
      )
    )
  })

  # Now put all UI outputs together
  output$model_tests <- renderUI({
    req(rval_model(), analysis_state())
    tagList(
      fluidRow(
        column(12, uiOutput('model_test_header')),
        column(12, uiOutput('gof_title')),
        column(12, verbatimTextOutput('gof')),
        column(12, uiOutput('gof_exp_lr_title')),
        column(12, verbatimTextOutput('lr')),
        column(12, uiOutput('lr_exp_pcar_title')),
        column(12, verbatimTextOutput('pcar_sum')),
        column(12, verbatimTextOutput('pcar_loadings')),
        column(12, uiOutput('pcar_exp'))
      )
    )
  })

  output$model_test_header <- renderUI({
    HTML(
      '<h2 style="color: #2F4F4F; font-weight: bold;">Results</h2>
      <p style="font-size: 16px;">Analysis is complete! On this page, you
        can find fit statistics, the likelihood-ratio test of invariance, as
        well as a test of unidimensionality using principal components
        analysis.</p>'
    )
  })
  output$gof_title <- renderUI({
    HTML(
      '<h3 style="color: #2F4F4F; font-weight: bold;">Goodness of Fit</h3>'
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
      <b>Mair et al. (2008).</b></a></p>
      <h3 style="color: #2F4F4F; font-weight: bold;">Invariance</h3>
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
      <h3 style="color: #2F4F4F; font-weight: bold;">Unidimensionality</h3>
      <p>Here we test that the scale is measuring a single dimension by running
      a Principal Components Analysis (PCA).'
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


  ## Test Linking ------------------------------------------------------------

  # If link_option == TRUE, get new rescaled thetas and add them to the outputs
  # Use these for graphs and such also

  # Pulling whole plink out, which includes constants and such
  rval_plink <- reactive({
    if (input$link_option) {
      rval_model() %>%
        link_tests(baseline_model, method = 'SL')
    }
  })

  # Pull rescaled scores for new model only
  rval_rescaled_scores <- reactive({
    if (input$link_option) {
      link.ability(rval_plink())[[2]] %>%
        round(3)
    }
  })


  ## Person Fit --------------------------------------------------------------

  person_fit_data <- reactive({
    out <- rval_model() %>%
      get_person_fit(rval_df_clean(), input$respondent_id)

    if (exists('rval_rescaled_scores') && input$link_option == TRUE) {
      out <- out %>%
        rename(rebl_unscaled = rebl_score) %>%
        mutate(rebl_score = rval_rescaled_scores()) %>%
        select(input$respondent_id, rebl_score, rebl_unscaled, everything())
    }
    return(out)
  })

  # Put outputs together on page
  output$person_fit_page <- renderUI({
    req(rval_model())
    tagList(
      column(12, uiOutput('person_fit_exp')),
      column(12, DT::DTOutput('person_fit_table'))
    )
  })

  output$person_fit_exp <- renderText({
    if (analysis_state()) {
      HTML(
        '<h3 style="color: #2F4F4F; font-weight: bold;">REBL Scores and Person Fit
          </h3>
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
        <br>
        '
      )
    }
  })

  output$person_fit_table <- DT::renderDT({
    req(rval_model())
    person_fit_data()
  })


  ## Item fit ----------------------------------------------------------------

  item_fit_data <- reactive({
    rval_model() %>%
      get_item_fit(rval_df_clean(), rebl_items)
  })

  output$item_fit_page <- renderUI({
    req(rval_model())
    tagList(
      fluidRow(
        column(12, uiOutput('item_fit_title')),
        column(12, uiOutput('item_fit_exp')),
        column(12, DT::DTOutput('item_fit'))
      )
    )
  })

  output$item_fit_title <- renderUI({
    HTML(
      '<h3 style="color: #2F4F4F; font-weight: bold;">Item Fit</h3>'
    )
  })

  output$item_fit_exp <- renderText({
    if (analysis_state()) {
      HTML(
        '<ul>
          <li><b>prop</b> is the proportion of people who performed the
            pro-environmental behavior in the last week.</li>
          <li><b>i.fit</b> is the item fit, as measured by the standardized
          squared residuals between model implied response patterns and observed
          response patterns.</li>
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
          <li><b>eta.se</b> is the standard error of the item difficulty.</li>
        </ul>'
      )
    }
  })

  output$item_fit <- DT::renderDT({
    item_fit_data()
  })


  # Plots -------------------------------------------------------------------
  ## Item Map ----------------------------------------------------------------

  rval_item_map <- reactive({

    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext = '.png')

    png(
      outfile,
      width = 8,
      height = 8,
      units = 'in',
      # pointsize = 16,
      res = 72
    )
    plotPWmap(
      rval_model(),
      imap = TRUE,
      latdim = 'Item Difficulty',
      tlab = 'Infit t Statistic',
      cex.gen = 1.1,
      cex.pch = 1.1
    )
    dev.off()

    # Return a list for some reason
    list(src = outfile)
  })

  output$item_map_page <- renderUI({
    req(rval_model())
    tagList(
      fluidRow(
        column(12, uiOutput('item_map_title')),
        div(
          style = "width: 100%; text-align: center;",
          plotOutput('item_map', width = '80%', height = '100%'),
        ),
        column(12, uiOutput('item_map_exp'))
      )
    )
  })

  output$item_map_title <- renderUI({
    HTML('<h3 style="color: #2F4F4F; font-weight: bold;">Item Map</h3>')
  })

  output$item_map <- renderImage({
    rval_item_map()
  }, deleteFile = FALSE)

  output$item_map_exp <- renderUI({
    HTML(
      '<p>The Item Map shows the item difficulties (y-axis) against item fit
      (x-axis). Infit is the information-weighted fit statistic, which weights
      nearby respondents more than those farther away on the latent dimension.
      The vertical green lines are at +/- 2 standard deviations from the mean.
      Items outside of these green lines are poorly fitting.</p><br><br><br>'
    )
  })


  ## ICC Plot ----------------------------------------------------------------

  rval_icc_plot <- reactive({

    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext = '.png')

    png(
      outfile,
      width = 6,
      height = 6,
      units = 'in',
      res = 100
    )
    plotjointICC(
      rval_model(),
      legend = TRUE,
      legpos = 'bottomright',
      lwd = 2,
      main = 'Joint ICC Plot',
      xlab = 'REBL Score',
      ylab = 'Probability to Perform REBL Item',
      xlim = c(-5, 6)
    )
    dev.off()

    # Return a list for some reason
    list(src = outfile,
         alt = "This is alternate text")
  })

  output$icc_page <- renderUI({
    req(rval_model())
    tagList(
      fluidRow(
        column(12, uiOutput('icc_title')),
        div(
          style = "width: 100%; text-align: center;",
          plotOutput('icc', width = '80%', height = '100%')
        ),
        column(12, uiOutput('icc_exp'))
      )
    )
  })

  output$icc_title <- renderUI({
    HTML(
    '<h3 style="color: #2F4F4F; font-weight: bold;">Item Characteristic Curves</h3>'
    )
  })

  output$icc <- renderImage({
    rval_icc_plot()
  }, deleteFile = FALSE)

  # Explanation
  output$icc_exp <- renderUI({
    HTML(
      'This plot shows the Item Characteristic Curves of all the REBL items.
      A person of a given REBL Score (x-axis) is estimated to have a certain
      probability of performing each PEB (y-axis).
      <br><br><br>'
    )
  })


  ## PI Map ------------------------------------------------------------------

  output$pi_map_page <- renderUI({
    req(rval_model())
    tagList(
      fluidRow(
        column(12, uiOutput('pi_map_title')),
        div(
          style = "width: 100%; text-align: center;",
          plotOutput('pi_map', width = '80%', height = '100%')
        ),
        # column(12, plotOutput('pi_map', height = '100%', width = '100%')),
        column(12, uiOutput('pi_map_exp'))
      )
    )
  })

  rval_pi_map <- reactive({

    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext = '.png')

    png(
      outfile,
      width = 8,
      height = 6,
      units = 'in',
      res = 100
    )
    plotPImap2(
      rval_model(),
      sorted = TRUE,
      main = NA,
      latdim = 'Latent Trait',
      pplabel = 'Latent Trait\nDistribution',
      cex.gen = 0.8,
      margins = c(3, 11, 0, 1)
    )
    dev.off()

    # Return a list for some reason
    list(src = outfile,
         alt = "This is alternate text")
  })

  # PI Map Title
  output$pi_map_title <- renderText({
    HTML('<h3 style="color: #2F4F4F; font-weight: bold;">Person Item Map</h3>')
  })

  # PI Map image for tab output
  output$pi_map <- renderImage({
    rval_pi_map()
  }, deleteFile = FALSE)

  # Explanation
  output$pi_map_exp <- renderUI({
    HTML(
      'The lower plot of the Person Item Map shows the REBL items and item
      difficulties in order of easiest to hardest. THe distribution of REBL
      scores is shown in the histogram on top. This is a way to see how well
      the item difficulties match the person abilities.
      <br><br><br>'
    )
  })


  ## REBL Hist --------------------------------------------------------------

  rval_rebl_hist <- reactive({

    person_fit_data() %>%
      ggplot(aes(x = rebl_score)) +
      geom_histogram(
        binwidth = 0.3,
        fill = 'grey',
        color = 'black'
      ) +
      theme_classic() +
      labs(
        x = 'REBL Score',
        y = 'Count'
      ) +
      scale_x_continuous(breaks = seq(-5, 3, 1))
  })

  output$hist_page <- renderUI({
    req(rval_model())
    tagList(fluidRow(
      column(12, uiOutput('hist_title')),
      column(
        12,
        div(style = 'display: flex; justify-content: center;', plotOutput('hist', width = '750px'))
      ),
      column(12, uiOutput('hist_exp'))
    ))
  })

  output$hist_title <- renderText({
    HTML('<h3 style="color: #2F4F4F; font-weight: bold;">REBL Score Histogram</h3>')
  })

  output$hist <- renderPlot({
    rval_rebl_hist() +
      theme(
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)
      )
  })

  output$hist_exp <- renderText({
    HTML(
      '<p>This histogram shows REBL Scores for your sample. If you chose to
      rescale your scores based on our baseline model, the rescaled scores will
      be shown here. Note that the CML estimation method used in the eRm package
      does not assume assume a perfectly normal distribution of scores, so it is
      okay if the scores are not centered on zero.</p><br><br><br>'
    )
  })


  # Landing Page -----------------------------------------------------------

  observeEvent(input$run_analysis, {
    analysis_state(TRUE)
  })


  # Skim --------------------------------------------------------------------

  output$skim_page <- renderUI({
    req(rval_df())
    tagList(
      fluidRow(
        column(12, uiOutput('skim_title')),
        column(12, verbatimTextOutput('skim'))
      )
    )
  })

  output$skim_title <- renderUI({
    req(rval_df())
    HTML(
      '<h3 style="color: #2F4F4F; font-weight: bold;">Skim</h3>
      <p>Explore summary statistics and missing values from your dataset.</p>'
    )
  })

  output$skim <- renderPrint({
    req(rval_df())
    skim(rval_df(), .data_name = 'Your_Data') %>%
      select(-ends_with('hist'))
  })


  # Download zip ------------------------------------------------------------

  output$download_button <- renderUI({
    req(input$run_analysis, rval_model(), person_fit_data(), item_fit_data(), analysis_state())
    tagList(
      HTML('<br><br>'),
      downloadButton(
        'download_zip',
        label = 'Download results',
        style =
          "color: #fff;
           background-color: #243f3f;
           border-color: #243f3f;
           border-radius: 10px;
           border-width: 2px;
           width: 100%;"
      )
    )
  })

  output$download_zip <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_rebl_output.zip")
    },
    content = function(zip_file) {
      temp_dir <- tempdir()

      # Define file names and paths
      files <- list(
        person_fit_csv = file.path(temp_dir, 'person_fit_and_scores.csv'),
        item_fit_csv = file.path(temp_dir, 'item_fit.csv'),
        item_map_png = file.path(temp_dir, 'item_map.png'),
        icc_plot_png = file.path(temp_dir, 'icc_plot.png'),
        pi_map_png = file.path(temp_dir, 'pi_map.png'),
        rebl_hist_png = file.path(temp_dir, 'rebl_hist.png'),
        rasch_model = file.path(temp_dir, 'rebl_model.rds'),
        model_tests = file.path(temp_dir, 'model_tests.rds')
      )

      # Save CSV files
      person_fit_data() %>%
        full_join(rval_df(), by = input$respondent_id) %>%
        write.csv(files$person_fit_csv)
      write.csv(item_fit_data(), files$item_fit_csv)

      # Save model as rds
      saveRDS(rval_model(), files$rasch_model)

      # Save list of GoF, LR, and PCAR as rds
      list(gof_obj(), lr_obj(), pcar_obj()) %>%
        setNames(c('GoF', 'LR', 'PCAR')) %>%
        saveRDS(files$model_tests)

      # If imputed, save rds list with raw imputation result and OOB
      if (input$impute_option == TRUE) {
        imp_out <- file.path(temp_dir, 'imp_out.rds')
        saveRDS(rval_imp_out(), imp_out)
        files$rval_imp_out <- imp_out
      }

      # Copy PNG files
      file.copy(from = rval_item_map()$src, to = files$item_map_png)
      file.copy(from = rval_icc_plot()$src, to = files$icc_plot_png)
      file.copy(from = rval_pi_map()$src, to = files$pi_map_png)

      # Save histogram plot
      ggsave(
        filename = files$rebl_hist_png,
        plot = rval_rebl_hist(),
        device = "png",
        width = 5,
        height = 3,
        units = 'in',
        dpi = 300
      )

      # Create the zip file
      zip::zipr(zip_file, files = unlist(files))
    },
    contentType = 'application/zip'
  )
}

shinyApp(ui = ui, server = server)
