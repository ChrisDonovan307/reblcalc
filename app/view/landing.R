box::use(
  shiny
)

# Landing Page

ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$uiOutput(ns("landing"))
}

server <- function(id, analysis_state) {
  shiny$moduleServer(id, function(input, output, session) {

    output$landing <- shiny$renderUI({
      shiny$req(!analysis_state())
      shiny$HTML(
        '<h3 class="body-header-3">Welcome</h3>
        <p>This app is designed to analyze data from an administration of the Repeated Environmental Behavior
          Latent (REBL) Scale, a tool for measuring preferences for pro-environmental
          behaviors (PEB). The scale consists of 24 questions with binary (yes/no)
          responses regarding repeatable actions in the last week that have an
          environmental impact. The survey takes just a few minutes to
          complete, so it can be repeatedly administered. It uses the Rasch
          model to develop a logistic scale of preferences for PEB that is both
          unidimensional invariant. See the pre-print for details on scale
          development:
          <a href="https://osf.io/preprints/osf/w92se"><b>Shrum et al. (2024).
          </b></a> Rasch modeling is performed using the eRm package:
          <a href="https://www.jstatsoft.org/article/view/v020i09"><b>Mair and
          Hatzinger (2007).</b></a></p>
        <p>To begin, press the <b>browse</b> button. The The REBL Scale
          Calculator accepts a .csv or .xlsx file with at least
          25 columns: 24 for the REBL Items and 1 for a unique respondent ID.
          Once you upload it, a dropdown will appear where you must select a
          <b>person id</b> column in the dataframe that uniquely identifies your
          respondents. Note that it does not matter what order the REBL items of
          your dataset are in, but they must all be present and spelled
          correctly. The app provides options for:</p>
        <ul>
            <li><b>Missing data imputation -</b> If checked, the app will impute
              missing data using the
              <a href="https://academic.oup.com/bioinformatics/article/28/1/112/219101?login=false">
              <b>missForest</b></a> package. Variable
              wise Out-of-Bag (OOB) error rates will be returned, as well as a
              copy of the imputed dataset. Note that datasets with lots of
              missing data can take a couple of minutes to process, whether or
              not you choose to impute it.</li>
            <li><b>Test Linking -</b> Rescale your REBL Scores to match our
              representative sample of US residents stratified by age, sex, and
              race (n = 1366). We use the
              <a href="https://cran.r-project.org/web/packages/plink/vignettes/plink-UD.pdf">
              <b>plink package</b></a> to equate
              tests using the Stocking-Lord method, giving you a linear
              transformation of scores using the item difficulties we identified
              in scale development.</li>
        </ul>
        <p>Once settings are entered, push the <b>run analysis</b> button. A
          summary of the outputs will appear
          on this page, while plots and tables will populate in the other panels.
          After the analysis has been run, a <b>download</b> button will
          appear in the side panel. This will allow you to download a .zip file
          of all the plots as .png, as well as person fit and item fit tables as
          .csv. It will also contain the rasch model output, a list of model
          tests (GoF, LR, PCAR), and if imputation data (if selected), all as
          .rds files. If any of the parameters in the side panel are changed
          after running an analysis, the info tab will reset to the welcome
          page.</p>
        <h3 class="body-header-3">Development</h3>
        <p>The REBL Score Calculator app is currently limited to CML models from
          the eRm package and test equating using the full set of all REBL
          items. Plans are to update the app with two-parameter and
          three-parameter options to model discrimination and guessing
          parameters, as well as adding fixed calibration methods.
        <p>If you have any questions about the REBL Scale or the REBL Score
          Calculator app or would like to report a bug or feature request, feel
          free to reach out to Dr. Trisha Shrum (trisha.shrum@uvm.edu) or Chris
          Donovan (christopher.donovan@uvm.edu)</p>'
      )
    })

  })
}
