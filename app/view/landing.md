<h3 class="body-header-3">Introduction</h3>

This application is designed to analyze data from an administration of the Repeated Environmental Behavior Latent (REBL) Scale, a tool for measuring preferences for pro-environmental behaviors. The scale consists of 24 binary (yes/no) questions regarding repeatable actions in the last week that have an environmental impact. The survey takes just a few minutes to complete, so it can be repeatedly administered. It uses the Rasch model to develop a logistic scale of preferences for pro-environmental behaviors that is both unidimensional invariant. See the pre-print for details on scale development: [**Shrum et al. (2024).**](https://osf.io/preprints/osf/w92se) Rasch modeling is performed using the eRm  [**Mair and Hatzinger (2007).**](https://www.jstatsoft.org/article/view/v020i09) and ltm [**Rizopoulos (2007)**](https://www.jstatsoft.org/article/view/v017i05/0) packages.

<h3 class="body-header-3">Calculating Scores</h3>

To begin, you can either upload your own dataset or use a synthetic dataset to explore the app. If you use your own, the REBL Scale Calculator accepts a .csv or .xlsx file with at least 25 columns: 24 for the REBL Items and 1 for a unique respondent ID. Once you upload it, a dropdown menu will appear where you ill select a **respondent id** column in the data frame that uniquely identifies your respondents. Note that it does not matter what order the REBL items of your dataset are in, but they must all be present and spelled correctly. The app provides options for:

- **Missing data imputation:** If checked, the app will impute missing data using the [**missRanger**](https://mayer79.github.io/missRanger/) package. Variable wise Out-of-Bag (OOB) error rates will be returned along with a copy of the imputed dataset. Note that datasets with lots of missing data can take some time to process, whether or not you choose to impute it.
- **Test Linking:** Rescale your REBL Scores to match our representative sample of US residents stratified by age, sex, and race (n = 1366). We use the [**plink package**](https://cran.r-project.org/web/packages/plink/vignettes/plink-UD.pdf) to equate tests using the Stocking-Lord method, giving you a linear transformation of scores using the item difficulties we identified in scale development.

Once settings are entered, push the **run analysis** button. Model validation outputs will appear on this page, while plots and tables will populate in the other panels. After the analysis has been run, a **download** button will appear at the bottom of the side panel. This will allow you to download a zip file of all the plots as .png files, as well as person fit and item fit tables as .csv files. It will also contain the Rasch model output, a list of model tests (Goodness of Fit, Likelihood Ratio, PCAR), and imputation data (if selected), all as .rds files. If any of the parameters in the side panel are changed after running an analysis, the info tab will reset to the welcome page.

<h3 class="body-header-3">Development</h3>

The REBL Score Calculator is currently limited to CML models from the eRm package and test equating using the full set of all REBL items. Plans are to update the app with two-parameter and three-parameter options to model discrimination and guessing parameters, as well as adding fixed calibration methods.

To request a feature or report a bug in the REBL Score Calculator, head to our [GitHub page](https://github.com/ChrisDonovan307/rebl). This package is built on the [REBL package](https://chrisdonovan307.github.io/rebl/index.html), which makes it easy to run these analyses on your own (but is also still a work in progress - more coming soon).

Feel free to reach out to Chris Donovan (christopher.donovan@uvm.edu) or Dr. Trisha Shrum (trisha.shrum@uvm.edu) with questions or to let us know how you're using the REBL Scale!
