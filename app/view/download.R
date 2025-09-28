box::use(
  shiny,
  dplyr[full_join, `%>%`],
  ggplot2[ggsave],
  zip[zipr],
  utils[write.csv],
  stats[setNames]
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$uiOutput(ns('download_button'))
}

#' @export
server <- function(id,
                   rval_model,
                   person_fit_data,
                   item_fit_data,
                   analysis_state,
                   rval_item_map,
                   rval_icc_plot,
                   rval_pi_map,
                   rval_imp_out,
                   rval_df,
                   rval_rebl_hist,
                   gof_obj,
                   lr_obj,
                   pcar_obj,
                   import_values,
                   impute_option) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$download_button <- shiny$renderUI({
      shiny$req(
        rval_model(),
        person_fit_data(),
        item_fit_data(),
        analysis_state(),
        gof_obj(),
        lr_obj(),
        pcar_obj()
      )
      shiny$tagList(
        shiny$HTML('<br><br>'),
        shiny$downloadButton(
          ns('download_zip'),
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

    output$download_zip <- shiny$downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_REBL_Calculator.zip")
      },
      content = function(zip_file) {
        tryCatch({
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
          tryCatch({
            person_fit_data() %>%
              full_join(rval_df(), by = import_values$respondent_id()) %>%
              write.csv(files$person_fit_csv)
          }, error = function(e) {
            cat("Error saving person fit CSV:", e$message, "\n")
            stop(e)
          })

          write.csv(item_fit_data(), files$item_fit_csv)

          # Save model as rds
          saveRDS(rval_model(), files$rasch_model)

          # Save list of GoF, LR, and PCAR as rds
          list(gof_obj(), lr_obj(), pcar_obj()) %>%
            setNames(c('GoF', 'LR', 'PCAR')) %>%
            saveRDS(files$model_tests)

          # If imputed, save rds list with raw imputation result and OOB
          if (impute_option() == TRUE) {
            imp_out <- file.path(temp_dir, 'imp_out.rds')
            saveRDS(rval_imp_out(), imp_out)
            files$rval_imp_out <- imp_out
          }

          # Copy PNG files
          tryCatch({
            file.copy(from = rval_item_map()$src, to = files$item_map_png)
            file.copy(from = rval_icc_plot()$src, to = files$icc_plot_png)
            file.copy(from = rval_pi_map()$src, to = files$pi_map_png)
          }, error = function(e) {
            cat("Error copying PNG files:", e$message, "\n")
            stop(e)
          })

          # Save histogram plot
          tryCatch({
            ggsave(
              filename = files$rebl_hist_png,
              plot = rval_rebl_hist(),
              device = "png",
              width = 5,
              height = 3,
              units = 'in',
              dpi = 300
            )
          }, error = function(e) {
            cat("Error saving histogram:", e$message, "\n")
            stop(e)
          })

          # Create the zip file
          zip::zipr(zip_file, files = unlist(files))

        }, error = function(e) {
          cat("ERROR in download handler:", e$message, "\n")
          stop(paste("Download failed:", e$message))
        })
      },
      contentType = 'application/zip'
    )

  })
}
