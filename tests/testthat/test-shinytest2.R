library(shinytest2)

test_that("{shinytest2} recording: reblcalc", {
  app <- AppDriver$new(name = "reblcalc", height = 702, width = 1163)
  app$set_inputs(`buttons-link_option` = FALSE)
  app$set_inputs(`buttons-import-data_source` = "example")
  app$set_inputs(`buttons-import-respondent_id` = "respondent_id")
  app$click("buttons-run_analysis")
  app$expect_values()
})
