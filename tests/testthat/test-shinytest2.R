library(shinytest2)

test_that("{shinytest2} recording: analysis runs with example data", {
  app <- AppDriver$new(variant = platform_variant(), name = "REBLcalc", height = 702,
      width = 1163)
  app$set_inputs(`buttons-link_option` = FALSE)
  app$set_inputs(`buttons-import-data_source` = "example")
  app$set_inputs(`buttons-import-respondent_id` = "respondent_id")
  app$click("buttons-run_analysis")
  app$wait_for_idle()
  app$expect_screenshot()
})

test_that("{shinytest2} recording: analysis with linked scores, item fit page", {
  app <- AppDriver$new(variant = platform_variant(), name = "analysis with linked scores, item fit page",
      height = 702, width = 1163)
  app$set_inputs(`buttons-link_option` = FALSE)
  app$set_inputs(`buttons-import-data_source` = "example")
  app$set_inputs(`buttons-import-respondent_id` = "respondent_id")
  app$set_inputs(`buttons-link_option` = TRUE)
  app$click("buttons-run_analysis")
  app$set_inputs(tabset = "Person Fit")
  app$wait_for_idle()
  app$expect_screenshot()
})
