box::use(
  testthat[test_that, expect_s3_class, expect_true],
  app/logic/show_placeholder[show_placeholder]
)

test_that("show_placeholder returns shiny tag list", {
  result <- show_placeholder()
  expect_s3_class(result, "shiny.tag.list")
})

test_that("show_placeholder accepts custom message", {
  result <- show_placeholder("Custom message")
  expect_s3_class(result, "shiny.tag.list")
})
