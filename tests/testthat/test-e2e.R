library(testthat)
library(shinytest2)

test_that("app computes AU from uploaded file", {
  app <- AppDriver$new(app_dir = testthat::test_path("..", ".."))
  app$upload_file(file = testthat::test_path("fixtures", "sample_good.xlsx"))
  app$set_inputs(sheet = "Sheet1", unit = "Buffalo pasture", acreage = 100)
  app$wait_for_idle()
  expect_equal(app$get_value(output = "au_value"), "26.018")
  app$stop()
})

test_that("app defaults to first unit when Buffalo pasture missing", {
  app <- AppDriver$new(app_dir = testthat::test_path("..", ".."))
  app$upload_file(file = testthat::test_path("fixtures", "sample_altname.xlsx"))
  app$set_inputs(sheet = "Sheet1")
  app$wait_for_idle()
  expect_equal(app$get_value("unit"), "Other pasture")
  app$stop()
})
