library(testthat)
library(shiny)

test_that("au_value_num computes expected value", {
  testServer(server, {
    path <- testthat::test_path("fixtures", "sample_good.xlsx")
    session$setInputs(file = list(datapath = path, name = "sample_good.xlsx"))
    session$setInputs(sheet = "Sheet1")
    session$flushReact()
    session$setInputs(unit = "Buffalo pasture")
    session$setInputs(acreage = 100)
    session$setInputs(intake = "10950")
    session$flushReact()
    expect_equal(au_value_num(), 26.018073059, tolerance = 1e-3)
  })
})

test_that("missing dry weight column triggers validation", {
  testServer(server, {
    path <- testthat::test_path("fixtures", "sample_missing_dry.xlsx")
    session$setInputs(file = list(datapath = path, name = "sample_missing_dry.xlsx"))
    session$setInputs(sheet = "Sheet1")
    session$flushReact()
    expect_error(dat(), "Missing 'Dry wegith'")
  })
})
