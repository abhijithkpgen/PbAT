# tests/testthat/test_eda_module.R

test_that("EDA UI constructs", {
  ui <- analysisUI("eda_test")
  expect_s3_class(ui, c("shiny.tag", "shiny.tag.list"), exact = FALSE)
})

test_that("Home UI constructs", {
  ui_home <- homeUI("home_test")
  expect_s3_class(ui_home, c("shiny.tag", "shiny.tag.list"), exact = FALSE)
})

test_that("analysisServer initializes with reactive home_inputs", {
  old <- options(shiny.testmode = TRUE); on.exit(options(old), add = TRUE)

  # minimal reactive that mimics homeServer() output
  home_inputs <- shiny::reactiveVal(NULL)

  # initialize once with NULL (no click yet)
  expect_no_error(
    suppressWarnings(suppressMessages(
      shiny::testServer(
        app = analysisServer,
        args = list(id = "eda_test", home_inputs = home_inputs),
        { TRUE }
      )
    ))
  )

  # now mimic a click from Home: provide analysis_mode
  home_inputs(list(analysis_mode = "eda"))

  expect_no_error(
    suppressWarnings(suppressMessages(
      shiny::testServer(
        app = analysisServer,
        args = list(id = "eda_test", home_inputs = home_inputs),
        { TRUE }
      )
    ))
  )
})

test_that("run_app returns a shiny app object", {
  old <- options(shiny.testmode = TRUE); on.exit(options(old), add = TRUE)
  app <- run_app()  # no namespace needed inside package tests
  expect_s3_class(app, "shiny.appobj")
})
