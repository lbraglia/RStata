test_that("string cmd input works", {
  expect_snapshot(stata("sum", mtcars))
})

test_that("version provided as an argument works", {
  version = as.numeric(Sys.getenv("STATA_VERSION"))
  expect_snapshot(
    stata("sum", mtcars, stata.version = version)
  )
})

test_that("path provided as an argument works", {
  path = Sys.getenv("STATA_PATH")
  expect_snapshot(
    stata("sum", mtcars, stata.path = path)
  )
})

test_that("string cmd input works with data.out", {
  expect_snapshot(stata("sum", mtcars, data.out = TRUE))
  
  expect_snapshot_value(
    stata("sum", mtcars, data.out = TRUE),
    style = "json2"
  )
})

test_that("do file input works", {
  expect_snapshot(stata(testthat::test_path("fixtures/test.do"), mtcars))
})

test_that("do file input works with data.out", {
  expect_snapshot(
    stata(testthat::test_path("fixtures/test.do"), mtcars, data.out = TRUE)
  )
  
  expect_snapshot_value(
    stata(testthat::test_path("fixtures/test.do"), mtcars, data.out = TRUE),
    style = "json2"
  )
})
