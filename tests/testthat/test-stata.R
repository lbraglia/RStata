test_that("string cmd input works", {
  expect_snapshot(stata("sum", mtcars))
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
