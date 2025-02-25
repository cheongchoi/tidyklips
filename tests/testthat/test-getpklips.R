test_that("Column name is correct", {
  path <- system.file("extdata", package = "tidyklips")
  df <- getpklips(path = path, year = 1998, datatype = "stata")
  expect_identical(colnames(df[1]), "pid")
  expect_identical(colnames(df[2]), "gender")
  expect_identical(colnames(df[3]), "age")
  expect_identical(colnames(df[4]), "year")
})

test_that("the dimenion of dataframe is correct", {
  path <- system.file("extdata", package = "tidyklips")
  df <- getpklips(path = path, year = 1998, datatype = "stata")
  expect_identical(dim(df), c(13319L, 4L))
})

test_that("wrong years are rejected", {
  path <- system.file("extdata", package = "tidyklips")
  expect_snapshot(
    error = TRUE,
    getpklips(path = path, year = 1997, datatype = "stata")
  )

  expect_snapshot(
    error = TRUE,
    getpklips(path = path, year = 2025:2026, datatype = "stata")
  )
})
