test_that("Column name is correct", {
  path <- system.file("extdata", package = "tidyklips")
  df <- gethklips(path = path, year = 2023, datatype = "stata")
  expect_identical(colnames(df[2]), "province")
  expect_identical(colnames(df[3]), "income")
  expect_identical(colnames(df[4]), "year")
})

test_that("the dimenion of dataframe is correct", {
  path <- system.file("extdata", package = "tidyklips")
  df <- gethklips(path = path, year = 2023, datatype = "stata")
  expect_identical(dim(df), c(1000L, 4L))
})

test_that("wrong years are rejected", {
  path <- system.file("extdata", package = "tidyklips")
  expect_snapshot(
    error = TRUE,
    gethklips(path = path, year = 1997, datatype = "stata")
  )

  expect_snapshot(
    error = TRUE,
    gethklips(path = path, year = 2025:2026, datatype = "stata")
  )
})

