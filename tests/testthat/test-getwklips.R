test_that("Column name is correct", {
  path <- system.file("extdata", package = "tidyklips")
  df <- getwklips(path = path, datatype = "stata")
  expect_identical(colnames(df[1]), "pid")
  expect_identical(colnames(df[2]), "jobseq")
  expect_identical(colnames(df[3]), "jobtype")
})

test_that("the dimenion of dataframe is correct", {
  path <- system.file("extdata", package = "tidyklips")
  df <- getwklips(path = path, datatype = "stata")
  expect_identical(dim(df), c(1000L, 3L))
})


