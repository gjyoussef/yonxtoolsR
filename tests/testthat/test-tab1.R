# library(testthat)
# library(yonxtoolsR)

# Use mtcars dataset for testing
df <- mtcars

test_that("tab1 function prints correct tables for mtcars", {
  # Capture and snapshot the output
  expect_snapshot_output(tab1(df, c("mpg", "cyl", "disp")))
})

test_that("tab1 gives error if df is not a dataframe", {

  expect_error(tab1(df$mpg, c("mpg","cyl", "disp")), "The 'df' argument must be a dataframe or tibble.", fixed = TRUE)
  expect_error(tab1(999, c("mpg","cyl", "disp")), "The 'df' argument must be a dataframe or tibble.", fixed = TRUE)

})

test_that("tab1 gives error if vars argument is not a character vector", {

  expect_error(tab1(df, 999), "The 'vars' argument must be a character vector (see docs for examples).", fixed = TRUE)
})
