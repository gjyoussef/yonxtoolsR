# Use mtcars dataset for testing
df <- mtcars

test_that("function prints correct proportions for mtcars$vs", {
  # Capture and snapshot the output
  expect_snapshot_output(calculate_props(df, "vs"))
  expect_snapshot_output(calculate_props(df, vs))
  expect_snapshot_output(calculate_props(df, vs, decimals=0))
  expect_snapshot_output(calculate_props(df, vs, decimals=3))
  expect_snapshot_output(calculate_props(df, vs, decimals=1, format_ci=TRUE))
})

test_that("function gives error if df is not a dataframe", {
  expect_error(calculate_props(df$vs), "The 'df' argument must be a dataframe or tibble.", fixed = TRUE)
  expect_error(calculate_props(999, vs), "The 'df' argument must be a dataframe or tibble.", fixed = TRUE)
})

test_that("function gives error if variable is not numeric or integer", {
  df$vs_str = as.character(df$vs)
  df$vs_fac = as.factor(df$vs)
  expect_error(calculate_props(df, vs_str), "The variable must be numeric or integer.", fixed = TRUE)
  expect_error(calculate_props(df, vs_fac), "The variable must be numeric or integer.", fixed = TRUE)
})

test_that("function gives error if variable is not binary coded as 0, 1, or NA", {
  expect_error(calculate_props(df, cyl), "The variable must be binary coded as 0 or 1 (or NA).", fixed = TRUE)
  expect_error(calculate_props(df, drat), "The variable must be binary coded as 0 or 1 (or NA).", fixed = TRUE)
})
