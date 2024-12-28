#' Prints frequency tables for specified variables in a data frame.
#'
#' Uses approach that is similar to Stata's 'tab1'
#'
#' @param df A dataframe or tibble containing the variables to tabulate.
#' @param vars A character vector specifying the names of the variables in the data frame.
#'
#' @return This function prints the frequency tables for the specified variables.
#' @export
#'
#' @examples
#' tab1(mtcars, "mpg") # Use for one variable.
#' tab1(mtcars, c("mpg", "cyl", "disp")) # Use a character vector for multiple.

tab1 <- function(df, vars) {

  ####### Validate inputs #######
  assertthat::assert_that(is.data.frame(df),
                          msg = "The 'df' argument must be a dataframe or tibble.")

  assertthat::assert_that(is.character(vars),
                          msg = "The 'vars' argument must be a character vector (see docs for examples).")

  ####### Tabulate variables #######
  for (i in vars) {
    if (i %in% colnames(df)) {
      print(i)
      print(janitor::tabyl(df[[i]]))
      message("\n") # Add spacing between tables
    }
    else {
      warning(paste("Variable", i, "was not found in the dataframe, skipped")) # Warn if variable is not found
    }
  }
}
