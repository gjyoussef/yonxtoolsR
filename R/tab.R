#' Prints frequency tables for specified variables in a data frame.
#'
#' Uses approach that is similar to Stata's 'tab1'
#'
#' @param df A data frame containing the variables to tabulate.
#' @param vars A character vector specifying the names of the variables in the data frame.
#'
#' @return This function prints the frequency tables for the specified variables.
#' @export
#'
#' @examples
#' df <- mtcars
#' tab(df, "mpg") # Use for one variable.
#' tab(df, c("mpg", "cyl", "disp")) # Use a character vector for multiple.
#'
tab <- function(df, vars) {

  # Check that df is a data frame
  if (!is.data.frame(df)) {
    stop("The 'df' argument must be a data frame.")
  }

  # Check that vars is a character vector
  if (!is.character(vars)) {
    stop("The 'vars' argument must be a character vector (see docs for examples).")
  }

  # Check that vars are in df's column names
  missing_vars <- setdiff(vars, colnames(df))
  if (length(missing_vars) > 0) {
    warning("The following variables are not found in the data frame: ",
            paste(missing_vars, collapse = ", "))
  }

  # Iterate through the specified variables and print frequency tables
  for (i in vars) {
    print(i)
    print(janitor::tabyl(df[[i]]))  # tabyl comes from janitor
    cat("\n\n")
  }
}
