tab <- function(df, vars) {
  # # Check that vars are in df's column names
  # missing_vars <- setdiff(vars, colnames(df))
  # if (length(missing_vars) > 0) {
  #   warning("The following variables are not found in the data frame: ", paste(missing_vars, collapse = ", "))
  # }

  # Iterate through the specified variables and print frequency tables
  for (i in vars) {
    print(i)
    print(janitor::tabyl(df[[i]]))  # tabyl comes from janitor
    cat("\n\n")
  }
}


#CO2
#mtcars
