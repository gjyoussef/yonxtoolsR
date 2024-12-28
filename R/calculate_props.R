# Suppress warnings about global variables that are dynamically created
utils::globalVariables(c("n_endorsed", "N_total", "outcome", "pct_loci", "pct_hici"))

#' Function to calculate proportions and confidence intervals
#'
#' This function calculates the proportion of a binary variable (coded as 0 and 1) in a dataframe
#' and optionally computes the confidence intervals for the proportion. The function checks that
#' the specified variable exists and is binary, and that its values are coded as 0, 1, or NA.
#'
#' @param df A dataframe containing the variable for which proportions and confidence intervals
#'           are calculated.
#' @param variable The name of the variable (column) in the dataframe, passed as a symbol or string.
#' @param decimals The number of decimal places to round the proportions and confidence intervals to. Default is 2.
#' @param format_ci A logical flag to determine whether to format the confidence intervals into a single
#'                  string (default is FALSE).
#'
#' @return A dataframe with the calculated proportion, and optionally the confidence intervals (as a string).
#' @export
#'
#' @examples
#' calculate_props(mtcars, vs, decimals = 3, format_ci = TRUE)
calculate_props <- function(df, variable, decimals=2, format_ci=FALSE) {

  variable <- rlang::ensym(variable) #Handle quoted/unquoted input

  ####### Validate inputs #######
  assertthat::assert_that(is.data.frame(df),
                          msg = "The 'df' argument must be a dataframe or tibble.")

  assertthat::assert_that(class(df[[rlang::as_string(variable)]]) %in% c("numeric", "integer"),
                          msg = "The variable must be numeric or integer.")

  assertthat::assert_that(all(df[[rlang::as_string(variable)]] %in% c(0, 1, NA)),
                          msg = "The variable must be binary coded as 0 or 1 (or NA).")

  ###### Calculate proportions and confidence intervals ######
  out <- df %>%
    dplyr::summarise(
      N_total = sum(!is.na(!!variable)),
      n_endorsed = sum(!!variable == 1, na.rm = TRUE),
      pct = mean(!!variable, na.rm = TRUE) * 100,

      #This below will only calculate the CIs if the cell sizes for the 1s AND 0s is > 5.
      #Otherwise the chi2 approximation is invalid. For those, it will return NA.
      pct_loci = ifelse((n_endorsed > 5) & (N_total-n_endorsed > 5), stats::prop.test(n_endorsed, N_total, conf.level = 0.95)$conf.int[1] * 100, NA),
      pct_hici = ifelse((n_endorsed > 5) & (N_total-n_endorsed > 5), stats::prop.test(n_endorsed, N_total, conf.level = 0.95)$conf.int[2] * 100, NA)
    ) %>%
    dplyr::mutate(dplyr::across(c('pct', 'pct_loci', 'pct_hici'), ~ format(round(., decimals), nsmall = decimals))) %>%
    dplyr::mutate(outcome = rlang::as_string(variable)) %>%
    dplyr::relocate(outcome, .before = 1)

  ###### Format Confidence Intervals if requested ######
  if (format_ci == TRUE) {
    out <- out %>%
      dplyr::mutate(pct_ci = paste0('(', pct_loci, ', ', pct_hici, ')')) %>%
      dplyr::select(!c(pct_loci, pct_hici))
  }

  return(out)
}
