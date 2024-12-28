# Function to calculate proportions and confidence intervals
#' Title
#'
#' @param df
#' @param variable
#' @param decimals
#'
#' @return
#' @export
#'
#' @examples
calculate_props <- function(df, variable, decimals=2, format_ci=FALSE) {

  variable <- rlang::ensym(variable) # Handle quoted/unquoted input

  # # Check that df is a data frame
  # if (!is.data.frame(df)) {
  #   stop("The 'df' argument must be a dataframe or tibble.")
  # }
  #
  # # Check that df$variable is a binary variable, coded as numeric 0 and 1
  # if (!(class(df[[rlang::as_string(variable)]]) %in% c("numeric", "integer"))) {
  #   stop("The variable must be binary and numeric, coded with 0 and 1.")
  # }
  #
  # # Check that df$variable is a binary variable, coded as 0, 1, or NA
  # if (!all(df[[rlang::as_string(variable)]] %in% c(0, 1, NA))) {
  #   stop("The variable must be coded with 0, 1, or NA.")
  # }

  # Validate input arguments
  assertthat::assert_that(is.data.frame(df), msg = "The 'df' argument must be a dataframe or tibble.")
  assertthat::assert_that(
    class(df[[rlang::as_string(variable)]]) %in% c("numeric", "integer"),
    msg = "The variable must be binary and numeric, coded with 0 and 1."
  )
  assertthat::assert_that(
    all(df[[rlang::as_string(variable)]] %in% c(0, 1, NA)),
    msg = "The variable must be coded with 0, 1, or NA."
  )


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
    dplyr::mutate(across(c('pct', 'pct_loci', 'pct_hici'), ~ format(round(., decimals), nsmall = decimals)))


  if (format_ci == TRUE) {
    out <- out %>%
      dplyr::mutate(pct_ci = paste0('(', pct_loci, ', ', pct_hici, ')')) %>%
      dplyr::select(!c(pct_loci, pct_hici))
  }



    # dplyr::mutate(pct_ci = paste0('(', pct_loci, ', ', pct_hici, ')'))
  # %>%
    # dplyr::rename(pct = prop,
           # pct_loci = loci_p,
           # pct_hici = hici_p)
  # %>%
    # dplyr::select(!c(pct_loci, pct_hici))

  # out <- df %>%
  #   dplyr::summarise(
  #     N_total = sum(!is.na(!!variable)),
  #     n_endorsed = sum(!!variable == 1, na.rm = TRUE),
  #     prop = mean(!!variable, na.rm = TRUE),
  #
  #     #This below will only calculate the CIs if the cell sizes for the 1s AND 0s is > 5.
  #     #Otherwise the chi2 approximation is invalid. For those, it will return NA.
  #     loci_p = ifelse((n_endorsed > 5) & (N_total-n_endorsed > 5), stats::prop.test(n_endorsed, N_total, conf.level = 0.95)$conf.int[1], NA),
  #     hici_p = ifelse((n_endorsed > 5) & (N_total-n_endorsed > 5), stats::prop.test(n_endorsed, N_total, conf.level = 0.95)$conf.int[2], NA)
  #   ) %>%
  #   dplyr::mutate(across(c('prop', 'loci_p', 'hici_p'), ~ round(. * 100, decimals))) %>%
  #   dplyr::mutate(pct_ci = paste0('(', loci_p, ', ', hici_p, ')')) %>%
  #   dplyr::rename(pct = prop,
  #                 pct_loci = loci_p,
  #                 pct_hici = hici_p)
  # # %>%
  # # dplyr::select(!c(pct_loci, pct_hici))



  return(out)
}
