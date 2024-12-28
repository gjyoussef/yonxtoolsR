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
calculate_props <- function(df, variable, decimals=2) {

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
      prop = mean(!!variable, na.rm = TRUE),
      # loci_p = ifelse(N_total > 0, stats::prop.test(n_endorsed, N_total, conf.level = 0.95)$conf.int[1], NA),
      # hici_p = ifelse(N_total > 0, stats::prop.test(n_endorsed, N_total, conf.level = 0.95)$conf.int[2], NA),
      loci_p = ifelse((n_endorsed > 5) & (N_total-n_endorsed > 5), stats::prop.test(n_endorsed, N_total, conf.level = 0.95)$conf.int[1], NA),
      hici_p = ifelse((n_endorsed > 5) & (N_total-n_endorsed > 5), stats::prop.test(n_endorsed, N_total, conf.level = 0.95)$conf.int[2], NA)
    ) %>%
    dplyr::mutate(across(c('prop', 'loci_p', 'hici_p'), ~ round(. * 100, decimals))) %>%
    dplyr::mutate(pct_ci = paste0('(', loci_p, ', ', hici_p, ')')) %>%
    dplyr::rename(pct = prop,
           pct_loci = loci_p,
           pct_hici = hici_p) %>%
    dplyr::select(!c(pct_loci, pct_hici))

  return(out)
}
