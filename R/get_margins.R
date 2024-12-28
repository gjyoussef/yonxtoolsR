#' Title
#'
#' @param results
#' @param binary_exposure
#' @param decimals
#' @param decimals_pvalue
#' @param outcome_is_binary
#'
#' @return
#' @export
#'
#' @examples
combineResults_noMod <- function(results, binary_exposure, decimals=2, decimals_pvalue=4, outcome_is_binary=FALSE) {

  binary_exposure <- rlang::ensym(binary_exposure) #Handle quoted/unquoted input

  a <- getMarginalMeans_noMod(results, rlang::as_string(binary_exposure)) #pass through the binary_exposure as string
  b <- getDifferencesAbsolute_noMod(results, rlang::as_string(binary_exposure)) #pass through the binary_exposure as string
  c <- getDifferencesRelative_noMod(results, rlang::as_string(binary_exposure)) #pass through the binary_exposure as string

  out <- merge(x=a,y=b, by.x=c("binary_exposure","subgroup", 'subgroup_level'), by.y=c("binary_exposure","subgroup", 'subgroup_level'))
  out <- merge(x=out,y=c, by.x=c("binary_exposure","subgroup", 'subgroup_level'), by.y=c("binary_exposure","subgroup", 'subgroup_level'))

  #Convert to percentages if the outcome is binary
  if (outcome_is_binary == TRUE) {
    out <- out %>%
      dplyr::mutate(dplyr::across(c('mean_0', 'loci_0', 'hici_0',
                                    'mean_1', 'loci_1', 'hici_1',
                                    'abs_est', 'abs_loci', 'abs_hici',
                                    'rel_est', 'rel_loci', 'rel_hici'), ~ . * 100))
  }

  #Format decimals
  out <- out %>%
    dplyr::mutate(dplyr::across(c('mean_0', 'loci_0', 'hici_0',
                                  'mean_1', 'loci_1', 'hici_1',
                                  'abs_est', 'abs_loci', 'abs_hici',
                                  'rel_est', 'rel_loci', 'rel_hici'), ~ format(round(., decimals), nsmall = decimals))) %>%
    dplyr::mutate(dplyr::across(c('abs_pvalue', 'rel_pvalue'), ~ format(round(., decimals_pvalue), nsmall = decimals_pvalue)))

  return(out)
}


#' Title
#'
#' @param results
#' @param binary_exposure
#'
#' @return
#' @export
#'
#' @examples
getMarginalMeans_noMod <- function(results, binary_exposure) {

  out <- marginaleffects::predictions(results, by = binary_exposure) #using defaults, which used empirical grid (See https://marginaleffects.com/chapters/predictions.html#empirical-grid)

  out <- as.data.frame(out)
  out <- out %>%
    dplyr::mutate(binary_exposure_name = binary_exposure,
                  binary_exposure_levels = out[[binary_exposure]],
                  subgroup = 'none',
                  subgroup_level = 'none'
                  ) %>%
    dplyr::select(binary_exposure_name, binary_exposure_levels, subgroup, subgroup_level, estimate, conf.low, conf.high)

  out_row0 <- out[1, c('binary_exposure_name', 'subgroup', 'subgroup_level', 'estimate', 'conf.low', 'conf.high')] %>%
    dplyr::rename(binary_exposure = binary_exposure_name,
                  mean_0 = estimate,
                  loci_0 = conf.low,
                  hici_0 = conf.high
    )

  out_row1 <- out[2, c('estimate', 'conf.low', 'conf.high')] %>%
    dplyr::rename(mean_1 = estimate,
                  loci_1 = conf.low,
                  hici_1 = conf.high
    )

  out <- dplyr::bind_cols(out_row0, out_row1)

  return(out)
}


#' Title
#'
#' @param results
#' @param binary_exposure
#'
#' @return
#' @export
#'
#' @examples
getDifferencesAbsolute_noMod <- function(results, binary_exposure) {

  out <- marginaleffects::avg_comparisons(results, variables = binary_exposure, comparison = "difference")

  out <- as.data.frame(out)
  out <- out %>%
    dplyr::mutate(binary_exposure = binary_exposure,
           subgroup = 'none',
           subgroup_level = 'none',
           abs_est = estimate,
           abs_loci = conf.low,
           abs_hici = conf.high,
           abs_pvalue = p.value) %>%
    dplyr::select(binary_exposure, subgroup, subgroup_level, abs_est, abs_loci, abs_hici, abs_pvalue)

  return(out)
}


#' Title
#'
#' @param results
#' @param binary_exposure
#'
#' @return
#' @export
#'
#' @examples
getDifferencesRelative_noMod <- function(results, binary_exposure) {

  out <- marginaleffects::avg_comparisons(results, variables = binary_exposure, comparison = "ratio")

  out <- as.data.frame(out)
  out <- out %>%
    dplyr::mutate(binary_exposure = binary_exposure,
                  subgroup = 'none',
                  subgroup_level = 'none',
                  rel_est = estimate - 1,
                  rel_loci = conf.low - 1,
                  rel_hici = conf.high - 1,
                  rel_pvalue = p.value) %>%
    dplyr::select(binary_exposure, subgroup, subgroup_level, rel_est, rel_loci, rel_hici, rel_pvalue)

  return(out)
}
