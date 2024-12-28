combineResults_noMod <- function(results, binary_exposure) {

  binary_exposure <- rlang::ensym(binary_exposure) #Handle quoted/unquoted input

  a <- getMarginalMeans_noMod(results, rlang::as_string(binary_exposure)) #pass through the binary_exposure as string
  b <- getDifferencesAbsolute_noMod(results, rlang::as_string(binary_exposure)) #pass through the binary_exposure as string
  c <- getDifferencesRelative_noMod(results, rlang::as_string(binary_exposure)) #pass through the binary_exposure as string

  merged <- merge(x=a,y=b, by.x=c("binary_exposure","subgroup", 'subgroup_level'), by.y=c("binary_exposure","subgroup", 'subgroup_level'))
  merged <- merge(x=merged,y=c, by.x=c("binary_exposure","subgroup", 'subgroup_level'), by.y=c("binary_exposure","subgroup", 'subgroup_level'))

  return(merged)
}


getMarginalMeans_noMod <- function(results, binary_exposure) {

  out <- marginaleffects::predictions(results, by = binary_exposure) #using defaults, which used empirical grid (See https://marginaleffects.com/chapters/predictions.html#empirical-grid)

  out <- as.data.frame(out)
  out$binary_exposure_name <- binary_exposure #tag the rows with the variable name string
  out$binary_exposure_levels <- out$binary_exposure #these will be the tags denoting whether the row is 0 or 1.
  out$subgroup <- 'none'
  out$subgroup_level <- 'none'

  out <- out %>% dplyr::select(binary_exposure_name, binary_exposure_levels, subgroup, subgroup_level, estimate, conf.low, conf.high)
  out <- data.frame(cbind(x[1, ], x[2, ])) %>%
    dplyr::select(binary_exposure, subgroup, subgroup_level,
                  estimate, conf.low, conf.high,
                  estimate.1, conf.low.1, conf.high.1)

  colnames(out) <- c("binary_exposure", "subgroup", "subgroup_level",
                     "mean_0", "loci_0", "hici_0",
                     "mean_1", "loci_1", "hici_1")
  return(out)
}


getDifferencesAbsolute_noMod <- function(results, binary_exposure) {

  out <- marginaleffects::avg_comparisons(results, variables = binary_exposure, comparison = "difference")

  out <- as.data.frame(out)
  out <- out %>%
    dplyr::mutate(binary_exposure = binary_exposure,
           subgroup = 'none',
           subgroup_level = 'none',
           estimate = estimate,
           conf.low = conf.low,
           conf.high = conf.high,
           p.value = p.value) %>%
    dplyr::select(binary_exposure, subgroup, subgroup_level, estimate, conf.low, conf.high, p.value)

  colnames(out) <- c('binary_exposure', 'subgroup', 'subgroup_level', 'abs_est', 'abs_loci', 'abs_hici', 'abs_pvalue')

  return(out)
}

getDifferencesRelative_noMod <- function(results, binary_exposure) {

  out <- marginaleffects::avg_comparisons(results, variables = binary_exposure, comparison = "ratio")

  out <- as.data.frame(out)
  out <- out %>%
    dplyr::mutate(binary_exposure = binary_exposure,
                  subgroup = 'none',
                  subgroup_level = 'none',
                  estimate = estimate - 1,
                  conf.low = conf.low - 1,
                  conf.high = conf.high - 1,
                  p.value = p.value) %>%
    dplyr::select(binary_exposure, subgroup, subgroup_level, estimate, conf.low, conf.high, p.value)

  colnames(out) <- c('binary_exposure', 'subgroup', 'subgroup_level', 'rel_est', 'rel_loci', 'rel_hici', 'rel_pvalue')

  return(out)
}
