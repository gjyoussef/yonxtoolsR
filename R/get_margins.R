combineResults_noMod <- function(results, binary_exposure) {

  binary_exposure <- rlang::ensym(binary_exposure) #Handle quoted/unquoted input

  # means <- predictions(res, by = "time", newdata = datagrid(grid_type = "balanced"))
  # means <- marginaleffects::predictions(results, by = rlang::as_string(binary_exposure)) #using this which defaults to using the empirical grid (which is marginalising over the observed data)
  # # means <- marginal_means(res, variables = c('time'), cross = TRUE)
  # abs<-marginaleffects::avg_comparisons(results, variables = rlang::as_string(binary_exposure), comparison = "difference")
  # rel<-marginaleffects::avg_comparisons(results, variables = rlang::as_string(binary_exposure), comparison = "ratio")

  a <- getMarginalmeans_noMod(results, rlang::as_string(binary_exposure)) #pass through the binary_exposure as sym()
  # a<-getMarginalMeans_noMod(means)
  # b<-getDifferences_absolute(abs)
  # c<-getDifferences_relative(rel)
  #
  # merged <- merge(x=a,y=b, by.x=c("variable","subgroup", 'subgroup_level'), by.y=c("variable","subgroup", 'subgroup_level'))
  # merged <- merge(x=merged,y=c, by.x=c("variable","subgroup", 'subgroup_level'), by.y=c("variable","subgroup", 'subgroup_level'))

  # print(means)
  # print(abs)
  # print(rel)

  # return(merged)
  return(a)
}


getMarginalmeans_noMod <- function(results, binary_exposure) {

  # binary_exposure <- rlang::ensym(binary_exposure) #Handle quoted/unquoted input

  out <- marginaleffects::predictions(results, by = binary_exposure) #using this which defaults to using the empirical grid (which is marginalising over the observed data)

  out <- as.data.frame(out)

  out$binary_exposure_name <- binary_exposure
  out$binary_exposure_levels <- out$binary_exposure
  out$subgroup <- 'none'
  out$subgroup_level <- 'none'

  out <- out %>% dplyr::select(binary_exposure_name, binary_exposure_levels, subgroup, subgroup_level, estimate, conf.low, conf.high)
  out <- data.frame(cbind(x[1, ], x[2, ])) %>%
    dplyr::select(binary_exposure, subgroup, subgroup_level,
           estimate, conf.low, conf.high,
           estimate.1, conf.low.1, conf.high.1)
  rownames(out) <- NULL
  colnames(out) <- c("binary_exposure", "subgroup", "subgroup_level", "mean_0", "loci_0", "hici_0", "mean_1", "loci_1", "hici_1")
  return(out)
}



# 5.2.1 Empirical grid # By default, the predictions() function uses the full original dataset as a grid, that is, it uses the empirical distribution of predictors (Section 3.2.1). This means that predictions() will compute fitted values for each of the rows in the dataset used to fit the model.
