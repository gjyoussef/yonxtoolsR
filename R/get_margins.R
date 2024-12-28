combineResults_noMod <- function(results, binary_exposure) {

  binary_exposure <- rlang::ensym(binary_exposure) #Handle quoted/unquoted input

  # means <- predictions(res, by = "time", newdata = datagrid(grid_type = "balanced"))
  means <- marginaleffects::predictions(results, by = rlang::as_string(binary_exposure)) #using this which defaults to using the empirical grid (which is marginalising over the observed data)
  # means <- marginal_means(res, variables = c('time'), cross = TRUE)
  abs<-marginaleffects::avg_comparisons(results, variables = rlang::as_string(binary_exposure), comparison = "difference")
  rel<-marginaleffects::avg_comparisons(results, variables = rlang::as_string(binary_exposure), comparison = "ratio")

  # # a<-getMarginalMeans(means)
  # a<-getMarginalMeans_noMod(means)
  # b<-getDifferences_absolute(abs)
  # c<-getDifferences_relative(rel)
  #
  # merged <- merge(x=a,y=b, by.x=c("variable","subgroup", 'subgroup_level'), by.y=c("variable","subgroup", 'subgroup_level'))
  # merged <- merge(x=merged,y=c, by.x=c("variable","subgroup", 'subgroup_level'), by.y=c("variable","subgroup", 'subgroup_level'))

  print(means)
  print(abs)
  print(rel)

  # return(merged)
}



# 5.2.1 Empirical grid # By default, the predictions() function uses the full original dataset as a grid, that is, it uses the empirical distribution of predictors (Section 3.2.1). This means that predictions() will compute fitted values for each of the rows in the dataset used to fit the model.
