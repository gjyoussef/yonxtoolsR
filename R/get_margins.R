combineResults_noMod <- function(results) {

  res <- results

  # means <- predictions(res, by = "time", newdata = datagrid(grid_type = "balanced"))
  means <- predictions(res, by = "time", newdata = data) #using this because I think it will do the closest to stata. i.e. it will get the predicted values for every possible combination in the actual data and then average over those for the by statement.
  # means <- marginal_means(res, variables = c('time'), cross = TRUE)
  # abs<-avg_comparisons(res, variables = "time", comparison = "difference")
  # rel<-avg_comparisons(res, variables = "time", comparison = "ratio")

  # # a<-getMarginalMeans(means)
  # a<-getMarginalMeans_noMod(means)
  # b<-getDifferences_absolute(abs)
  # c<-getDifferences_relative(rel)
  #
  # merged <- merge(x=a,y=b, by.x=c("variable","subgroup", 'subgroup_level'), by.y=c("variable","subgroup", 'subgroup_level'))
  # merged <- merge(x=merged,y=c, by.x=c("variable","subgroup", 'subgroup_level'), by.y=c("variable","subgroup", 'subgroup_level'))

  print(means)

  # return(merged)
}
