# library(geepack)

df <- mtcars

res <- lm(mpg ~ vs, data = df)
summary(res)
# res$coefficients
tidy(res)

out <- combineResults_noMod(res)
out





# x = colnames(df)
#
# tab1(df, x)
#
# tab1(df, c("vs", "am", "carb"))
#
#
# calculate_props(df, "am")
# calculate_props(df, "vs", decimals=3)
# calculate_props(df, vs, decimals=3)
#
#
# df$vs_str = as.character(df$vs)
# df$vs_fac = as.factor(df$vs)
# df$vs_int = as.integer(df$vs)
# df$vs_num = as.numeric(df$vs)
# calculate_props(df, vs_str, decimals=3)
# calculate_props(df, vs_fac, decimals=3)
# calculate_props(df, vs_int, decimals=3)
# calculate_props(df, vs_num, decimals=3)
#
# calculate_props(df, gear, decimals=3)
#
#
# df <- head(mtcars, 10)
# calculate_props(df, "vs", decimals=2)
# x <- calculate_props(df, "vs", decimals=3)
# x$pct_ci
#
#
#
# calculate_props(df, "vs", decimals=3)
# calculate_props(df, "vs", decimals=3, format_ci = TRUE)
#
#
#
#
#
#
#
#
# dtype(df$vs)
#
# typeof(df$vs)
#
#
# structure(df)
#
# class(df$vs)




# tab(df$mpg, c("mpg","cyl", "disp"))
# tab(999, c("mpg","cyl", "disp"))
#
# tab(df, 999)
# tab(df, "mpg", "cyl")
#
# tab(df, c("mpg", "cyl", "novar", "disp"))
#
#
#
# tab(df, "mpg" 'dog'))
#
#
# tab(df, "mpg"))

