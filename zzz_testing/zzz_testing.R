df <- head(mtcars, 10)


colnames(df)

tab(df, c("mpg","cyl", "disp"))


tab(df$mpg, c("mpg","cyl", "disp"))
tab(999, c("mpg","cyl", "disp"))

tab(df, 999)
tab(df, "mpg", "cyl")

tab(df, c("mpg", "cyl", "novar", "disp"))



tab(df, "mpg" 'dog'))


tab(df, "mpg"))

