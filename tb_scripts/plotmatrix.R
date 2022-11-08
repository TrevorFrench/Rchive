head(mtcars)

pairs(mtcars, pch = 20, lower.panel = NULL, xaxt = "n", yaxt="n", col = "#FC4E07")

library("corrplot")
corrplot(cor(mtcars), method="number")

library(stats)

model <- lm(mpg~drat, data=mtcars)
acf(model$residuals, type="correlation")

df <- read.csv('autocorrelation.csv')

model <- lm(Independent~Dependent, data=df)
acf(model$residuals, type="correlation")