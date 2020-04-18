# READS THE DATA INTO OUR ENVIRONMENT
dataOne = read.csv("data.csv", header = TRUE, stringsAsFactors = FALSE)
# CREATES VARIABLE NAMES FOR EASY REFERENCE
Year <- dataOne$Year
Population <- dataOne$Population
# PRINTS THE CLASSES OF OUR INDEPENDENT AND DEPENDENT VARIABLES
print(class(Population))
print(class(Year))
# DISPLAYS THE FIRST TEN ROWS OF OUR CSV FILE
head(dataOne, 10)
# CREATES A LINEAR REGRESSION MODEL TITLED "lm1"
lm1 <- lm(Population~Year)
# SUMMARIZES lm1
summary(lm1)
lm1$coefficients
# This predicts July 1, 2024
newData = data.frame(Year = 14.25)
predict(lm1, newData, interval = "predict")
# PLOTS lm1
plot(Population ~ Year,
     xlab = "Years Since Base Year",
     ylab = "Population",
     main = "Washington State Population"
)

abline(lm1, col = "red") # (lwd will adjust the thickness of the abline)
