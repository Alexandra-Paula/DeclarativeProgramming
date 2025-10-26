# ------------------------------------------------------------
# R Basics and Data Analysis Exercises
#
# This script contains several R exercises covering basic 
# programming and data analysis concepts.
#
# Contents:
#  Ex1 - Calculate the mean of a vector (4-decimal precision)
#  Ex2 - Factorial calculation using loops and built-in functions
#  Ex3 - Plot the sine function
#  Ex4 - Visualize the 'Orange' dataset
#  Ex5 - Plot the normal distribution
#  Ex6 - Plot a custom mathematical function
#  Ex7 - Data analysis of the 'cars' dataset using tidyverse
#        (export/import, statistics, filtering, mutation,
#         sorting, renaming, summarizing)
#
# Requirements:
#  - R version >= 4.0
#  - Packages: tidyverse
#
# How to run:
#  1. Open this script in RStudio or R console.
#  2. Install required packages if needed:
#       install.packages("tidyverse")
#  3. Run each exercise section separately.
# ------------------------------------------------------------

#Ex1 create a function that calculates the mean of a vector with a precision of four decimals
meanVector <- function(x) {
  n <- length(x)
  meanVal <- round(mean(x), 4)
  return(meanVal)
}
meanVector

vector <- c(1, 1, 1, 1)
meanVector(vector)

#Ex2 calculate factorial using a for loop
n <- 5
fact <- 1

for (i in 1:n) {
  fact <- fact * i
}

fact

#second method using prod()
n <- 5
fact <- prod(1:n)
fact

#Ex3 plot the sine function
x <- seq(0.5, 3*pi, length.out = 70)
y <- sin(x)
plot(x, y, type = "l", main="Graficul functiei sinus", col="red", lwd=15)

#Ex4 use the built-in 'Orange' dataset
data("Orange")
View(Orange)
OrangeAge <- Orange$age
OrangeCircumferince <- Orange$circumference
plot(OrangeAge, OrangeCircumferince, pch=25, col="green", ylab="Circumference", xlab="Age", main="A scatter plot of the age variable as a function of the circumference variable")

#Ex5 plot the normal distribution between -6 and 6
curve(dnorm(x, 0, 1), from=-6, to=6)
lim <- par("usr")  
segments(x0 = 0, y0 = lim[3], x1 = 0, y1 = dnorm(0, 0, 1), col = "black", lty = 2)
legend("topright", legend = "la loi normale entre -6 et 6", cex = 0.8)

#Ex6 plot the function x^5 + x^3 - 3x
f = curve(x^5+x^3-3*x,-2,2)
f
plot(f, type="l", lty=1, xlab="x", ylab="x^5+x^3-3*x")

#Ex7 data analysis of the 'cars' dataset using tidyverse.
install.packages("tidyverse")
library("tidyverse")

data("cars")
View(cars)
head(cars)

write.csv(cars, file = "cars.csv", row.names = FALSE)

cars_data <- read.csv("cars.csv")

head(cars_data, 15)
str(cars_data)
summary(cars_data)

mean(cars_data$speed, na.rm = TRUE)
median(cars_data$speed, na.rm = TRUE)
sd(cars_data$speed, na.rm = TRUE)
var(cars_data$speed, na.rm = TRUE)

cars_filtered <- cars_data %>% filter(speed > 15)
cars_filtered

cars_mutated <- cars_filtered %>% mutate(dist_m = dist * 0.3048)
cars_mutated

cars_arranged <- cars_mutated %>% arrange(dist)
cars_arranged

cars_transmuted <- cars_mutated %>%
  transmute(speed_mph = speed, dist_m = dist * 0.3048, dist_ft = dist)
cars_transmuted

cars_renamed <- cars_arranged %>% rename(speed_mph = speed)
cars_renamed

cars_selected <- cars_renamed %>% select(speed_mph, dist)
cars_selected

cars_distinct <- cars_selected %>% distinct()
cars_distinct

cars_summary <- cars_distinct %>%
  mutate(speed_group = ifelse(speed_mph < 15, "Mică", "Mare")) %>%
  group_by(speed_group) %>%
  summarise(
    medie_speed = mean(speed_mph),
    medie_dist = mean(dist),
    max_dist = max(dist)
  )
cars_summary


