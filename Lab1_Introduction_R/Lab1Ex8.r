data("airquality")
View(airquality)
summary(airquality)
#media
mean(airquality$Temp)
#mediana
median(airquality$Temp)
#standard deviation, abaterea
sd(airquality$Temp)
#varianta
var(airquality)
#function for abaterea standard
standardDeviation <- function(x) {
  sqrt(var(x))
}
standardDeviation(airquality$Temp)
standardDeviation(airquality)

#extract 2th row
airquality[2, ]

#3th column
airquality[, 3]
#1,2,4 rows with c
airquality[c(1, 2, 4),]

#rows 2-6 with ":"
airquality[2:6, ]

#toate, dar nu coloanele 1, 2
airquality[, c(-1, -2)]

#linii cu temp > 90
airquality[airquality$Temp > 90,]


