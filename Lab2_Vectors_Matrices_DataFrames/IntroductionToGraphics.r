#rnorm Normal distributions on special spaces
# rnorm() is used for generating a vector of  random numbers with a normal distribution
#pch=18 

x <- rnorm(10)
y <- rnorm(10)
plot(x,y)
plot(x, y, xlab="Ten random values", ylab="Ten other values", 
     xlim=c(-2,2), ylim=c(-2, 2), pch=22, col="red", bg="orange",
     bty="l", main="How to customize a plot with R")

#ex2
Temperature <- airquality$Temp
hist(Temperature)
plot(Temperature, type="l", lty=1)

#histogram with added parameters
hist(Temperature, main="Maximum daily temperature at La Guardia Airport",
     xlab="Temperature in degrees Fahrenheit",
     xlim=c(50, 100),
     col="darkmagenta",
     freq=FALSE
     )

#ex3
x = c(-2:2)
f = x^5+x^3*x
f
plot(f)
plot(f, type="l", lty=1)

#"p” for points
# “l” for lines
# “b” for both points and lines
# “c” for empty points joined by lines
# “o” for overplotted points and lines
# “s” and “S” for stair steps
# “n” does not produce any points or lines

#ex3.1
x = c(-2:2)
f = curve(x^5+x^3-3*x,-2,2)
plot(f)
plot(f, type="l", lty=1)