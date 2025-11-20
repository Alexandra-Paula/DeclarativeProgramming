vectorVariance <- function(x) {
  n <- length(x)
  meanX <- mean(x)
  varResult <- sum((x - meanX)^2)/(n - 1)
  return(varResult)
}

vector <- c(1, 2, 3, 4)
vectorVariance(vector)
