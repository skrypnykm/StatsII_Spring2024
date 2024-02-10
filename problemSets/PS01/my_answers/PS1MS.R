#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

kosmogorov <- function(data) {
  ECDF <- ecdf(data)  #Empirical distribution
  empiricalCDF <- ECDF(data)
  D <- max(abs(empiricalCDF - pnorm(data)))  #Test statistic (reference distribution is normal)
  n <- length(data) #Number of observations
  k <- 1:n #Indices 
  dsum <- sum(exp(-(2*k-1)^2*pi^2/(8*D^2))) #Sum of exponential terms for the p-value
  p_value <- sqrt(2*pi)/D*dsum #Final p-value
  return(list(D = D, p_value = p_value))
}

set.seed(123)
variables <- rcauchy(1000, location = 0, scale = 1)

kosmogorov(variables)

ks.test(variables, 'pnorm')

#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

ols <- function(beta, x, y) {
  y_hat <- beta[1] + beta[2] * x #Predicted values of the DV
  res <- y - y_hat #Residuals
  SSE <- (sum(res^2)) #Sum of squared errors
  return(SSE)
}

ols_bfgs <- optim(par = c(0, 3), 
                  ols, 
                  x = data$x, 
                  y = data$y, 
                  method = "BFGS")
print(ols_bfgs$par)

lm1<- lm(data$y ~ data$x)
print(lm1)

linear.lik <- function(theta, y, X) {
  n <- nrow(X) #Number of observations
  k <- ncol(X) #Number of predictors
  beta <- theta[1:k] #Coefficients for predictors
  sigma2 <- theta[k + 1]^2 #Variance
  e <- y - X %*% beta #Residuals
  logl <- -0.5*n*log(2*pi)-0.5*n*log(sigma2)-((t(e) %*% e)/(2*sigma2)) #Log-likelihood function 
  return(-logl) #Negative log-likelihood
}

linear.MLE <- optim(fn = linear.lik, 
                    par = c(0, 3, 1), 
                    hessian = TRUE, 
                    y = data$y, 
                    X = cbind(1, data$x), 
                    method = "BFGS")
print(linear.MLE$par)

