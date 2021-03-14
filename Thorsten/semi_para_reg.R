rm(list = objects())
library(shiny)
source("Thorsten/ui.R")
source("Thorsten/scatter-smooth-app-master/server.R")

true_effect <- function(x) sin(2*(4*x-2))+2*exp(-256*(x-.5)^2)
x <- runif(150)
y <- rnorm(150, mean = true_effect(x), sd = 0.3)

plot(x, y)
lines(y, col = "red")

## Polynom Spline with truncated powers

tp <- function(z, degree = 3, knots = NULL, ...) {
  if(is.null(knots))
    knots <- seq(min(z), max(z), length = 10)
  if(length(knots) < 2)
    knots <- seq(min(z), max(z), length = knots)
  Z <- outer(z, 0:degree, "^"); cn <- paste("z^", 0:degree, sep = "")
  if(length(knots) > 2) {
    knots <- sort(unique(knots))
    for(j in 2:(length(knots) - 1)) {
      zk <- z - knots[j]
      check <- zk < 0
      zk <- zk^degree
      zk[check] <- 0
      Z <- cbind(Z, zk)
      cn <- c(cn, paste("(z-", round(knots[j], 2), ")^", degree, sep = ""))
    }
  }
  colnames(Z) <- cn
  return(Z)
}


#### Practical session ROOM 3
source("Thorsten/scatter-smooth-app-master/scatter-smooth-functions.R")

#true_effect <- function(x) sin(2*(4*x-2))+2*exp(-256*(x-.5)^2)
simdata <- function(n = 200, sd = .2, true_effect=function(x) sin(2*(4*x-2))+2*exp(-256*(x-.5)^2), seed = NULL){
  set.seed(seed) 
  x <- seq(0,1, length.out = n)
  y <- rnorm(n, mean = true_effect(x), sd = sd) 
  return(data.frame(x = x, y = y))
}

d <- simdata(seed=42)

scatter_smooth <- function(formula, data, true_effect=function(x) sin(2*(4*x-2))+2*exp(-256*(x-.5)^2)){
  mod <- lm(formula = formula, data = data)
  te <- true_effect(data$x)
  plot(y~x, data=data)
  lines(fitted(mod)~data$x, col = "red")
  lines(te~data$x, col = "green")

}

scatter_smooth(y~0+tp(x), data = d, true_effect = true_effect)
