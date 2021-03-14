# ---------------------------------------------
# Name:        scatter-smooth-functions.R
# Author:      Thorsten Simon, Nikolaus Umlauf
# Date:        2020-03-26
# Description: Work horses for app.
# License:     CC-BY
# ---------------------------------------------

## Evaluate a B-spline design matrix
## first, define the B-spline basis function
## recursively.
bsbasis <- function(z, knots, j, degree) {
    if(degree == 0)
        B <- 1 * (knots[j] <= z & z < knots[j + 1])
    if(degree > 0) {
        b1 <- (z - knots[j]) / (knots[j + degree] - knots[j])
        b2 <- (knots[j + degree + 1] - z) /
            (knots[j + degree + 1] - knots[j + 1])
        B <- b1 * bsbasis(z, knots, j, degree - 1) +
            b2 * bsbasis(z, knots, j + 1, degree - 1)
    }
    B[is.na(B)] <- 0
    return(B)
}


## Now, compute the design matrix for all knots.
bsDesign <- function(z, degree = 3, knots = NULL) {
    ## Compute knots.
    if(is.null(knots))
        knots <- 40
    if(length(knots) < 2) {
        step <- (max(z) - min(z)) / (knots - 1)
        knots <- seq(min(z) - degree * step,
            max(z) + degree * step, by = step)
    }

    ## Evaluate each basis function
    ## and return the full design matrix B.
    B <- NULL
    for(j in 1:(length(knots) - degree - 1))
        B <- cbind(B, bsbasis(z, knots, j, degree))
    return(B)
}


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

#true_effect <- function(x) sin(2*(4*x-2))+2*exp(-256*(x-.5)^2)
simdata <- function(n = 200, sd = .2, true_effect=function(x) sin(2*(4*x-2))+2*exp(-256*(x-.5)^2), seed = NULL, equidist = T){
  set.seed(seed) 
  if(equidist==T){
    x <- seq(0,1, length.out = n)
    y <- rnorm(n, mean = true_effect(x), sd = sd)
    
  }else{
    x <- sort(runif(n, 0,1))
    y <- rnorm(n, mean = true_effect(x), sd = sd)
    
  }
   
  return(data.frame(x = x, y = y))
}



scatter_smooth <- function(formula, data, true_effect=function(x) sin(2*(4*x-2))+2*exp(-256*(x-.5)^2)){
  mod <- lm(formula = formula, data = data)
  te <- true_effect(data$x)
  plot(y~x, data=data)
  lines(fitted(mod)~data$x, col = "red")
  lines(te~data$x, col = "green")
  
}



