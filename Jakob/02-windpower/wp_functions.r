quantile_score <- function(error, p){
  score <- ifelse(error<0, abs(error)*(1-p), abs(error)*p)
  mean(score)
}

mle <- function(y, X, Z){
  loglik <- function(theta) {
    beta <- theta[1:ncol(X)]
    gamma <- theta[(ncol(X)+1):(ncol(X)+ncol(Z))]
    mu <- X %*% beta
    #sigma <- sqrt(mean((y-mu)^2))
    sigma <- exp(Z%*%gamma)
    ll <- sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
    -ll
  }
  k <- ncol(X)+ncol(Z)
  theta <- optim(rep(0, k), loglik, method = "BFGS", control=list(reltol=1e-15))$par
  
  beta <- theta[1:ncol(X)]
  gamma <- theta[(ncol(X)+1):(ncol(X)+ncol(Z))]
  return(list("beta"=beta, "gamma" = gamma))
}


bsbasis <- function(z, knots, j, degree) {
  if(degree == 0)
    B <- 1 * (knots[j] <= z & z < knots[j + 1])
  if(degree > 0) {
    b1 <- (z - knots[j]) / (knots[j + degree] - knots[j])
    b2 <- (knots[j + degree + 1] - z) / (knots[j + degree + 1] - knots[j + 1])
    B <- b1 * bsbasis(z, knots, j, degree - 1) + b2 * bsbasis(z, knots, j + 1, degree - 1)
  }
  B[is.na(B)] <- 0
  return(B)
}

bsDesign <- function(z, degree = 3, knots = NULL) {
  ## compute knots
  if(is.null(knots))
    knots <- 40
  if(length(knots) < 2) {
    step <- (max(z) - min(z)) / (knots - 1)
    knots <- seq(min(z) - degree * step, max(z) + degree * step, by = step)
  }
  
  ## evaluate each basis function
  ## and return the full design matrix B
  B <- NULL
  for(j in 1:(length(knots) - degree - 1))
    B <- cbind(B, bsbasis(z, knots, j, degree))
  return(B)
}

dcnorm <- function(x, mu, sigma, a, b, log){
  ifelse(x<=a, pnorm(a, mu, sigma, log.p = log), 
         ifelse(x>=b, pnorm(b, mu, sigma, lower.tail = F, log.p = log)),
         ifelse(x == b, 1-pnorm(b, mu, sigma, log.p = log)))
}
