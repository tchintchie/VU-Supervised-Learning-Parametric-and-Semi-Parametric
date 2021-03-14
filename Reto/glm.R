rm(list = objects())
data("airquality")
data <- na.omit(airquality)
data[,-1]

## Raum 4

mylm <- function(formula, data){
  mf <- model.frame(formula,data) # creates framework
  y <- model.response(mf) # creates generic response variable
  X <- model.matrix(mf, data = data) # creates generic feature matrix
  
  beta <- solve(t(X) %*% X) %*% t(X) %*% y # (XTX)^-1*XTy or solve(X, y)
  eta <- X %*% beta # Link function --> XT*beta or X*beta?
  eps <- y - eta # y - yhat --> yhat = eta
  var_hat <- sum(eps^2)/(nrow(X)-ncol(X))
  beta_sd <- sqrt(var_hat*diag(solve(t(X)%*%X)))
 # beta_sd <- drop((t(eps) %*% eps)/(nrow(X)-ncol(X))) # this line works without var_hat
  t <- beta / beta_sd
  pval <- 2*(1-pt(q = abs(t), df = nrow(X)-ncol(X)))
  #pval <- 2 * pt(abs(t),df = (nrow(X)-ncol(X)),lower.tail = FALSE) # also correct
  
  return(list("beta" = beta, "eta" = eta, "residuals" =eps , "bs" = beta_sd, "t" = t, "pval" = pval))
}

mylm(Ozone~Temp, data = data)$beta
lm(Ozone~Temp, data = data)

### Loglikelyhood ##################

mylm <- function(formula, data){
  mf <- model.frame(formula,data) # creates framework
  y <- model.response(mf) # creates generic response variable
  X <- model.matrix(mf, data = data) # creates generic feature matrix
  
  ## calc loglikelyhood
  ll <- function(par, y, X) {
    eta <- X %*% par[-length(par)]
    sd <- exp(par[length(par)]) # sd(y - eta)
    return(sum(dnorm(y, mean = eta, sd = sd, log = TRUE)))
  }
  par <- rep(0, ncol(X) + 1) # Initial values (beta, log(sd))
  opt <- optim(par, ll, y = y, X = X, method = "BFGS", control = list(fnscale = -1))
  beta <- opt$par[-length(opt$par)] # final best guess of optimizer
  sigma2 <- exp(opt$par[length(opt$par)])^2 
  ## calc loglikelyhood
  
  eta <- X %*% beta # Link function --> XT*beta or X*beta?
  eps <- y - eta # y - yhat --> yhat = eta
  var_hat <- sum(eps^2)/(nrow(X)-ncol(X))
  beta_sd <- sqrt(var_hat*diag(solve(t(X)%*%X)))
  # beta_sd <- drop((t(eps) %*% eps)/(nrow(X)-ncol(X))) # this line works without var_hat
  t <- beta / beta_sd
  pval <- 2*(1-pt(q = abs(t), df = nrow(X)-ncol(X)))
  #pval <- 2 * pt(abs(t),df = (nrow(X)-ncol(X)),lower.tail = FALSE) # also correct
  
  return(list("beta" = beta, "eta" = eta, "residuals" =eps , "bs" = beta_sd, "t" = t, "pval" = pval))
}

mylm(Ozone~Temp, data = data)$beta
lm(Ozone~Temp, data = data)



