rm(list = objects())

library(ggplot2)

data("airquality")
data <- na.omit(subset(airquality, select=c(Ozone, Solar.R, Wind, Temp)))
head(data, n = 2)

# prepare response vector y

y <- data$Ozone
head(y)

# covariate matrix X
X <- as.matrix(cbind("(Intercept)" = 1, subset(data, select = -Ozone)))
head(X, n = 2)

beta <- solve(t(X)%*%X)%*%t(X)%*%y

ll <- function(beta, y, X){
  sd <- exp(beta[length(beta)])
  mu <- X%*%beta[-length(beta)]
  loglik <- dnorm(y, mean = mu, sd = sd, log = T)
  return(-sum(loglik))
}

mf <- model.frame(Ozone~Solar.R+Temp+Wind, data = data)
start <- c(rep(0, ncol(mf)),1)
opt <- optim(start, ll, method = "BFGS", control = list(reltol = 1e-20),
             y = model.response(mf), X=model.matrix(mf, data))

opt$par
as.vector(beta)
coef(lm(Ozone~Solar.R+Temp+Wind, data = data))
opt$par[-length(opt$par)]

# evaluate
opt$value
#Proof
logLik(lm(Ozone~Solar.R+Temp+Wind, data = data))

# OLS vs Gaussian MLE
x <- seq(-10, 10, length = 501)
xlab <- expression(paste("y - ", hat(y)))
par(mfrow = c(1, 3))
plot(x, x^2, type = "l", xlab = xlab, main = "Squared Error")
plot(x, dnorm(x, log = T), type = "l", xlab =xlab, main = "Gaussian Loglikelihood")
plot(x^2, dnorm(x, log = T), type = "l", xlab =xlab, main = "Comparison")