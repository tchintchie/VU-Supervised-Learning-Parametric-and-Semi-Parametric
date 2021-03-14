data("airquality")
data <- na.omit(subset(airquality, select=c(Ozone, Solar.R, Wind, Temp)))
head(data, n = 2)

# prepare response vector y

y <- data$Ozone
head(y)

# covariate matrix X
X <- as.matrix(cbind("(Intercept)" = 1, subset(data, select = -Ozone)))
head(X, n = 2)

# estimate OLS

beta <- solve(t(X)%*%X)%*%t(X)%*%y
beta

## calculate residuals

yhat <- X%*%beta
eps <- y-yhat

