rm(list = objects())


logit <- function(x) exp(x)/(1+exp(x))
probit <- function(x) pnorm(x)
cloglog <- function(x) 1 - exp(-exp(x))


data <- na.omit(subset(airquality, select = c(Ozone, Solar.R, Wind, Temp)))
data$Ozone <- data$Ozone > mean(data$Ozone)
head(data, n = 3)

ll <- function(beta, y, X){
  eta <- X%*%beta
  pi <- exp(eta)/(1+exp(eta))
  return(-sum(y*log(pi/(1-pi))+log(1-pi)))
}

y <- model.response(model.frame(Ozone~.,data = data))
X <- model.matrix(model.frame(Ozone~., data = data), data)
par <- rep(0, ncol(X))
opt <- optim(par = par, fn = ll, method = "BFGS", y = y, X = X)
print(opt$par)

c("evaluate ll()" = -ll(opt$par, y = y, X = X), "optim return" = -opt$value)

summary(glm(Ozone~., data = data, family = binomial(link = "logit")))

# interpretation of model
mod <- glm(Ozone~., data = data, family = binomial(link = "logit"))
coef(mod)

# a change by +1 in Wind speed:
b <- coef(mod)["Wind"]
c("beta"=b, "exp(beta)" = exp(b), "change" = 100*exp(b)-100)
# decreases the odds of observing y_i = 1 by -25.4%

# different link functions
logit <- glm(Ozone~., data = data, family = binomial(link = "logit"))
probit <- glm(Ozone~., data = data, family = binomial(link = "probit"))
cloglog <- glm(Ozone~., data = data, family = binomial(link = "cloglog"))

c(logLik(logit), logLik(probit), logLik(cloglog))
anova(logit, probit, cloglog)

pi_logit <- fitted(logit); eta_logit <- predict(logit)
pi_probit <- fitted(probit); eta_probit <- predict(probit)
pi_cloglog <- fitted(cloglog); eta_cloglog <- predict(cloglog)


plot(logit$fitted.values~probit$fitted.values, col = "red", ylab = "logit model", xlab = "probit/cloglog model")
points(logit$fitted.values~cloglog$fitted.values)
legend(x = "topleft", inset = 0, legend = c("logit~probit", "logit~cloglog"), pch = c(10,10), cex = 1, col = c("red", "black"))



plot(eta_logit~eta_probit, col = "red", ylab = "logit model", xlab = "probit/cloglog model")
points(eta_logit~eta_cloglog)
legend(x = "topleft", inset = 0, legend = c("logit~probit", "logit~cloglog"), pch = c(10,10), cex = 1, col = c("red", "black"))

## Rs Formula interface
data <- na.omit(subset(airquality, select = c(Ozone, Solar.R, Wind, Temp)))
glm(Ozone>mean(Ozone)~Solar.R+Wind+I(Temp>77), data = data, family = binomial(link = "logit"))

# calculate individual Effects
mod <- glm(Ozone>mean(Ozone)~Solar.R+Wind+Temp, data = data, family = binomial(link = "logit"))
effect_Wind <- data$Wind*coef(mod)["Wind"]

plot(data$Wind, col = "black", type = "l", ylim = range(c(data$Wind)), main = "Wind (Observation) and Wind Effect (Coefficient: -0.29)")
par(new = T)
plot(effect_Wind, col = "red", type = "l", ylim = range(effect_Wind), axes = F, xlab = "", ylab="")
mtext("effect_Wind", side = 4, col = "red")
axis(4, ylim = range(effect_Wind), col = "red", col.axis = "red", las = 1)

# get modelled effects
mod <- glm(Ozone>mean(Ozone)~Solar.R+Wind+Temp, data = data, family = binomial(link = "logit"))
eff <- predict(mod, type = "terms")

head(eff, n = 3)

par(mfrow = c(3,1))
plot(eff[,"Solar.R"], type = "l", main = "Effects/Terms", ylab = "Solar.R")
plot(eff[,"Wind"], type = "l",  ylab = "Wind")
plot(eff[,"Temp"], type = "l",  ylab = "Temp")

### Poisson Regression usually used with factors (but didn´t have a dataset)
data <- read.csv("D:/DataScience/ULG/VU1-Data-Management/data/bevoelkerung-tirol.csv", comment.char = "#")

x <- 0:7
plot(x, dpois(x, lambda = 0.5), type = "h", lwd = 5, main = "Poisson Density (0.5)")
plot(x, dpois(x, lambda = 1), type = "h", lwd = 5, main = "Poisson Density (1.0)")
plot(x, dpois(x, lambda = 3), type = "h", lwd = 5, main = "Poisson Density (3.0)")

mgauss <- lm(Tuerkei~factor(Jahr), data = data)
mpoiss <- glm(Tuerkei~factor(Jahr), data = data, family = "poisson")

print(mpoiss)
pred_gauss <- predict(mgauss)
pred_poiss <- predict(mpoiss)

#effect_gauss <- data$Bezirk*coef(mgauss)
#effect_poiss <- data$Bezirk*coef(mpoiss)

## visualize
par(mfrow = c(1,1))
plot(x = data$Jahr, y = data$Tuerkei, type = "l", col = "black")
par(new = T)
plot(x = data$Jahr, y = pred_gauss, col = "red", type = "l", ylim = range(pred_gauss), axes = F, xlab = "", ylab="")
par(new = T)
plot(x = data$Jahr, y = pred_poiss, col = "blue", type = "l", ylim = range(pred_poiss), axes = F, xlab = "", ylab="")
legend(x = "topleft", inset = 0, legend = c("observed", "Gaussian lm", "Poisson glm"), lwd = 3, cex = 1, col = c("black", "red", "blue"))

## interpretation
coefficients(mpoiss)[c("(Intercept)","factor(Jahr)2002")]
exp(coefficients(mpoiss)["factor(Jahr)2002"])
# change in percent
100*exp(coefficients(mpoiss)["factor(Jahr)2002"])-100
100*exp(coefficients(mpoiss)["factor(Jahr)2009"])-100


