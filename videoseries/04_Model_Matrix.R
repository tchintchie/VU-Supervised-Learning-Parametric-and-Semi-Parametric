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

# create model matrix

mf <- model.frame(Ozone~., data = data)
head(mf, n = 2)

y <- model.response(mf)
X <- model.matrix(mf, data = data)
c(class(y), class(X))
head(X, n = 2)

# extract model matrix
mod <- lm(Ozone~., data = data)
X <- model.matrix(mod)
head(X, n = 2)

mf <- model.frame(mod)
head(mf, n = 2)

#transformed covariates
f <- Ozone~.+sqrt(Solar.R)+I(Solar.R^2)+log(Solar.R)
modelX <- lm(f, data = data)
head(model.matrix(modelX), n = 3)

# categorical covariates (extending linear predictor)
data$TempFac <- cut(data$Temp, breaks = 3, labels = c("Low", "Mod", "High"))
modelX <- lm(Ozone~., data = data)
mm <- model.matrix(modelX)
head(mm, n = 3)

ggplot(data, aes(y = TempFac, x = Temp))+
  geom_point()+
  labs(title = "Temperature distribution by class")

# Interpretation
nd1 <- data[1,]; nd1$TempFac <- "Low"
nd2 <- data[1,]; nd2$TempFac <- "Mod"
nd3 <- data[1,]; nd3$TempFac <- "High"

p1 <- predict(modelX, newdata = nd1)
p2 <- predict(modelX, newdata = nd2)
p3 <- predict(modelX, newdata = nd3)

coef(modelX)

# Interactions
getmm <- function(formula, n = 1){
  head(model.matrix(model.frame(formula, data), data), n=n)
}

# continous variables
getmm(Ozone~Wind*Temp)
getmm(Ozone~Wind:Temp)
getmm(Ozone~Wind:Temp:Solar.R)

# Continous and factors
getmm(Ozone~Wind*TempFac)
getmm(Ozone~Wind:TempFac)
getmm(Ozone~Wind:TempFac:Solar.R)
getmm(Ozone~Wind*TempFac*Solar.R)

# factor and factor
data$WindFac <- cut(data$Wind, 2, labels = c("Calm","Windy"))
data$SolarFac <- cut(data$Solar.R, 2, labels = c("Cloudy","Sunny"))
getmm(Ozone~WindFac*TempFac*SolarFac, n = 2)

# crossing
getmm(Ozone~(WindFac+TempFac+SolarFac)^3, n = 3)

# Harmonics
data("airquality"); data <- na.omit(airquality)
head(data, n = 2)

date <- ISOdate(1970, data$Month, data$Day)
data$yday <- as.POSIXlt(date)$yday
data$ydayrad <- data$yday/365*2*pi
modelX <- lm(Ozone~Solar.R+Wind+Temp+sin(ydayrad)+cos(ydayrad),data = data)
head(model.matrix(modelX), n=2)


plot(x = data$yday, y = sin(data$ydayrad), type = "l")
plot(x = data$yday, y = cos(data$ydayrad), type = "l")

# OLS estimate using first harmonic only
mf <- model.frame(Ozone~sin(ydayrad)+cos(ydayrad), data = data)
y <- model.response(mf)
X <- model.matrix(mf, data = data)
beta <- solve(t(X)%*%X)%*%t(X)%*%y
yhat <- X%*%beta

plot(x = data$yday, y = data$Ozone, type = "l")
lines(x=data$yday, y = yhat, col = "red")
