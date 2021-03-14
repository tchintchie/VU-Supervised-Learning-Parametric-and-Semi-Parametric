rm(list = objects())

# Workplan
# Steps towards the final model
# 1 Understand, prepare, and clean the data set
# 2 Define the task/formulate the hypothesis to be tackled
# 3 Feature engineering
# 4 Given response and covariates: select/specify appropriate model
# 5 Estimate the model
# 6 Interpret and evaluate the estimate (→ 3/4?)
# 7 Model selection, comparison, and validation (→ 3/4?)


### Raum 4 ####
data <- readRDS("Reto/traffic_data_mittenwald_motorcycles.rds")
str(data)
corr <- cor(data[-1])
round(corr, 2)

data$mon <- as.factor(data$mon)
data$wday <- as.factor(data$wday)
data$yday <- as.factor(data$yday)
data$year <- as.factor(format(data$date, format = "%Y"))



lin <- lm(MOT~mon+wind100m+ssrd+tp+tcc+t2m+wday,data = data)
summary(lin)
plot(lin)


lin2 <- lm(MOT~mon+ssrd+t2m+wday+year,data = data)
summary(lin2)
plot(lin2)

preds <- predict(lin)
summary(preds)
# Negatives Minimum!
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -242.440    9.531  194.043  246.178  471.875  998.498 


## Besser --> GLM count-data Poisson Model
gmod <- glm(MOT~mon+wind100m+ssrd+tp+tcc+t2m+wday+year,data = data, family = "poisson")
summary(gmod)
pred_glm <- predict(gmod)
summary(pred_glm)
plot(gmod)

##### Resolutioin ##########
## investigate data
data <- readRDS("Reto/traffic_data_mittenwald_motorcycles.rds")
str(data)
## --> because response var MOT is a positive integer --> we need poisson model for count data!
hist(data$MOT)

# feature engineering
# When do we need to do speed controls?
# Mayor wants to keep no of bikes low (closing roads)
# Learn when roads are busy (restaurants, festivals)
data <- transform(data, year = factor(format(date, "%Y")))
data <- transform(data, ydaysin = sin(yday/365*2*pi), ydaycos = cos(yday/365*2*pi)) # sin and cos don´t have to be used both
plot(data$ydaysin, type = "l")



# select an appropriate model
# Poisson Model (gives relative changes) vs. Linear Model (gives absolute changes)


# Estimate Model
m1 <- glm(MOT~ydaysin+ydaycos+t2m+tcc*tp+ssrd+as.factor(mon)+as.factor(wday), data = data, family = "poisson")
summary(m1)
## Interpretation
# Good Selection of covariates --> all have very low p-value
# NULL Hypothesis = no correlation with features --> reject NULL Hypothesis! Impact is significant
# Relative Changes:
coef(m1)["t2m"]
#t2m 
#0.03050094
exp(coef(m1)["t2m"])
#1.030971  for each degree the temp. rises we have 3% more bikers on the road
1-exp(coef(m1)["as.factor(wday)1"])
# Monday (Day 1) has 58% less bikers than Sunday (Day 0 = Intercept)


## Experiment:
x1 <- glm(MOT~t2m, data = data, family = "poisson")
x2 <- glm(MOT~as.factor(mon), data = data, family = "poisson")
x3 <- glm(MOT~ydaysin+ydaycos, data = data, family = "poisson")

plot(predict(x1, type = "response"), type = "l")
lines(data$MOT, col = "tomato")
plot(predict(x2, type = "response"), type = "l")
lines(data$MOT, col = "tomato")
plot(predict(x3, type = "response"), type = "l")
lines(data$MOT, col = "tomato")

## Models compensate each other which is what we want
matplot(predict(x1, type = "term"))
matplot(predict(x2, type = "term"))
matplot(predict(x3, type = "term"))

# figure out the best model
AIC(x1, x3, x3)
BIC(x1, x2, x3)


##### Temperature ##########
data <- readRDS("GFS_Innsbruck/GFS_Innsbruck_039.rds")
data <- na.omit(subset(data, select=c(observation, t2m, t850)))
head(data)

## What model to use?
mod <- lm(observation~t2m, data = data)
summary(mod)
## Interpretation:
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 7.940982   0.083778   94.79   <2e-16 ***
#   t2m         1.049006   0.008065  130.06   <2e-16 ***
#   ---

# 
new <- subset(tail(readRDS("GFS_Innsbruck/GFS_Innsbruck_039.rds"),1), select = c(observation, t2m, t850))
coef(mod)
new
# forecast for tomorrow afternoon 15 UTC
## T = 7.9409 - 0.6041*1.049
predict(mod, newdata = new)
plot(observation~t2m, data = data)
abline(0,1, col = "tomato", lwd = 3)
## Huge bias, model adjusts for that with intercept of 7.9409

data <- readRDS("GFS_Innsbruck/GFS_Innsbruck_039.rds")
modx <- lm(observation~.-datetime, data = data)
summary(modx)
predict(modx, newdata = tail(data, 1))

