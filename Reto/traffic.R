# -----------------------------------
# ULG Data Science
# Supervised Learning: Session 01
# Motorcycle counts Mittenwald
# -----------------------------------

rm(list = objects())
# setwd("~/Downloads/ULG_01")
data <- readRDS("traffic_data_mittenwald_motorcycles.rds")

# Investigate the data set; understand what we have at hand
data <- transform(data, year = factor(format(date, "%Y")))
data <- transform(data, ydaysin = sin(yday / 365 * 2 * pi),
                        ydaycos = cos(yday / 365 * 2 * pi))

# Feature engineering
# When do we need to do speed checks
# Major want's to keep the number of bikes low (closing roads)
# To learn when the roads are busy

# Select an appropriate model
# Poisson

# Estimate the model
m1 <- glm(MOT ~ ydaysin + ydaycos + t2m + tcc * tp + ssrd + as.factor(mon) + as.factor(wday),
          data = data, family = "poisson")

x1 <- glm(MOT ~ t2m, data = data, family = "poisson")
x2 <- glm(MOT ~ as.factor(mon), data = data, family = "poisson")
x3 <- glm(MOT ~ ydaysin + ydaycos, data = data, family = "poisson")

plot(predict(x1, type = "response"), type = "l")
lines(data$MOT, col = "tomato")

plot(predict(x2, type = "response"), type = "l")
lines(data$MOT, col = "tomato")

plot(predict(x3, type = "response"), type = "l")
lines(data$MOT, col = "tomato")

# Checking model, interpret model

# Adapt, improve, overcome


# ...

# Why not a Gaussian linear model?
# Also shown on slides (01-count-data something .pdf)
mgauss <- lm(MOT ~ ydaysin + ydaycos + t2m + tcc * tp + ssrd + as.factor(mon) + as.factor(wday),
              data = data)
mpoiss <- glm(MOT ~ ydaysin + ydaycos + t2m + tcc * tp + ssrd + as.factor(mon) + as.factor(wday),
             data = data, family = "poisson")
plot(MOT ~ date, data = data, type = "l")
lines(data$date, predict(mgauss), col = "#ff000090")
lines(data$date, predict(mpoiss, type = "response"), col = "#00ff0090")

