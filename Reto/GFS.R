# -----------------------------------
# ULG Data Science
# Supervised Learning: Session 01
# Weather Forecast Innsbruck (GFS)
# -----------------------------------

rm(list = objects())
# setwd("~/Downloads/ULG_01")

data <- readRDS("GFS_Innsbruck_039.rds")
data <- na.omit(subset(data, select = c(observation, t2m, t850)))

mod <- lm(observation ~ t2m, data = data)


new <- subset(tail(readRDS("GFS_Innsbruck_039.rds"), 1), select = c(observation, t2m, t850))
new
coef(mod)
## Forecast for tomorrow afternoon 15 UTC:
## T = 7.9409 - 0.6041 * 1.049
predict(mod, newdata = new)

plot(observation ~ t2m, data = data)
abline(0, 1, col = "tomato", lwd = 3)


data <- readRDS("GFS_Innsbruck_039.rds")

modx <- lm(observation ~ . - datetime, data = data)
predict(modx, newdata = tail(data, 1))

