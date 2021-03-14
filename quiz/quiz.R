rm(list = objects())

library(mgcv)
library(sf)
library(gamlss)


data <- read.csv("quiz/gam-data.csv")

plot(y~x, data = data)

model <- gam(y~s(x, bs = "ps", k = 14), data = data)
summary(model)

BIC(model)

gb <- readRDS("quiz/gadm36_GBR_2_sf.rds")
head(gb)
str(gb)
class(gb)


distreg <- read.csv("quiz/distreg-data.csv")
mod <- gamlss(y~x, data = distreg)
summary(mod)
