rm(list = objects())

library(mgcv)
library(sf)
library(spdep)
library(tidyverse)
library(colorspace)


data(Sacramento, package = "modeldata")
Sacramento$price_sqft <- Sacramento$price/Sacramento$sqft

## explain price by long and lat

model <- gam(price_sqft~s(longitude, latitude), data = Sacramento)
summary(model)
plot(model)

### Hands on Session
