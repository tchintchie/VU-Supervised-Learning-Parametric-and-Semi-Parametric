rm(list = objects())
library("mgcv")
library(dplyr)


par(mfrow = c(1,1))
d <- na.omit(readRDS("Reto/traffic_data_mittenwald_motorcycles.rds"))
d$weekend <- factor(d$wday, 0:6, c("yes", rep("no",5),"yes"))
d$season <- ifelse((d$mon >2) & (d$mon < 12), "Summer","Winter")

head(d)

### TASK 1

b <- gam(MOT~s(t2m, bs="cp")+weekend, data = d)
plot(b)

t <- gam(t2m~s(yday, bs = "cp"), data = d)
plot(t, main = "Temp per day of year")

d$t2m_ano <- residuals(b)
head(d)

### TASK 2

counts <- gam(MOT~s(yday, bs = "cp"), data = d, family = "poisson")
plot(counts, main = "Counts per day of year")
summary(counts)

### TASK 3
# i
#weekends <- subset(d, weekend == "yes")
#head(weekends)
weekend_counts <- gam(MOT~s(yday, bs = "cp"), data = d, family = "poisson", subset = weekend == "yes")
plot(weekend_counts, main = "Weekends")
summary(weekend_counts)

# ii
#weekdays <- subset(d, weekend == "no")
#head(weekdays)
weekdays_counts <- gam(MOT~s(yday, bs = "cp"), data = d, family = "poisson", subset = weekend == "no")
plot(weekdays_counts, main = "Weekdays")
summary(weekdays_counts)

both <- gam(MOT~s(yday, bs = "cp", by = weekend), data = d, family = "poisson")
plot(both, main = "Counts per day of year by Weekend")
summary(both)
## diff. to task 3:
# model was able to adjust intercept!! subsetting does not!


### TASK 4

task4 <- gam(MOT~t2m_ano, data = d, family = "poisson")
summary(task4)


# i 
#winter <- subset(d, (d$mon <=2) & (d$mon >= 12))
#head(winter)
winter_counts <- gam(MOT~t2m_ano, data = d, family = "poisson", subset = season == "Winter")
plot(winter_counts, main = "Winter")
summary(winter_counts)

# ii
summer_counts <- gam(MOT~t2m_ano, data = d, family = "poisson", subset = season == "Summer")
plot(summer_counts, main = "Summer")
summary(summer_counts)

sum_wint <- gam(MOT~s(yday, bs = "cp", by = t2m_ano), data = d, family = "poisson")
plot(sum_wint, main = "Summer and Winter effects")
summary(sum_wint)

## Variation of intercept
var_int <- gam(MOT ~ s(yday, bs = "cp")+ s(yday, bs = "cp", by = t2m_ano), family = "poisson",data = d)
plot(var_int, main = "Variation of Intercepts")
summary(var_int)

### FINAL TASK
BIC(counts)
BIC(weekend_counts)
BIC(weekdays_counts)
BIC(both)
BIC(task4)
BIC(winter_counts)
BIC(summer_counts)
BIC(sum_wint)
BIC(var_int)
