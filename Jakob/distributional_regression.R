rm(list = objects())
source("Jakob/02-windpower/wp_functions.r")


data <- na.omit(read.csv("Jakob/02-windpower/windpower.csv"))
str(data)
data$date <- as.Date(data$date, "%Y-%m-%d")
str(data)

plot(data)
plot(data$wisp_24~data$p)

## OLS as Maximum Likelihood:


### Practical Session 1: Distributional Regression
# Wind Power
# Room 3


# load dataset
data <- na.omit(read.csv("Jakob/02-windpower/windpower.csv"))
str(data)
data$date <- as.Date(data$date, "%Y-%m-%d")
data <- data[order(data$wisp_24),]
str(data)


## generate Design matrix
X <- bsDesign(z=data$wisp_24, degree= 9,knots= 9)
mle_coef <- mle(data$p,X,X)
lin <- lm(p~X-1, data = data)

# summary(lin)
# 
# ## predictions
pred_mle <- X%*%mle_coef$beta
stde <- X%*%mle_coef$gamma

# derive quantiles
#qnorm(0.25, pred_mle, stde)

# 
# 
# plot(p~wisp_24, data = data)
# lines(pred_mle~data$wisp_24, col = "red", lwd = 3)
# abline(lin, col = "blue")
# 
