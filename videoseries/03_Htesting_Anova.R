rm(list = objects())

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

# residual mean squared error
sigma2_hat <- sum(eps^2)/(nrow(X) - ncol(X)) 
sigma2_hat

# standard erros of beta_hat
beta_sd <- sqrt(sigma2_hat*diag(solve(t(X)%*%X)))
beta_sd

#t-test
t_value <- beta/beta_sd
residual_df <- nrow(X)-nrow(beta)
p.value <- 2*pt(abs(t_value), residual_df, lower.tail = FALSE)

#result
res <- cbind(beta, beta_sd, t_value, p.value)
colnames(res) = c("Estimate", "Std. Error", "t value", "Pr(>|t|")
round(res, 4)

mod <- lm(Ozone~., data = data)
round(summary(mod)$coefficients, 4)


## ANOVA
# F = ((RSS0-RSS1)/(df0-df1))/(RSS1/df1)
# estimate null model
Xic <- matrix(1, nrow = nrow(X), ncol = 1, dimnames = list(NULL, "(Intercept)"))
beta_ic <- solve(t(Xic)%*%Xic)%*%t(Xic)%*%y
eps_ic <- y-Xic%*%beta_ic

## RSS/df

# NULL model
df0 <- nrow(Xic)-nrow(beta_ic)
RSS0 <- sum(eps_ic^2)

# by hand OLS Model
df1 <- nrow(X)- nrow(beta)
RSS1 <- sum(eps^2)

# calculate F
F_value <- ((RSS0-RSS1)/(df0-df1))/(RSS1/df1)
c(F = F_value, df0=df0, df1 = df1, RSS0 = RSS0, RSS1 = RSS1)

p.value <- 2*pf(F_value, df0, df1, lower.tail = FALSE)
p.value

## Proof
c("F" = F_value, "Pr(>F)"=p.value)

mod0 <- lm(Ozone~1, data = data)
anova(mod0, mod)

summary(lm(Ozone~Solar.R+Wind+Temp, data = data))
