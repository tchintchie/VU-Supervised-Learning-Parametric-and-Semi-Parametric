# -------------------------------------
# ULG Data Science
# Supervised Learning
# Just a live script
# -------------------------------------

rm(list = objects())

# Loading the data set
data("airquality")
data <- na.omit(airquality)
head(data)

# Custom function for OLS regression models
mylm <- function(formula, data) {
    mf <- model.frame(formula, data)
    y  <- model.response(mf)
    X  <- model.matrix(mf, data = data)
    
    beta    <- solve(t(X) %*% X) %*% t(X) %*% y
    yhat    <- X %*% beta
    epsilon <- y - yhat
    var_hat <- sum(epsilon^2) / (nrow(X) - ncol(X))
    beta_sd <- sqrt(var_hat * diag(solve(t(X) %*% X)))
    t       <- beta / beta_sd
    pval    <- 2 * (1 - pt(q = abs(t), df = nrow(X) - ncol(X)))
    return(list(beta = beta, pval = pval))
}
x <- mylm(Ozone ~ Temp, data = data)

m <- lm(Ozone ~ Temp, data = data)





