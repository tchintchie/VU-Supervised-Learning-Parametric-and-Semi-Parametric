rm(list = objects())
# https://discdown.org/flexregression/bayesreg.html

par(mar = c(4.1, 4.1, 0.1, 0.1))
f <- function(x) dgamma(x, shape = 10)
curve(f, 0, 30, lwd = 4)

# calculate area under curve
pgamma(30, shape = 10) - pgamma(0, shape = 10)

par(mar = c(4.1, 4.1, 0.1, 0.1))
curve(f, 0, 30, ylim = c(0, 0.15), lwd = 4)
rect(0, 0, 30, 0.15, border = 4, lwd = 4)


## Set the seed for reproducibility.
set.seed(123)

par(mar = c(4.1, 4.1, 0.1, 0.1))
curve(f, 0, 30, ylim = c(0, 0.15), lwd = 4)
rect(0, 0, 30, 0.15, border = 4, lwd = 4)
n <- 200
p <- data.frame("x" = runif(n, 0, 30), "y" = runif(n, 0, 0.15))
fx <- f(p$x)
col <- rep(1, n)
col[p$y <= fx] <- 2
points(p, col = col)

mean(p$y <= fx) * (30 * 0.15)
integrate(f, 0, 30)


## Set the seed for reproducibility.
set.seed(123)

par(mar = c(4.1, 4.1, 0.1, 0.1))
curve(f, 0, 30, ylim = c(0, 0.15), lwd = 4)
rect(0, 0, 30, 0.15, border = 4, lwd = 4)
n <- 10000
p <- data.frame("x" = runif(n, 0, 30), "y" = runif(n, 0, 0.15))
fx <- f(p$x)
col <- rep(1, n)
col[p$y <= fx] <- 2
points(p, col = col)

## Compute the integral.
mean(p$y <= fx) * (30 * 0.15)

## Extract the samples saving only
## the x-coordinate of the points.
x <- p[p$y <= fx, ]$x

## Draw a histogram.
par(mar = c(4.1, 4.1, 4.1, 0.1))
hist(x, freq = FALSE, ylim = c(0, 0.15), breaks = "Scott")

## Add the true density curve.
curve(f, 0, 30, add = TRUE, col = 2, lwd = 4)
mean(x)

integrate(function(x) f(x) * x, -Inf, Inf)


#pracitcal exercise ROOM 2
n <- 1000
x <- 0.4*rnorm(n, mean = -1, sd = 1)+(0.6*rgamma(n, shape = 6, rate = 1))

g <- function(x) (0.4*dnorm(x, mean = -1,sd = 1))+(0.6*dgamma(x, shape = 6, rate = 1))
min(x)
max(x)
xmax <- 13
xmin <- -4
ymin <- 0
ymax <- 0.16

xmc <- runif(n, xmin, xmax)
ymc <- runif(n, ymin, ymax)

curve(g, -10, 20, lwd = 4)
borders <- seq(-10, 20, by = 0.01)
rect(xmin,ymin,xmax, ymax, lwd = 4, border = 3)
points(ymc~xmc, cex = 0.5, col = ifelse(ymc<=g(xmc), "red", "black"))
mean(ymc<=g(xmc))*(xmax-xmin)*(ymax-ymin)

xsample <- xmc[ymc<=g(xmc)]
hist(xsample, breaks = "Scott")
mean(xsample)
q <- quantile(xsample, probs = c(0.05, 0.5, 0.95))

curve(g, -10, 20, lwd = 4)
points(ymc~xmc, cex = 0.5, col = ifelse(ymc<=g(xmc), "red", "black"))
abline(v = q, col = "blue", lwd = 2)
abline(v = mean(xsample), col = "green", lwd = 2)


### numerische integration
curve(g)
curve(g, -10, 20)
integrate(g, -Inf, 0)
integrate(g, -Inf, -2)
integrate(g, -Inf, -2.1)
integrate(g, -Inf, 2.15)

foo <- function(x, prob = 0.05){
  (integrate(g, -Inf, x)$value - prob)^2
}

optimize(foo, c(-10, 20))

## rejection sampling - univariate example
## The Gamma distribution we want to draw from.
f <- function(x) dgamma(x, shape = 10)

## The nu-scaled version.
fxnu <- function(x, nu = 0.5) nu * f(x)

par(mar = c(4.1, 4.1, 0.1, 0.1))
curve(fxnu, 0, 30, lwd = 4, ylim = c(0, 0.08))

## The envelope distribution.
f_Y <- function(x) dnorm(x, mean = 10, sd = 5)

## Add to plot.
curve(f_Y, 0, 30, col = 4, lwd = 4, add = TRUE)

## The rejection sampler function.
rsamp <- function(n, f_X, f_Y, q_Y, nu)
{
  x <- rep(NA, length = n)
  i <- 0
  while(i < n) {
    y <- q_Y(runif(1))
    alpha <- nu * f_X(y) / f_Y(y) 
    u <- runif(1)
    if(u <= alpha) {
      i <- i + 1
      x[i] <- y
    }
  }
  return(x)
}

## Quantile function of the envelope distribution to
## draw samples using the inversion method.
q_Y <- function(p) qnorm(p, mean = 10, sd = 5)

## Draw 10000 samples.
set.seed(123)
x <- rsamp(10000, f_X = f, f_Y = f_Y, q_Y = q_Y, nu = 0.5)

## And plot the histogram.
hist(x, freq = FALSE, breaks = "Scott", ylim = c(0, 0.15))
curve(f, 0, 30, lwd = 4, col = 2, add = TRUE)

## Linear model example

set.seed(123)

n <- 300

x <- runif(n, -2 * pi, 2 * pi)
y <- 1.2 + sin(x) + rnorm(n, sd = 0.3)

par(mar = c(4.1, 4.1, 0.1, 0.1))
plot(x, y)

loglik <- function(theta) {
  eta <- X %*% theta
  sum(dnorm(y, mean = eta, sd = 0.3, log = TRUE))
}

logprior <- function(theta) {
  sum(dnorm(theta, mean = 0, sd = 1000, log = TRUE))
}

logpost <- function(theta) {
  loglik(theta) + logprior(theta)
}

## Design matrix.
X <- cbind(1, poly(x,8))
k <- ncol(X)

## Rejection sampler scaling parameter.
nu <- 0.3

## Number of iterations.
iter <- 100000

## Matrix to save the samples.
theta.save <- matrix(NA, iter, k)

## Initial values.
theta <- rep(0, k)

for(i in 1:iter) {
  ## Propose from envelop density.
  theta.prop <- rnorm(k, mean = theta, sd = 4)
  
  ## Compute acceptance rate (log scale).
  alpha <- log(nu) + logpost(theta.prop) - sum(dnorm(y, mean = theta, sd = 4, log = TRUE))
  
  ## Acceptance step.
  u <- runif(1)
  if(u <= min(c(exp(alpha), 1))) {
    theta.save[i, ] <- theta.prop
    theta <- theta.prop
  }
  
}

## Omit all NAs, i.e., unsuccessful draws.
theta.save <- na.omit(theta.save)

## Final number of saved samples.
print(dim(theta.save))

## Evaluate the regression curve for each sample.
fit <- apply(theta.save, 1, function(beta) {
  drop(X %*% beta)
})

## Compute quantiles from samples.
fit <- t(apply(fit, 1, quantile, probs = c(0.1, 0.5, 0.9), na.rm = TRUE))

## Plot the estimate.
par(mar = c(4.1, 4.1, 0.1, 0.1))
i <- order(x)
matplot(x[i], fit[i, ], type = "l", lty = c(2, 1, 2), col = 2, lwd = 2,
        xlab = "x", ylab = "f(x)")
points(x, y)


## metropolis hastings
igamma <- function(x, a = 2, b = 1) {
  b^a / gamma(a) * x^(-(a + 1)) * exp(-b / x)
}
par(mar = c(4.1, 4.1, 0.1, 0.1))
curve(igamma(x, a = 2, b = 1), 0, 10, lty = 2, ylab = "Density", n = 200)
curve(igamma(x, a = 1, b = 0.5), 0, 10, lty = 4, add = TRUE, n = 200)
curve(igamma(x, a = 1, b = 1), 0, 10, lty = 1, add = TRUE, n = 200)
curve(igamma(x, a = 0.5, b = 1), 0, 10, lty = 3, add = TRUE, n = 200)
curve(igamma(x, a = 0.0001, b = 0.0001), 0, 10, col = 2, lwd = 2,
      lty = 1, add = TRUE, n = 200)
legend("topright", c("a = 1, b = 1", "a = 2, b = 1",
                     "a = 0.5, b = 1", "a = 1, b = 0.5", "a = 0.001, b = 0.001"),
       lwd = 1, lty = c(1:4, 1), col = c(rep(1, 4), 2), bty = "n")

set.seed(111)
n <- 100
y <- rnorm(n, mean = 3.44, sd = 0.9)

## Log-prior for sigma.
log_prior_sigma <- function(theta, a = 0.001, b = 0.001) {
  log(igamma(theta[2], a, b))
}

## Log-prior for mu.
log_prior_mu <- function(theta, mu0 = 0, M0 = 10) {
  dnorm(theta[1], mean = mu0, sd = M0, log = TRUE)
}

## Log-likelihood.
log_lik <- function(y, theta) {
  sum(dnorm(y, mean = theta[1], sd = theta[2], log = TRUE))
}

## Log-posterior.
log_post <- function(y, theta) {
  log_lik(y, theta) + log_prior_mu(theta) + log_prior_sigma(theta)
}

prop <- function(x) rnorm(length(x), mean = x, sd = 0.1)

## Set the seed.
set.seed(111)

## Set starting values for the parameters.
theta <- theta_prop <- c(mean(y), sd(y))

## Number of iterations.
T <- 10000

## Matrices to save the samples and acceptance rates.
theta_samples <- matrix(0, T, 2)
acc_rate <- matrix(0, T, 2)

colnames(theta_samples) <- colnames(acc_rate) <- c("mu", "sigma")

## Start sampling.
for(i in 1:T) {
  ## Iterate over parameters.
  for(j in 1:2) {
    ## Propose new parameter.
    theta_prop[j] <- prop(theta[j])
    
    ## Compute acceptance probability.
    ## Note, as we compute the log-posterior, we need to use
    ## exp() to get this on to the probability scale.
    alpha <- exp(log_post(y, theta_prop) - log_post(y, theta))
    
    ## Acceptance step.
    if(!is.na(alpha) && runif(1) < min(alpha, 1)) {
      theta[j] <- theta_prop[j]
      acc_rate[i, j] <- 1
    }
    
    ## Save current parameter sample.
    theta_samples[i, j] <- theta[j]
  }
}

par(mfrow = c(2, 2), mar = c(4.1, 4.3, 1.1, 1.1))
plot(theta_samples[, 1], type = "l", xlab = "Iteration", ylab = expression(mu))
acf(theta_samples[, 1], lag.max = 250, main = "")
plot(theta_samples[, 2], type = "l", xlab = "Iteration", ylab = expression(sigma))
acf(theta_samples[, 2], lag.max = 250, main = "")

colMeans(acc_rate)

t(apply(theta_samples, 2, function(x) {
  x <- c(
    quantile(x, prob = 0.025),
    mean(x),
    quantile(x, prob = 0.975)
  )
  names(x) <- c("2.5%", "Mean", "97.5%")
  x
}))

theta <- theta_prop <- c(0, 2)

prop <- function(x) x + rnorm(1, sd = 0.01)

prop <- function(x) x + rnorm(1, sd = 0.8)


### Exercise Munic Rent Model###########################################
file_url <- "http://www.bamlss.org/misc/rent99.raw"
rent <- read.table(file_url, header = TRUE)
head(rent)

## Log-prior for sigma.
log_prior_sigma <- function(theta, a = 0.001, b = 0.001) {
  log(igamma(theta[length(theta)], a, b))
}

## Log-prior for mu.
log_prior_mu <- function(theta, mu0 = 0, M0 = 10) {
  dnorm(theta[length(theta)], mean = mu0, sd = M0, log = TRUE)
}

## Log-likelihood.
# design matrix
X <- cbind(1,poly(rent$area, 3))
y <- rent$rentsqm

log_lik <- function(y, theta) {
  sum(dnorm(y, mean = X%*%theta[-length(theta)], sd = theta[length(theta)], log = TRUE))
}

## Log-posterior.
log_post <- function(y, theta) {
  log_lik(y, theta) + log_prior_mu(theta) + log_prior_sigma(theta)
}

prop <- function(x) rnorm(length(x), mean = x, sd = 0.1)

## Set starting values for the parameters.
theta <- theta_prop <- c(rep(1, ncol(X)), sd(y))

## Number of iterations.
T <- 10000

## Matrices to save the samples and acceptance rates.
theta_samples <- matrix(0, T, ncol(X)+1)
acc_rate <- matrix(0, T, ncol(X)+1)

colnames(theta_samples) <- colnames(acc_rate) <- c("mu", "sigma")

## Start sampling.
for(i in 1:T) {
  ## Iterate over parameters.
  for(j in 1:length(theta)) {
    ## Propose new parameter.
    theta_prop[j] <- prop(theta[j])
    
    ## Compute acceptance probability.
    alpha <- exp(log_post(y, theta_prop) - log_post(y, theta))
    
    ## Acceptance step.
    if(!is.na(alpha) && runif(1) < min(alpha, 1)) {
      theta[j] <- theta_prop[j]
      acc_rate[i, j] <- 1
    }
    
    ## Save current parameter sample.
    theta_samples[i, j] <- theta[j]
  }
}

### plotting #####

par(mfrow = c(length(theta), 2), mar = c(4.1, 4.3, 1.1, 1.1))
for(i in 1:length(theta)){
  plot(theta_samples[, i], type = "l", xlab = "Iteration", ylab = paste0("Theta", i))
  acf(theta_samples[, i], lag.max = 500, main = "")
  
}

t(apply(theta_samples, 2, function(x) {
  x <- c(
    quantile(x, prob = 0.025),
    mean(x),
    quantile(x, prob = 0.975)
  )
  names(x) <- c("2.5%", "Mean", "97.5%")
  x
}))

quantile(X%*%theta_samples[1, -ncol(theta_samples)], probs = 0.025)



###########################################################################
#### 7.3.1 Slice Sampling
rm(list = objects())
par(mfrow = c(1,1))
set.seed(123)

n <- 300

x <- runif(n, -2 * pi, 2 * pi)
y <- 1.2 + sin(x) + rnorm(n, sd = 0.3)

plot(x, y)

## Design Matrix X
X <- cbind(1,poly(x, 3))
k <- ncol(X)
