y <- cars$dist
x <- cars$speed
beta_0 <- -20
beta_1 <- 4
sigma <- 15

prob_obs_lm <- function(y, x, beta_0, beta_1, sigma, log = F){
  mu <- beta_0 + beta_1 * x
  dnorm(y, mean = mu, sd = sigma, log = log)
} 

prob_obs_lm(y, x, beta_0, beta_1, sigma)
# log of the (conditional) probability of the observed data..
sum(prob_obs_lm(y, x, beta_0, beta_1, sigma, log = T))


# Get maximum likelihood estimates for slope and intercept
M <- lm(dist ~ speed, data = cars)
coefficients(M)
coef(M)

# get the residuals
residuals(M)

# square of the residuals
sum(residuals(M)^2) # RSS

# maximum likelihood estimator of sigma^2
mean(residuals(M)^2)

# maximum likelihood estimator of sigma is
sqrt(mean(residuals(M)^2))

# not to be confused with 
sigma(M)

# use our mle to calculate model log probability

beta_0_mle <- coef(M)[1]
beta_1_mle <- coef(M)[2]
sigma_mle <- sqrt(mean(residuals(M)^2))

# log probability of the observed according to model with its ml estimators
# of the parameters
sum(prob_obs_lm(y, x, beta_0_mle, beta_1_mle, sigma_mle, log = T))

logLik(M)



# Deviance in GLMs --------------------------------------------------------

library(tidyverse)
cars_df <- mutate(cars, z = dist > median(dist))

M2 <- glm(z ~ speed, data = cars_df, family = binomial())
logLik(M2)
deviance(M2)
logLik(M2) * -2

# deviance residuals
sum(residuals(M2)^2)
