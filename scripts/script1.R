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



# Nested linear models ----------------------------------------------------

M1 <- lm(Fertility ~ Agriculture + Education + Catholic, data = swiss)
M0 <- lm(Fertility ~ Agriculture + Education, data = swiss)
M0_alt <- lm(Fertility ~ Education + Agriculture, data = swiss)

RSS_1 <- sum(residuals(M1)^2)
RSS_0 <- sum(residuals(M0)^2)

RSS_0
RSS_1

RSS_0/RSS_1

(RSS_0 - RSS_1) / RSS_1

df_0 <- M0$df.residual
df_1 <- M1$df.residual

(RSS_0 - RSS_1) / (df_0 - df_1)

RSS_1/df_1

f_stat <- ((RSS_0 - RSS_1) / (df_0 - df_1)) / (RSS_1/df_1)

# p value for the null hypothesis test 
pf(f_stat, df1 = df_0 - df_1, df2 = df_1, lower.tail = F)

anova(M0, M1)
anova(M0,M1)$`Pr(>F)`[2] # for just the p-value

drop1(M1, scope = ~ Catholic, test = 'F')

anova(M0)

M0_drop_edu <- lm(Fertility ~ Agriculture, data = swiss)
sum(residuals(M0_drop_edu)^2)
M0_drop_edu_drop_agri <- lm(Fertility ~ 1, data = swiss)
sum(residuals(M0_drop_edu_drop_agri)^2)

M0
M_null <- lm(Fertility ~ 1, data = swiss)
RSS_null <- sum(residuals(M_null)^2)
c(RSS_null, RSS_0, RSS_null - RSS_0)

(RSS_null - RSS_0)/RSS_null
summary(M0)$r.squared

source("https://raw.githubusercontent.com/mark-andrews/msms01/master/utils/utils.R")

rsq_sample(N = 101, x_1:x_5)
quantile(map_dbl(seq(100), ~rsq_sample(N = 101, x_1:x_5)))
quantile(map_dbl(seq(100), ~rsq_sample(N = 50, x_1:x_10)))
quantile(map_dbl(seq(100), ~rsq_sample(N = 50, x_1:x_10, rsq = 'adj.r.squared')))

n <- nrow(swiss)
K <- length(coef(M0)) - 1
1 - (RSS_0/RSS_null) * ((n-1)/(n-K-1))

# Generalized linear models and deviance ----------------------------------

swiss_df <- mutate(swiss, y = Fertility > median(Fertility))

M1 <- glm(y ~ Agriculture + Education + Catholic,
          data = swiss_df,
          family = binomial())

M0 <- glm(y ~ Agriculture + Education,
          data = swiss_df,
          family = binomial())


D_0 <- deviance(M0)
D_1 <- deviance(M1)
c(D_0, D_1)
