library(tidyverse)
slice(swiss, 1) # observation (row) 1
slice(swiss, -1) # all rows but row 1

n <- nrow(swiss)

data_sets <- map(seq(n), 
                 ~list(train = slice(swiss, -.), 
                       test = slice(swiss, .))
)

# get our functions from utils.R
source("https://raw.githubusercontent.com/mark-andrews/msms01/master/utils/utils.R")

# normal linear model of Fertility modelled as a function of all the other variables
M_1 <- lm(Fertility ~ ., data = swiss)

all_loo_splits <- lm_loo(M_1)
length(all_loo_splits)
class(all_loo_splits)
summary(all_loo_splits[[1]])
summary(M_1)

lm_loo_cv(M_1) * -2 # elpd of M_1

M_2 <- lm(Fertility ~ Agriculture + Education + Examination, data = swiss)

lm_loo_cv(M_2) * -2 # elpd of M_2

AIC(M_1)
AICc(M_1)
AICc(M_2)

AICc(M_2) - AICc(M_1)


# Variable selection ------------------------------------------------------

data_set <- make_data_set(N = 1000, 
                          K = 10, 
                          p = 3, 
                          test_proportion = 0.1)


library(GGally)
ggcorr(data_set$train, label = T)

M_3 <- lm(y ~ ., data = data_set$train)
summary(M_3)

M_3_step_bw <- step(M_3, direction = 'backward')

M_3_null <- lm(y ~ 1, data = data_set$train)
M_3_step_fwd <- step(M_3_null, direction = 'forward', scope = formula(M_3))

M_3_step_both <- step(M_3, direction = 'both')

M_3a <- lm(y ~ x_1, data = data_set$train)


# All subsets lm ----------------------------------------------------------

multimodel <- all_subsets_lm(data_set$train, y, x_1:x_10)
result <- all_subsets_lm_eval(multimodel)

result %>% relative_importance_weights()

bootstrap_aic(multimodel)


# Model averaging ---------------------------------------------------------

library(MuMIn)
M4 <- lm(Fertility ~ ., data = swiss, na.action = 'na.fail')
mmM4 <- dredge(M4)
mmM4

model.avg(mmM4, subset = delta < 4)
model.avg(mmM4, subset = delta < 2)

confset <- get.models(mmM4, cumsum(weight) <= 0.95)
avg_models <- model.avg(confset)
confint(avg_models)


# Ridge, lasso, elastic ---------------------------------------------------

library(glmnet)
y <- data_set$train$y
X <- as.matrix(select(data_set$train, starts_with('x_')))

M5_lasso <- glmnet(X, y, alpha = 1)
plot(M5_lasso, xvar='lambda', label = TRUE)

lambda_cv <- cv.glmnet(X, y, alpha = 1)
plot(lambda_cv)
lambda_cv$lambda.min

M5_lasso <- glmnet(X, y, alpha = 1, lambda = 0.04)
coef(M5_lasso)



M5_ridge <- glmnet(X, y, alpha = 0)
plot(M5_ridge, xvar='lambda', label = TRUE)

lambda_cv <- cv.glmnet(X, y, alpha = 0)
plot(lambda_cv)
lambda_cv$lambda.min

M5_ridge <- glmnet(X, y, alpha = 0, lambda = 0.1)
coef(M5_ridge)

# Bayesian methods --------------------------------------------------------

library(lme4)
classroom_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/msms01/master/data/classroom.csv")

M5a <- lmer(mathscore ~ ses + (ses|schoolid) + (ses|classid), data = classroom_df)
M5b <- lmer(mathscore ~ ses + (ses|schoolid) + (ses||classid), data = classroom_df)
M5c <- lmer(mathscore ~ ses + (ses|schoolid) + (1|classid), data = classroom_df)

library(brms)
M5a_bayes <- brm(mathscore ~ ses + (ses|schoolid) + (ses|classid),
                 cores = 4,
                 data = classroom_df)
M5a_bayes_lm <- brm(mathscore ~ 1, data = classroom_df)

M5a_bayes
waic(M5a_bayes)
brms::loo(M5a_bayes)
