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

lm_loo_cv(M_1) * -2

M_2 <- lm(Fertility ~ Agriculture + Education + Examination, data = swiss)

lm_loo_cv(M_2) * -2
