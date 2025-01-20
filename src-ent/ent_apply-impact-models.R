## K. Garner, 2025 ######################################
#########################################################
# apply logistic regression models to subjects, and extract beta
# values

#################################################
# clear environment and get what you need
rm(list=ls())
library(tidyverse)
source(paste('src-ent/ent_functions.R', sep='/'))

#################################################
# load data
exp_str <- 'lt'
dat <- read.csv(paste('../doors-data/data-wrangled/exp', 
                      exp_str, 'evt.csv', sep='_')) %>% 
  filter(ses == 2) %>% 
  select(sub, train_type, t, context, door, door_cc, door_oc, door_nc, switch)

#################################################
# add regressors for each subject
subs <- unique(dat$sub)
dat <- do.call(rbind, lapply(subs, get_Sw, dat=dat))
dat <- do.call(rbind, lapply(subs, get_p_context, dat=dat))

#################################################
# write a function that runs the logistic regression
# and returns beta coefficients, for each subject
run_logist <- function(dat, subN){
  
  tmp <- dat %>% filter(sub == subN)
  fit <- glm(door_m ~ Sw + Swr + N_oc , data=tmp, 
             family=binomial(link="logit")) 
  coef <- fit$coefficients
  tibble(sub = subN,
         train_type = tmp$train_type[1],
         mu = coef['(Intercept)'],
         sw = coef['Sw'],
         swr = coef['Swr'],
         noc = coef['N_oc'])
}

betas <- do.call(rbind, lapply(subs, run_logist, dat=dat))
write.csv(betas, paste('betas', exp_str, 'first-level.csv', sep="_"))

