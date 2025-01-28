## K. Garner, 2025 ######################################
#########################################################
# apply logistic regression models to subjects, and extract beta
# values

#################################################
# clear environment and get what you need
rm(list=ls())
library(tidyverse)
source(paste('src-ent/ent_functions.R', sep='/'))

###################################################
# settings
trl_flt <- 100 # if you want to filter trials from 
# the analysis
fstm <- '../doors-data/data-wrangled/' # where will you get
# data from (and save it to)
exp_str <- 'ts' # which experiment to analyse

#################################################
# load data
dat <- read.csv(paste(fstm, 'exp_', 
                      exp_str, '_evt.csv', sep='')) %>% 
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
  fit <- glm(door_m ~ scale(Sw) + scale(succss_odds) + 
               scale(cntxt_odds), 
             data=tmp, 
             family=binomial(link="logit")) 
  coef <- fit$coefficients
  tibble(sub = subN,
         train_type = tmp$train_type[1],
         mu = coef['(Intercept)'],
         sw = coef['scale(Sw)'],
         scs = coef['scale(succss_odds)'],
         cntx = coef['scale(cntxt_odds)'])
}

betas <- do.call(rbind, lapply(subs, run_logist, dat=dat %>% filter(t > trl_flt)))
write.csv(betas, paste(fstm, 'exp_', exp_str, '_betas-first-level.csv', sep=""))

