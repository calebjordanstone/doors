# K. Garner 2025
# test whether routines are above what we would expect by chance
###################################################################
rm(list=ls())
library(tidyverse)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

##################################################################
# first, load data and set up for stan
exp_str <- 'lt'
dat <- read.csv(paste('../doors-data/data-wrangled/exp',
                      exp_str, 'rnulls.csv', sep='_'))

data <- list("y" = dat$null_diff, 
             "n" = length(dat$null_diff),
             "N" = length(unique(dat$sub)),
             "subs" = dat$sub)

##################################################################
# now run the model
model <- stan_model("src-ent/model-files/hierarchical_normal_cp.stan")
fit <- sampling(model, data, iter=1000, chains=4)
traceplot(fit)
