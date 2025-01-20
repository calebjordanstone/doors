## K. Garner, 2025 ######################################
#########################################################
# apply second level analysis to the beta values estimated
# at the first level

#################################################
# clear environment and get what you need
rm(list=ls())
library(tidyverse)
source(paste('src-ent/ent_functions.R', sep='/'))

#################################################
# load data
exp_str <- 'ts'
betas <- read.csv(paste('betas', exp_str, 'first-level.csv', 
                      sep='_'))

#################################################
# plot the beta co_efficients by group
betas$train_type <- as.factor(betas$train_type)
levels(betas$train_type) <- c("stable", "variable")

betas %>% ggplot(aes(x=mu, group=train_type, fill=train_type)) +
  geom_density(alpha = 0.5)
betas %>% ggplot(aes(x=sw, group=train_type, fill=train_type)) +
  geom_density(alpha = 0.5)
betas %>% ggplot(aes(x=swr, group=train_type, fill=train_type)) +
  geom_density(alpha = 0.5)
betas %>% ggplot(aes(x=noc, group=train_type, fill=train_type)) +
  geom_density(alpha = 0.5)

#################################################
# all the variables have wild outliers, so am 
# going to remove anything +/- 3sdev above and below the mean
betas <- cbind(betas, betas %>% summarise(mu_mn = mean(mu) - (3*sd(mu)),
                                          mu_mx = mean(mu) + (3*sd(mu)),
                                          sw_mn = mean(sw) - (3*sd(sw)),
                                          sw_mx = mean(sw) + (3*sd(sw)),
                                          swr_mn = mean(swr) - (3*sd(swr)),
                                          swr_mx = mean(swr) + (3*sd(swr)),
                                          noc_mn = mean(noc) - (3*sd(noc)),
                                          noc_mx = mean(noc) + (3*sd(noc))))

flt <- betas %>% mutate( mu_flt = if_else(mu < mu_mx & mu > mu_mn, mu, NA),
                         sw_flt = if_else(sw < sw_mx & sw > sw_mn, sw, NA),
                         swr_flt = if_else(swr < swr_mx & swr > swr_mn, swr, NA),
                         noc_flt = if_else(noc < noc_mx & noc > noc_mn, noc, NA))

flt$train_type <- as.factor(flt$train_type)
levels(flt$train_type) <- c("stable", "variable")
flt %>% ggplot(aes(x=mu_flt, group=train_type, fill=train_type)) +
  geom_density(alpha = 0.5)
flt %>% ggplot(aes(x=sw_flt, group=train_type, fill=train_type)) +
  geom_density(alpha = 0.5)
flt %>% ggplot(aes(x=swr_flt, group=train_type, fill=train_type)) +
  geom_density(alpha = 0.5)
flt %>% ggplot(aes(x=noc_flt, group=train_type, fill=train_type)) +
  geom_density(alpha = 0.5)


#################################################
# compare the beta co-efficients against zero
with(flt, t.test(mu_flt, mu=0)) 
with(flt, t.test(sw_flt, mu=0))
with(flt, t.test(swr_flt, mu=0)) 
with(flt, t.test(noc_flt, mu=0))

# compare the groups
with(flt, t.test(mu_flt ~ train_type))
with(flt, t.test(sw_flt ~ train_type))
with(flt, t.test(swr_flt ~ train_type))
with(flt, t.test(noc_flt ~ train_type))

#################################################
# quick test of the correlation between regressors per group

