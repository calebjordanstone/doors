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
betas <- read.csv(paste('src-ent/betas', exp_str, 'first-level.csv', 
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
betas %>% ggplot(aes(x=scs, group=train_type, fill=train_type)) +
  geom_density(alpha = 0.5)
betas %>% ggplot(aes(x=cntx, group=train_type, fill=train_type)) +
  geom_density(alpha = 0.5)

# do specific cleaning
source(paste('src-ent/ent_', exp_str, '-beta-clean.R', sep=""))

sd_adj = 3
betas <- cbind(betas, betas %>% summarise(mu_mn = mean(mu) - (sd_adj*sd(mu)),
                                          mu_mx = mean(mu) + (sd_adj*sd(mu)),
                                          sw_mn = mean(sw) - (sd_adj*sd(sw)),
                                          sw_mx = mean(sw) + (sd_adj*sd(sw)),
                                          swr_mn = mean(swr) - (sd_adj*sd(swr)),
                                          swr_mx = mean(swr) + (sd_adj*sd(swr)),
                                          scs_mn = mean(scs) - (sd_adj*sd(scs)),
                                          scs_mx = mean(scs) + (sd_adj*sd(scs)),
                                          cntx_mn = mean(cntx) - (sd_adj*sd(cntx)),
                                          cntx_mx = mean(cntx) + (sd_adj*sd(cntx))))

flt <- betas %>% mutate( mu_flt = if_else(mu < mu_mx & mu > mu_mn, mu, NA),
                         sw_flt = if_else(sw < sw_mx & sw > sw_mn, sw, NA),
                         swr_flt = if_else(swr < swr_mx & swr > swr_mn, swr, NA),
                         scs_flt = if_else(scs < scs_mx & scs > scs_mn, scs, NA),
                         cntx_flt = if_else(cntx < cntx_mx & cntx > cntx_mn, cntx, NA))

flt$train_type <- as.factor(flt$train_type)
levels(flt$train_type) <- c("stable", "variable")
flt %>% ggplot(aes(x=mu_flt, group=train_type, fill=train_type)) +
  geom_histogram(alpha = 0.5)
flt %>% ggplot(aes(x=sw_flt, group=train_type, fill=train_type)) +
  geom_histogram(alpha = 0.5)
flt %>% ggplot(aes(x=swr_flt, group=train_type, fill=train_type)) +
  geom_histogram(alpha = 0.5)
flt %>% ggplot(aes(x=scs_flt, group=train_type, fill=train_type)) +
  geom_histogram(alpha = 0.5)
flt %>% ggplot(aes(x=cntx_flt, group=train_type, fill=train_type)) +
  geom_histogram(alpha = 0.5)

#################################################
# compare the beta co-efficients against zero
with(flt, t.test(mu_flt, mu=0)) 
with(flt, t.test(sw_flt, mu=0))
with(flt, t.test(swr_flt, mu=0)) 
with(flt, t.test(scs_flt, mu=0))
with(flt, t.test(cntx_flt, mu=0))

# compare the groups
with(flt, t.test(mu_flt ~ train_type))
with(flt, t.test(sw_flt ~ train_type))
with(flt, t.test(swr_flt ~ train_type))
with(flt, t.test(scs_flt ~ train_type))
with(flt, t.test(cntx_flt ~ train_type))


