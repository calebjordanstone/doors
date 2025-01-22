## K. Garner, 2025 ######################################
#########################################################
# apply second level analysis to the beta values estimated
# at the first level

#################################################
# clear environment and get what you need
rm(list=ls())
library(tidyverse)
library(GGally)
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
betas %>% ggplot(aes(x=scs, group=train_type, fill=train_type)) +
  geom_density(alpha = 0.5)
betas %>% ggplot(aes(x=cntx, group=train_type, fill=train_type)) +
  geom_density(alpha = 0.5)

# # do specific cleaning ### now not necessary as more stable estimates
# if (exp_str == 'ts'){
#  source(paste('src-ent/ent_', exp_str, '-beta-clean.R', sep=""))
# }

sd_adj = 3
betas <- cbind(betas, betas %>% summarise(mu_mn = mean(mu) - (sd_adj*sd(mu)),
                                          mu_mx = mean(mu) + (sd_adj*sd(mu)),
                                          sw_mn = mean(sw) - (sd_adj*sd(sw)),
                                          sw_mx = mean(sw) + (sd_adj*sd(sw)),
                                          scs_mn = mean(scs) - (sd_adj*sd(scs)),
                                          scs_mx = mean(scs) + (sd_adj*sd(scs)),
                                          cntx_mn = mean(cntx) - (sd_adj*sd(cntx)),
                                          cntx_mx = mean(cntx) + (sd_adj*sd(cntx))))

flt <- betas %>% mutate( mu_flt = if_else(mu < mu_mx & mu > mu_mn, mu, NA),
                         sw_flt = if_else(sw < sw_mx & sw > sw_mn, sw, NA),
                         scs_flt = if_else(scs < scs_mx & scs > scs_mn, scs, NA),
                         cntx_flt = if_else(cntx < cntx_mx & cntx > cntx_mn, cntx, NA))

flt$train_type <- as.factor(flt$train_type)
levels(flt$train_type) <- c("stable", "variable")
flt %>% ggplot(aes(x=mu_flt, group=train_type, fill=train_type)) +
  geom_density(alpha = 0.5)
flt %>% ggplot(aes(x=sw_flt, group=train_type, fill=train_type)) +
  geom_density(alpha = 0.5)
flt %>% ggplot(aes(x=scs_flt, group=train_type, fill=train_type)) +
  geom_density(alpha = 0.5)
flt %>% ggplot(aes(x=cntx_flt, group=train_type, fill=train_type)) +
  geom_density(alpha = 0.5)

#################################################
# compare the beta co-efficients against zero
with(flt, t.test(mu_flt, mu=0)) 
with(flt, t.test(sw_flt, mu=0))
with(flt, t.test(scs_flt, mu=0))
with(flt, t.test(cntx_flt, mu=0))

# compare the groups
with(flt, t.test(mu_flt ~ train_type))
with(flt, t.test(scs_flt ~ train_type))
with(flt, t.test(cntx_flt ~ train_type))

#################################################
# now, correlate the regressors with each other
# and with the routine score
# first get the routine score
R <- read.csv(file=paste("../doors-data/data-wrangled/exp", exp_str, 
                            "rscore-full.csv", sep="_"))
# first get an average routine score
R <- R %>% group_by(sub) %>% summarise(mur = mean(r))
flt <- inner_join(flt, R, by="sub")
# remove rows with nas
flt <- flt %>% na.omit()

# first, check for multivariate outliers
mhl.mat <- as.matrix(flt[,c("mu_flt", "scs_flt", "cntx_flt", "mur")]) 
mhl.cov <- cov(mhl.mat) # here I get the covariance matrix
mhl.dist <- mahalanobis(mhl.mat, colMeans(mhl.mat), mhl.cov) # now calc the M dist
hist(mhl.dist, breaks = 20)
sprintf("For a Mahalanobis to be less that .1 per cent likely to have occured by chance, given our degrees of feedom (%f), it has to be a value greater than %f", length(mhl.dist)-1, qchisq(.001, df=length(mhl.dist)-1))

#### learning transfer - we therefore remove the participants with a mahalanobis distance
#### greater than 56.47
rm_sbs <- as.numeric(names(mhl.dist)[which(mhl.dist > qchisq(.001, df=length(mhl.dist)-1))])
flt <- flt %>% filter(!sub %in% rm_sbs)
flt <- flt %>% mutate(mur_l = log(mur))
ggpairs(flt %>% select(mu_flt, scs_flt, cntx_flt, mur_l),
        mapping=ggplot2::aes(colour = flt$train_type),
        upper = list(continuous = wrap("cor", method = "spearman")))
### now do a pairs plot
