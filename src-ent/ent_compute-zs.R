# K. Garner, 2025
################################################################
# for a given experiment, compute z scores for each participant and
# context, summarise into a data frame, plot, and do the statistical
# testing

rm(list=ls())
library(tidyverse)
exp_str = "ts"

################################################################
# load data
dat <- read.csv(file=paste("../doors-data/data-wrangled/exp", exp_str, 
                          "rnulls.csv", sep="_"))
###############################################################
# for each participant and context, compute z-scores
zdat <- dat %>% group_by(sub, context) %>% summarise(r = r[1],
                                             mu = mean(null),
                                             std = sd(null)) %>%
                                          mutate(z = (r - mu)/std)
rm(dat)
###############################################################
# now add the group variable to the dataframe, and plot the z-scores
# by group and context (although the latter is kinda meaningless)
tmp <- read.csv(file=paste("../doors-data/data-wrangled/exp", exp_str, 
                                    "avg.csv", sep="_"))
tmp <- tmp %>% filter(ses == 2) %>% select(sub, train_type) %>% distinct()
zdat <- inner_join(zdat, tmp, by="sub", relationship = "many-to-many")
rm(tmp)

###############################################################
# now test the z-scores against the null of 0 - do the z-scores
# likely contain no effect
zdat <- zdat %>% group_by(sub, train_type) %>% summarise(mu_z = mean(z))

zdat %>% ggplot(aes(x=mu_z, colour=as.factor(train_type), 
                    fill=as.factor(train_type), group=as.factor(train_type))) +
  geom_histogram(alpha=0.5) + xlim(-300, 10) 

## looks like some outliers so will remove + or - 3 sdevs
zsum <- zdat %>% ungroup() %>% reframe(gmu=mean(mu_z), gsd=sd(mu_z))
zdat <- cbind(zdat, zsum)
zdat_cl <- zdat %>% mutate(mu_z = if_else(mu_z < gmu + 3*gsd & 
                                 mu_z > gmu - 3*gsd, mu_z, NA)) %>%
                        na.omit() 
# lost 3 observations
# save this data file for plotting
write.csv(zdat_cl, file=paste("../doors-data/data-wrangled/exp", 
                              exp_str,
                              "zs_cl.csv", sep="_"))

# now test the z-scores against zero and save results to a csv file
null0 <- t.test(zdat_cl$mu_z, mu=0, alternative="less")

# and against -2
null2 <- t.test(zdat_cl$mu_z, mu=-2, alternative="less")
z_t_stats <- do.call(rbind, lapply(list(null0, null2), function(x) unlist(x)))
write.csv(z_t_stats, file=paste("../doors-data/data-wrangled/exp", 
                                exp_str,
                                "zs_cl_inf-test.csv", sep="_"))
