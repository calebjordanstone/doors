# K. Garner, 2025
################################################################
# for a given experiment, compute z scores for each participant and
# context, summarise into a data frame, plot, and do the statistical
# testing

rm(list=ls())
library(tidyverse)
exp_str = "lt"

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
zdat %>% ggplot(aes(x=z, colour=as.factor(train_type), 
                    fill=as.factor(train_type), group=as.factor(train_type))) +
  geom_histogram(alpha=0.5) + xlim(-300, 10) + facet_wrap(~context)

###############################################################
# now test the z-scores against zero
t.test(zdat$z[zdat$context == 1], mu=0, alternative="less")
t.test(zdat$z[zdat$context == 2], mu=0, alternative="less")
# and against -2
t.test(zdat$z[zdat$context == 1], mu=-2, alternative="less")
t.test(zdat$z[zdat$context == 2], mu=-2, alternative="less")

