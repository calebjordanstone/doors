# K. Garner, 2025
# plotting a sample of 20 participants from the variable train type group
# to see the probability of their errors over the course of the training
# session
#############################################################################
rm(list=ls())
library(tidyverse)
exp_str = "lt"

###################################################################
# some functions to help
source("src-ent/ent_functions.R")

#############################################################################
# get data
dat <- read.csv(paste("../doors-data/data-wrangled/exp", exp_str, 
                           "evt.csv", sep="_"))
dat <- dat %>% filter(ses == 2 & train_type == 2)
dat$context_assign_ent <- dat$context # this is for sorting the data as defined
# below
## step 3: assign the data to contexts
subs <- unique(dat$sub)
dat <- do.call(rbind, lapply(subs, get_context_swch_idx_per_prsn, dat=dat))
dat <- dat %>% select(sub, t, context, door_oc, door_nc)

#############################################################################
# now just plot a sample of 20 participants
subs <- sample(dat$sub, size=1)
plt_dat <- dat %>% filter(sub %in% subs) %>% group_by(sub, context, t) %>% 
               summarise(oc = sum(door_oc), nc = sum(door_nc))
plt_dat %>% ggplot(aes(x=t, y=oc, colour=as.factor(context))) + geom_line() +
  facet_wrap(~sub)
plt_dat %>% ggplot(aes(x=t, y=nc, colour=context)) + geom_line() +
  facet_wrap(~sub)
