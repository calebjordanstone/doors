# K. Garner, 2025
# plotting a sample of 20 participants from the variable train type group
# to see the probability of their errors over the course of the training
# session'
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
dat <- dat %>% select(sub, t, context_assign_ent, door_oc, door_nc)

# now pick a sample of subjects
sample_subs <- sample(unique(dat$sub), 20, replace=FALSE)
dat <- dat %>% filter(sub %in% sample_subs)

#############################################################################
# now, for each run in each context, I want to count how many oc or nc errors they
# are, as a function of distance from the last context switch
# to do this, I need to first compute the cumulative distribution function
# first, I will tally up, for each context switch, the number of ocs or ncs
# made at each step

# first work out when each subject had worked out there was a context
# switch
dat$switch_assign <- 0
idx <- c(1, which(!!diff(dat$context_assign_ent))+1)
dat$switch_assign[idx] <- 1

# prepare vectors for mapply
subs <- unique(dat$sub)
cntxts <- unique(dat$context_assign_ent)
subs <- rep(subs, times=length(unique(cntxts)))
cntxts <- rep(cntxts, each=length(subs))

dat <- do.call(rbind, mapply(counts_since_last_switch, subs, cntxts, 
                             MoreArgs = list(dat = dat),
                    SIMPLIFY = FALSE))

# now I can sum across door_oc and door_nc for each door selection since last 
# switch
sw_cnt_dat <- dat %>% group_by(sub, selections_since_last_context_switch) %>% summarise(oc = sum(door_oc),
                                                   nc = sum(door_nc))
get_surv <- function(sw_cnt_dat, subN){
  # get survival function by subject
              tmp <- sw_cnt_dat %>% filter(sub == subN)
              tmp$cum_oc <- cumsum(tmp$oc)
              tmp$cum_nc <- cumsum(tmp$nc)
              tmp <- tmp %>% mutate(oc_p = oc/sum(oc),
                                    oc_cdf = cum_oc/max(cum_oc),
                                    oc_surv = 1 - oc_cdf,
                                    nc_p = nc/sum(nc),
                                    nc_cdf = cum_nc/max(cum_nc),
                                    nc_surv = 1 - nc_cdf)
}
srv_dat <- do.call(rbind, lapply(unique(sw_cnt_dat$sub), 
                                 get_surv, sw_cnt_dat = sw_cnt_dat))
#### now do some plotting
srv_dat %>% ggplot(aes(x=selections_since_last_context_switch,
                       y=oc_cdf)) +
            geom_line() +
            facet_wrap(~sub)

srv_dat %>% ggplot(aes(x=selections_since_last_context_switch,
                       y=nc_cdf)) +
            geom_line() +
            facet_wrap(~sub)


########################################################################
# now plot the trend over the course of the whole session
t_sum <- dat %>% group_by(sub, t) %>% 
            summarise(oc = sum(door_oc),
                      nc = sum(door_nc))

# now work out the cumulative probabilities over the course of 
# the experiments. Would expect to see some humps in it, if the
# context is what is important.


t_sum %>% ggplot(aes(x=t, y=oc)) + geom_line() + facet_wrap(~sub)
t_sum %>% ggplot(aes(x=t, y=nc)) + geom_line() + facet_wrap(~sub)

