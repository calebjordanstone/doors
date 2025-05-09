###########################################################
# K. Garner, 2025. 
# get the data from the dopamine experiment, compute
# routine scores and correlate with mindfulness scores
###########################################################
rm(list=ls())
library(tidyverse)
library(GGally)
source('src-ent/ent_functions.R')

# ----------------------------------------------------------------------------
# functions
# ---------------------------------------------------------------------------
# the tricky thing with this data is that I have to create the counts matrices
# by trial and then sum them to make one matrix
sum_counts_across_trials <- function(dat, subN, drugN, condN, n_doors = 16){
  
  tmp <- dat %>% filter(sub == subN & drug == drugN & cond == condN )
  n_trials <- length(unique(tmp$t))
  trials <- unique(tmp$t)
  counts <- array(0, c(n_doors, n_doors, n_trials))
  
  for (i in 1:n_trials){
    counts[,,i] <- data_2_counts_matrix(tmp$door[tmp$t == trials[i]], n_doors = 16)
  }
  counts <- apply(counts, c(1,2), sum)
  counts
}

get_Rs <- function(dat, subN, drugN, condN){
  # for each subject, get routine scores by condition and session
  
  counts_mat <- sum_counts_across_trials(dat=dat,
                                         subN=subN,
                                         drugN=drugN,
                                         condN=condN)
  p_mat <- p_st1_gs(counts_mat, n_doors = 16)
  r_score = sum(apply(p_mat, 1, H))
  tibble(sub=subN, cond=condN, drug=drugN, r=r_score)
}

# ----------------------------------------------------------------------------
# load data
# ---------------------------------------------------------------------------
proj_fstem <- '../da-data/data/derivatives/'
load(paste(proj_fstem, 'dat4_seq_model.Rda', sep=""))

# ----------------------------------------------------------------------------
# compute R's
# ---------------------------------------------------------------------------
total_subs <- length(unique(blocked_dat$sub))
subNs <- rep(unique(blocked_dat$sub), times=4)
drugNs <- rep(unique(blocked_dat$drug), each=total_subs*2)
condNs <- rep(unique(blocked_dat$cond), each=total_subs, times=2)
r_dat <- do.call(rbind, mapply(get_Rs, subN=subNs, drugN=drugNs, condN=condNs, 
                      MoreArgs=list(dat=blocked_dat), SIMPLIFY=FALSE))


r_dat <- r_dat %>% group_by(sub, drug) %>% summarise(r = mean(r)) %>% ungroup()
r_dat %>% ggplot(aes(x=r, group=drug, fill=drug)) + geom_density(alpha=0.5)
with(r_dat, t.test(r[drug=="placebo"], r[drug=="levodopa"], paired=TRUE))
# pretty similar, so I will average over them
#r_dat <- r_dat %>% group_by(sub) %>% summarise(r = mean(r))

# now load mindfulness and scores
load(paste(proj_fstem, 'mind_scores.Rda', sep=""))
load(paste(proj_fstem, 'bis_scores.Rda', sep="")) 

r_dat <- inner_join(r_dat, mind_sum, by="sub")
r_dat <- inner_join(r_dat, bis_sum, by="sub")
#r_dat <- r_dat %>% mutate(r = log(r))

ggpairs(r_dat %>% select(r, m, bis),
        mapping=ggplot2::aes(colour = r_dat$drug))
