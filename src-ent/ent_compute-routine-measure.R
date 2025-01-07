######################################################################
## K. Garner (2025) - use initially wrangled data to compute transition
## matrices and entropy scores
######################################################################

rm(list=ls())
library(tidyverse)

str <- 'ts' ## experiment to get scores for

######################################################################
## functions
######################################################################
H <- function(x){
  # to be applied over the rows of the matrix of transitions
  -sum(x * log(x), na.rm = TRUE)
}

data_2_counts_matrix <- function(data, n_doors){
  # turn data into a counts matrix of transitions made
  mat <- matrix(rep(0, times=n_doors*n_doors), nrow=n_doors, ncol=n_doors)
  idxs <- matrix(c(data[1:length(data)-1], data[2:length(data)]), nrow=2, byrow=TRUE)
  for(i in 1:ncol(idxs)){
    mat[idxs[1,i],idxs[2,i]] <- mat[idxs[1,i],idxs[2,i]] + 1
  }
  mat
}

p_st1_gs <- function(counts_matrix, n_doors){
  # convert the counts matrix into the row probabilities
  denom <- matrix(rep(rowSums(counts_matrix), n_doors), nrow=n_doors, byrow=FALSE)
  out <- counts_matrix / denom
  out[is.na(out)] = 0
  out
}

######################################################################
## load data
######################################################################
dat <- read.csv(paste("../doors-data/data-wrangled/exp", str, 
                          "door_selections_for_ent.csv", sep="_"))

### compute entropy score/routine measure for each subject
subs <- unique(dat$sub)
cntxts <- unique(dat$context_assign_ent)
subs <- rep(subs, times=length(cntxts))
cntxts <- rep(cntxts, each = max(subs))

compute_ent_by_sub <- function(subN, cntxN, dat, n_doors = 16){
  # compute routine score and create a mini tibble for a given subject
  # and context
    tmp <- dat %>% filter(sub == subN & context_assign_ent == cntxN)
    door_dat <- tmp$door
    count_mat <- data_2_counts_matrix(data = door_dat, n_doors)
    probs <- p_st1_gs(counts_matrix = count_mat, n_doors = n_doors)
    ent <- sum(apply(probs, 1, H))
    tibble(sub = subN, context = cntxN, r = ent)
}

r_dat <- do.call(rbind, mapply(compute_ent_by_sub, subs, cntxts, MoreArgs = list(dat=dat),
            SIMPLIFY = FALSE))
plot(x=r_dat$r[r_dat$context == 1], y=r_dat$r[r_dat$context == 2]) # very similar so will average
r_dat <- r_dat %>% group_by(sub) %>% summarise(r = mean(r)) %>% ungroup()

write.csv(r_dat, file=paste("../doors-data/data-wrangled/exp", str, 
                               "rscore.csv", sep="_"),
          row.names=FALSE)
