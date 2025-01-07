######################################################################
## K. Garner (2025) - initial data wrangling to get transition matrices and 
## compute routine scores
######################################################################
rm(list=ls())
library(tidyverse)

### step 1: read in the data files from the appropriate experiment
str <- 'ts'
dat <- read.csv(paste("../doors-data/data-wrangled/exp", str, 
                      "evt.csv", sep="_"))

#################################################################

## step 2: filter to the training session
dat <- dat %>% filter(ses == 2)
dat$context_assign_ent <- dat$context # this is for sorting the data as defined
# below
## step 3: assign the data to contexts

get_context_swch_idx_per_prsn <- function(dat, subN){
  # ugly function to do the sorting/splitting of data as follows:
  ## now how to split the data up? First, by context.
  ## Second, we don't want to artificially inflate the routine from
  ## each context with the times that the context has switched but 
  ## the participant still thinks they are in the other context.
  ## So, I will take the data:
  ## when there is a context switch
  ##     -- find next trial where cc is a 1. 
  ##     -- trials between switch and 1 go to previous context
  ##     -- trials cc to end go to current context  
  print(paste("sub =", subN))
  tmp <- dat %>% filter(sub == subN)
  
  # first find context switches
  # select current context is the variable that breaks the data how I want
  swch_idx <- which(diff(tmp$context_assign_ent) != 0) + 1 # get points at which 
  # there is a context switch
  # now add the last trial
  swch_idx <- c(swch_idx, nrow(tmp))
  # now for each switch index row, find the next row that has a 1 in the 
  # door_cc column
  # first make the swch_idx for the starts and ends
  nswch = length(swch_idx)
  swch_strs <- swch_idx[1:nswch-1]
  swch_ends <- swch_idx[2:nswch]

  # get indexes for where the new context number should be replaced with the old one
  fll_idxs <- rep(0, length(swch_strs))
  for (i in 1:length(swch_strs)){
    fll_idxs[i] <- swch_strs[i] + which(diff(tmp[swch_strs[i]:(swch_ends[i]-1), "door_cc"]) > 0 )[1] - 1
  }       
  rms <- which(is.na(fll_idxs))  
  if (any(rms)){
    swch_strs <- swch_strs[-rms]
    fll_idxs <- fll_idxs[-rms]
  }
  for (i in 1:length(swch_strs)){
    tmp$context_assign_ent[swch_strs[i]:fll_idxs[i]] <- tmp$context_assign_ent[swch_strs[i]-1]
  }
  
  tmp
}

subs <- unique(dat$sub)
dat <- do.call(rbind, lapply(subs, get_context_swch_idx_per_prsn, dat=dat))
dat <- dat %>% select(sub, ses, context_assign_ent, door)

write.csv(dat, file=paste("../doors-data/data-wrangled/exp", str, 
                          "door_selections_for_ent.csv", sep="_"),
          row.names=FALSE)