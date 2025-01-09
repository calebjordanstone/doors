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

subs <- unique(dat$sub)
dat <- do.call(rbind, lapply(subs, get_context_swch_idx_per_prsn, dat=dat))
dat <- dat %>% select(sub, ses, context_assign_ent, door)

write.csv(dat, file=paste("../doors-data/data-wrangled/exp", str, 
                          "door_selections_for_ent.csv", sep="_"),
          row.names=FALSE)