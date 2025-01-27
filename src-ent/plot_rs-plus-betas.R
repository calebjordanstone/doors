######################################################################
## K. Garner (2025) - plot rs, p(o-context modelling, and betas)
######################################################################

rm(list=ls())
library(tidyverse)
library(vioplot)

# stuff you need and settings
exp_strs <- c('lt', 'ts')
fstem <- '../doors-data/'
col_scheme <- c('#a6cee3','#1f78b4')
names(col_scheme) <- c(exp_strs)
col_scheme <- unlist(lapply(col_scheme, adjustcolor, alpha.f=0.5))

p_wdth <- 10 # plot width, in cm
p_hgt <- 6

###########################################################
# load r data from each exp
rdat <- do.call(rbind, lapply(exp_strs, function(x) {
  # get r scores
  dat <- read.csv(file=paste(fstem, 'data-wrangled/', 'exp_', x, '_rscore.csv', sep=""))
  dat$exp <- x
  # add training group
  dat <- inner_join(dat, read.csv(file=paste(fstem, 'data-wrangled/', 'exp_', x, '_avg.csv', sep="")) %>% 
                      filter(ses == 2) %>% select(sub, train_type) %>% distinct(),
                    by="sub")
  dat
}))




###########################################################
# plot the data by group and experiment
par(mfrow=c(2,2), las=2)
with(rdat, vioplot(r[exp == exp_strs[[1]] & train_type == 1],
                   r[exp == exp_strs[[1]] & train_type == 2],
     col=col_scheme, names=c("S", "V"),
     axes=F, ylim=c(0,35),
     yaxt='n',
     ylab='freq',
     xlab='r'))
axis(2, at = seq(0, 35, by = 15))

with(rdat, vioplot(r[exp == exp_strs[[2]] & train_type == 1],
                   r[exp == exp_strs[[2]] & train_type == 2],
                   col=col_scheme, names=c("S", "V"),
                   axes=F, ylim=c(0,35)))
