###########################################################
# K. Garner, 2025. 
# For each experiment, plot a histogram of the cleaned
# z-scores, along side that of the random agent.
###########################################################
rm(list=ls())

# stuff you need and settings
library(tidyverse)
exp_strs <- c('lt', 'ts')
fstem <- '../doors-data/'
col_scheme <- c('#1b9e77','#d95f02', '#7570b3')
names(col_scheme) <- c(exp_strs, "agent")
col_scheme <- unlist(lapply(col_scheme, adjustcolor, alpha.f=0.5))

p_wdth <- 10 # plot width, in cm
p_hgt <- 6

###########################################################
# load data from each exp
zdat <- do.call(rbind, lapply(exp_strs, function(x) {
  dat <- read.csv(file=paste(fstem, 'data-wrangled/', 'exp_', x, '_zs_cl.csv', sep=""))
  dat$exp <- x
  dat
}))

# load agent data
load(paste(fstem, 'sims/random-agent_z-score-analysis.Rds', sep=""))

###########################################################
# plot histograms
##########################################################
# for manuscripts
pdf(paste(fstem, 'figs/z-hsts.pdf', sep=""), 
          width = p_wdth/2.54, height = p_hgt/2.54) 
par(mfrow = c(1,2), mar = c(4, 4, 2, 1), las=2, cex=2/3)


##########################################################
# for talks
pdf(paste(fstem, 'figs/z-hsts_talk.pdf', sep=""), # for talks
          width = p_wdth/2.54*2.5, height = p_hgt/2.54*2.5)
par(mfrow = c(1,2), mar = c(4, 4, 2, 1), las=2, cex=1.5)

##########################################################
hist(with(zdat, mu_z[exp == exp_strs[1]]), 
     probability=TRUE,
     col = col_scheme[exp_strs[1]], 
     xlim = c(min(zdat$mu_z), max(zs)),
     ylim = c(0,0.08),
     xlab = "z", ylab = "freq",
     main = "humans",
     xaxt = "n",
     yaxt = "n")
axis(1, at = seq(-100, 0, by = 50))
axis(2, at = seq(0, 0.08, by = 0.04))
hist(with(zdat, mu_z[exp == exp_strs[2]]), probability=TRUE, 
     col=col_scheme[exp_strs[2]], add=T)
# add legend

hist(zs, probability = TRUE,
     col = col_scheme["agent"],
     xlim = c(min(zdat$mu_z), max(zs)),
     xlab = "z", ylab = "",
     main="random agent",
     xaxt = "n",
     yaxt = "n")
axis(1, at = seq(-100, 0, by = 50))
axis(2, at = seq(0, 0.4, by = 0.2))
dev.off()
