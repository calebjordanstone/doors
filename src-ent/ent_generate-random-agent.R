# K. Garner, 2025
# because people were sooooo significant, we are generating an entirely
# random agent to complete an example of the task, and then performing
# the permuted null and z-score, over 1000 iterations, to check what we
# get
#######################################################################
rm(list=ls())
set.seed(42)

source('src-ent/ent_functions.R')
library(tidyverse)

#######################################################################
# run simulation
n_samples <- 1000 # how many times we will generate random responses from
# the perfect random agent, generate a null, and compute the z score

zs <- replicate(n_samples, null_z_for_random_agent())

#######################################################################
# save results
save(zs, file="doors-data/sims/random-agent_z-score-analysis.Rds")

#######################################################################
# plot histogram
pdf(file="doors-data/sims/random-agent_z-score-null.pdf")
hist(zs)
dev.off()
