######################################################################
# K. Garner 2025
# plot participant's transition matrices
######################################################################
# first, load things I need
rm(list=ls())

library(tidyverse)
library(RColorBrewer)
source("src-ent/ent_functions.R")

fstem <- '../doors-data/'
exp_str <- 'lt'
mat_cols <- colorRampPalette(brewer.pal(5, "PuBu"))(200)
p_wdth <- 20
p_hgt <- 19
######################################################################
# now get the data
dat <- read.csv(paste(fstem, 'data-wrangled/exp_', exp_str, '_evt.csv', sep="")) %>%
             filter(ses == 2)

# now select a subject and context and compute their transition prob matrix
subN <- 1
cntxt <- 1
sub_dat <- dat %>% filter(sub == subN & context == cntxt)
counts_mat <- data_2_counts_matrix(sub_dat$door, n_doors = 16)
pst_gcs <- p_st1_gs(counts_mat, n_doors = 16)

heatmap(pst_gcs, Rowv=NA, Colv="Rowv", scale="none", 
        col=mat_cols)

######################################################################
# toy model subject for talk

path_taken <- c(4, 11, 14, 2, 4, 2, 14, 11, 4, 2, 14, 11, 4,
                2, 14, 11, 4, 2, 14, 11, 4)
model_counts <- data_2_counts_matrix(rep(path_taken, times = 100), 
                                     n_doors = 16)
model_pst_gcs <- p_st1_gs(model_counts, n_doors = 16)
rownames(model_counts) = paste(1:16, sep=",")
rownames(model_pst_gcs) = rownames(model_counts)

pdf(paste(fstem, 'figs/transition-matrix.pdf', sep=""), 
    width = p_wdth/2.54, height = p_hgt/2.54) 
pheatmap(model_pst_gcs, color=mat_cols, cluster_rows=FALSE,
         cluster_cols=FALSE, legend=FALSE, show_rownames=TRUE,
         fontsize_row=12)
dev.off()
