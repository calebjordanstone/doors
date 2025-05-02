library(data.table)
library(lattice)
library(tidyverse)
library(ggplot2)
library(tidyquant)
library(ggthemes)
library(ggpubr)

path <- 'C:/Users/cstone/OneDrive - UNSW/Documents/Projects/honours_projects/data/multitasking/pilot_data'
files <- list.files(path, pattern = '.*(ses-mts).*(beh.tsv)', recursive = T)
dt <- rbindlist(lapply(file.path(path, files), fread), fill = TRUE)

dt[, acc := case_when(
  resp == cresp ~ 1, # if response == correct response, acc = 1
  T ~ 0) # else acc = 0
]

m_acc = dt[, .(MeanAcc = mean(acc)), by = t]

m_acc[, .(MeanAll = mean(MeanAcc),
          StdAll = sd(MeanAcc))]
