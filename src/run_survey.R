# load data
library(tidyverse)
library(stringr)
library(magrittr)

# settings
version <- "pilot-data" 
exp <- "flexibility" # or "multitasking

# paths
project_path <- getwd()
if (!dir.exists(file.path(project_path, "res"))) {
  # check that the results directory exists. if it doesn't, create it.
  dir.create(file.path(project_path, "res"))
}
file_path <- "C:/Users/cstone/OneDrive - UNSW/Documents/Projects/honours_projects/data"
exp_path <- str_glue("/{exp}/{version}")
data_path <- file.path(file_path + exp_path)
if (!dir.exists(data_path)) {
  stop(paste0(data_path, " does not exist"))
}

# load data
data <- read.csv(paste0(data_path, '/FFMQ.csv'))
data %<>% rename(SUBID = SUBID_1)

# subset data
dat <- data[-c(1,2), ] %>% select(c(SUBID, starts_with('FFMQ')))

# convert text labels to numeric scores for all response variables
dat %<>% mutate(
  across(
    starts_with('FFMQ'), ~
    case_when( 
      . == "Never or very rarely true" ~ 1,
      . == "Rarely true" ~ 2,
      . == "Sometimes true" ~ 3,
      . == "Often true" ~ 4,
      . == "Very often or always true" ~ 5
      )
    )
  )

dat %>%
  group_by(SUBID) %>%
  summarise(Mean = sum(c_across(FFMQ_B1_1:FFMQ_B2_19)))


            
  

