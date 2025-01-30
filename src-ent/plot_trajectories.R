#########################################################
# K. Garner 2025
# plot participant trajectories (1 high routine 1 low routine)
#########################################################
########################################################
# get helpful things
#######################################################
rm(list=ls())
library(tidyverse)
library(gridExtra)
#source('fig_label.R') # maybe later

#figinfo = 's'
w <- 20 # in cm
h <- 20 # in cm

fstem <- '../doors-data/'
exp_str <- 'lt'
########################################################
# get load data
#######################################################
# now add 2 x 5 search routines on the bottom row
all_dat <- read.csv(paste(fstem, 'data-wrangled/exp_', exp_str, '_evt.csv', sep=""))

xs <- rep(c(1, 2, 3, 4), times = 4)
ys <- rep(c(4, 3, 2, 1), each = 4)

draw_trajectory <- function(dat, cntxt, xs, ys, obs){
  # dat - data frame for one participant from session 2
  # cntxt [int] - context to plot
  # xs = x co-ords of doors
  # ys = y co-ords of doors
  # obs = a vector containing the row numbers of the dataframe that we 
  # want to plot
  # set up the door locations/co-ordinates
  dat$door_x <- xs[dat$door]
  dat$door_y <- ys[dat$door]
#  dat$t <- as.factor(dat$t) # make trial a factor for
  # indexing later
  
  #### set up the door dataframe for colouring things in
  doors <- data.frame(xs = rep(c(1:4), times=4),
                      ys = rep(c(4:1), each=4))
  # now I need to find which are the target doors for the 
  # context from which the trials are taken
  # first, find the last row of every trial
  tgts <- dat$door[which(diff(dat$t) > 0)]
  cntxts <- dat$context[which(diff(dat$t) > 0)]
  trl_tgts <- unique(tgts[cntxts == cntxt])
  
  doors$tgt <- "ntgt"
  doors$tgt[trl_tgts] <- "tgt"
  doors$tgt[unique(tgts[cntxts != cntxt])] <- "otgt"
  
  # now select the observations you want to plot
  p <- dat[obs, ] %>% ggplot(aes(x=door_x, y=door_y)) +
    geom_path(arrow = arrow(length=unit(0.2, "cm")), alpha=0.75) +
    geom_point(alpha=0.75) + xlim(0.5, 4.5) + ylim(0.5, 4.5) +
    geom_point(inherit.aes = FALSE, 
               data = doors,
               aes(x = xs,
                   y = ys,
                   colour = tgt), size = 4, alpha = .3) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(legend.position = 'none')
  p
}

#########
# first filter the data for the second session and pick some subjects
############
subs <- c(1, 10, 61, 64)
ses_dat <- all_dat %>% filter(ses == 2)
cntx = 1

# change sub 1 arrows so that they are all going the right way
sub1_idxs <- list(c(179:183), c(247:251), c(400:404), c(514:518), c(890:894))
sub1_ps <- lapply(sub1_idxs, draw_trajectory, 
                    dat = ses_dat %>% filter(sub == 1),
                    xs = xs, ys=ys, cntxt = cntx)

sub10_idxs <- list(c(233:239), c(998:1005), c(1035:1044), c(1392:1399), c(1413:1417))
sub10_ps <- lapply(sub10_idxs, draw_trajectory, 
              dat = ses_dat %>% filter(sub == 10),
              xs = xs, ys=ys, cntxt = 3-cntx)

all_ps <- c(sub1_ps, sub10_ps)
nsubs = 2

tracs <- grid.arrange(all_ps[[1]], all_ps[[2]],
                      all_ps[[3]], all_ps[[4]],
                      all_ps[[5]], all_ps[[6]],
                      all_ps[[7]], all_ps[[8]],
                      all_ps[[9]], all_ps[[10]],
                      nrow=nsubs, ncol=length(all_ps)/nsubs,
                      widths = rep(2.5, length(all_ps)/nsubs), 
                      heights = rep(2.5, nsubs),
                      left = "sub",
                      top = "trials")
ggsave(paste(fstem, "figs/trajectories2subs_4talk.pdf", sep=""), tracs, 
       width = w+.5, height = (w/6*2)+.5, units="cm")