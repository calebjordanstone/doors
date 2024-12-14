## replicating Betancourt's hierarchical modelling code
## for 6(5) sided simplex
## K. Garner, 2024
###############################################################
# step 1 = generate data that matches the scenario
rm(list=ls())

library(rstan)
rstan_options(auto_write = TRUE)
library(bayesplot)

N_dice <- 100
# for each dice, generate N observations
N_obs_per_dice <- sample.int(120, N_dice)
# for each dice, now generate that many observations between 1 & 6
outcomes <- unlist(lapply(N_obs_per_dice, function(x) sample(1:6, x, replace = TRUE, prob = rep(1/6, 6))))
die_id <- rep(1:N_dice, times=N_obs_per_dice)

# Betancourt vectors
N <- length(outcomes)
N_dice <- N_dice

data <- list("N" = N,
             "outcome" = outcomes,
             "N_dice" = N_dice,
             "die_idxs" = die_id)

##### set up the model
model <- stan_model("src-dirich/model_files/hierarchical_simplex_algebraic.stan")
fit <- sampling(model, data, iter=2000, chains=4)

## model diagnostics
traceplot(fit, pars = c("q[1,1]"), inc_warmup = FALSE) # e.g.
plot(fit, pars="q[1,1]")
plot(fit, pars="q_baseline")
print(fit)
# also see https://mc-stan.org/rstan/articles/rstan.html for shinystan info

## use Betancourt code to print outcomes of the model
samples <- extract(fit, inc_warmup=FALSE, pars="q") # rows of this will be the iteractions,
# and each column will be the probabilities for one participant, and each slice in the 3D will
# be each outcome. The problem seems to me to be that there is no 'group' level parameter that's
# put out
samples <- extract(fit, inc_warmup=FALSE, pars="q_baseline")
