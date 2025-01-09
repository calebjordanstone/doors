//
// This programme models the null differences as being drawn from a
// normal distribution where the mu is determined by a group level 
// intercept, and an intercept for each participant
// 
// because I have a lot of observations per context, and a fair
// number of contexts, and I know that the lower bound of tau is gonna be above 0.1
// I am going to start with a centred parametrisation
// The input data is a vector 'y' of length n, which comes from 'N' subjects.
// see https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations/3cc1d83dcff96857669ecb653a207b6b2620065f

data {
  int<lower=0> N; // number of subjects
  int<lower=0> n; // length of the data
  int subs<lower=1, upper=N>[n]; // subject indexes for entropy scores
  vector[n] y; // entropy difference scores
}

// The parameters accepted by the model. Our model accepts
// the parameters mu, tau, and eta
parameters {
  real mu; // population mu
  real<lower=0> tau; // population scale
  vector[N] theta_N; // individual group intercepts
  real<lower=0> sigma; // variance of the likelihood
}

model {
  
  // hyperpriors
  mu ~ normal(0, 10);
  tau ~ exponential(1);
  
  // prior
  sigma ~ normal(0,5);
  
  // population model and likelihood
  theta_k ~ normal(mu, tau)
  for (i in 1:n){
    y[n] ~ normal(theta_N[subs[i]], sigma)
  }
}

// Generate data according to the hierarchical regression.
generated quantities {
  vector[n] d;                    // vector of observations
  vector[N] theta_N;              // vector of sub intercepts

  // draw parameter values and generate data.
  for (k in 1:N) {
    theta_N[k] = normal_rng(mu, tau);
  }
  for (j in 1:N) {
    d[j] = normal_rng(theta_N[subs[j]], sigma);
  }
}