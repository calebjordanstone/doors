//
// This programme models the null differences as being drawn from a
// normal distribution where the mu is determined by a group level 
// intercept, and an intercept for each participant
// 
// But for future reference: https://discourse.mc-stan.org/t/how-to-build-a-simple-hierarchical-model-with-different-samples-in-stan-rstan/29568
// https://discourse.mc-stan.org/t/what-does-non-centered-parameterization-actually-do-how-to-interpret-model-brms/22266
// https://discourse.mc-stan.org/t/partial-non-centered-parametrizations-in-stan/7104
// https://discourse.mc-stan.org/t/choosing-correct-non-centered-parametrization/18306
// https://github.com/paul-buerkner/brms/issues/1162

// The input data is a vector 'y' of length n, which comes from 'N' subjects.
data {
  int<lower=0> N; // number of subjects
  int<lower=0> n; // length of the data
  int subs[n]; // subject indexes for entropy scores
  vector[n] y; // entropy difference scores

}

// The parameters accepted by the model. Our model accepts
// the parameters mu, tau, and eta
parameters {
  real mu; // population mu
  real<lower=0> sigma; // population sd
  
  vector[N] sub_mu; // subject sd
  vector<lower=0>[N] sub_sigma; // subject level sigma
}

// The parameters that require parameterisation to
// enter into the model
// transformed parameters{
//  vector[N] theta;  //subject effects
//  theta = mu + tau*eta;
// }

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  
  mu ~ std_normal();
  sigma ~ std_normal();
  
  for (i in 1:N){
    sub_mu[N] ~ normal(mu, sigma);
    sub_sigma
  }
  y ~ normal(sub_mu, sigma);
}

