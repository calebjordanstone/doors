//
// This programme models the null differences as being drawn from a
// normal distribution with a single mu 
// and a single sigma. This is the simplest possible model which
// I will build on for the hierarchical model
//
// But for future reference: https://discourse.mc-stan.org/t/how-to-build-a-simple-hierarchical-model-with-different-samples-in-stan-rstan/29568


// The input data is a vector 'y' of length n, which comes from 'N' subjects.
data {
  int<lower=0> N; // number of subjects
  int<lower=0> n; // length of the data
  vector[n] y; // entropy difference scores
}

// The parameters accepted by the model. Our model accepts
// the parameters mu, tau, and eta
parameters {
  real mu; // population mu
  real<lower=0> tau; // population sd
  vector[N] eta; // subject level error from mu
}

// The parameters that require parameterisation to
// enter into the model
transformed parameters{
  vector[N] theta;  //subject effects
  theta = mu + tau*eta;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  eta ~ normal(0, 1);
  y ~ normal(theta, sigma);
}

