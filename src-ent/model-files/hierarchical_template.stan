data {
   int N_districts;
   int N_schools;
   int N_students;
   int school_in_district_idx[N_schools];
   int student_in_school_idx[N_students];
   vector[N_students] score;
}

parameters {
  real top_mu;
  real<lower=0> top_sigma;

  vector[N_districts] district_mu;
  vector<lower=0>[N_districts] district_sigma;
 
  vector[N_schools] school_mu;
  vector<lower=0>[N_schools] school_sigma;

  vector[N_students] student_mu;
  real<lower=0> sigma;
}

model {
  
  top_mu ~ std_normal()
  top_sigma ~ std_normal()
  district_mu ~ normal(top_mu, top_sigma);

  for (s in 1:N_schools) {
     int idx = school_in_district_idx[s];
     school_mu[s] ~ normal(district_mu[idx],district_sigma[idx]);
  }

  for (k in 1:N_students) {
    int idx = student_in_school_idx[k];
    student_mu[k] ~ normal(school_mu[idx],school_sigma[idx]);
  }
score ~ normal(student_mu, sigma);
}
