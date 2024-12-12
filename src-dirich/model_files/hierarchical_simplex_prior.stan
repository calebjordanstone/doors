transformed data {
	vector[6] ones = rep_vector(1, 6);
}

generated quantities {
	real zeta = abs(normal_rng(0, 5));
	vector[6] q_baseline = dirichlet_rng(7.5 * ones); # reflects domain knowledge on the baseline
	vector[6] delta = dirichlet_rng(ones); # suitable prior over each of q outcomes
	vector[6] q = (1 / (1 + zeta )) * q_baseline 
	            + (zeta / (1 + zeta )) * delta # make sure you can add the two and respect the simplex
}
