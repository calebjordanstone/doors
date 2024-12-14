data {
	int<lower=1> N; // Number of rolls aka length of data
	array[N] int<lower=1, upper=6> outcome; // Roll outcomes, the outcome for each row of data
	int<lower=1> N_dice; // Number of dice - aka this would be the number of participants
	array[N] int<lower=1, upper=N_dice> die_idxs; // Die rolled - this would be the subject id for each row of data
}

transformed data {
// Compute outcome totals
	array[6] int counts = rep_array(0, 6); // an array of size 6 with all values set to 0
	array[N_dice, 6] int counts_per_die; // matrix, number of dice as rows, outcomes as columns
	array[N_dice] int N_per_die; // an array that is 1, n_dice, with the total number of counts per dice
	
	for (n in 1:N) {
	counts[outcome[n]] += 1; // counts is the tally of outcomes, outcomes is the list of actual outcomes, n is the datapoint, therefore this is simply collating the total number of each outcome
	}
	
	for (i in 1:N_dice) {
		counts_per_die[i] = rep_array(0, 6);
		for (n in 1:N) {
			counts_per_die[die_idxs[n], outcome[n]] += 1; // collate per die
		}
		N_per_die[i] = sum(counts_per_die[i]); // get the total number per die
	}
}

parameters {
	array[N_dice] simplex[6] delta;
	simplex[6] q_baseline;
	real<lower=0, upper=1> zeta;
}

transformed parameters {
	array[N_dice] simplex[6] q;
	for (i in 1:N_dice) {
		q[i] = (1 / (1 + zeta )) * q_baseline + (zeta / (1 + zeta)) * delta[i];
	}
}

model {
	vector[6] ones = rep_vector(1, 6);
	q_baseline ~ dirichlet(7.5 * ones); // this is the effect across everyone - aka mu - the question is whether the prior for this is reasonable. The next step is to plot it
	zeta ~ normal(0, 5);
	for (i in 1:N_dice) {
		delta[i] ~ dirichlet(ones); // delta is the extra bit that is due to individual differences
		counts_per_die[i] ~ multinomial(q[i]);
	}
}
