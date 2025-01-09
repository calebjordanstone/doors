# K. Garner, 2025, a set of functions to help with the analyses

H <- function(x){
  # to be applied over the rows of the matrix of transitions
  -sum(x * log(x), na.rm = TRUE)
}

data_2_counts_matrix <- function(data, n_doors){
  # turn data into a counts matrix of transitions made
  mat <- matrix(rep(0, times=n_doors*n_doors), nrow=n_doors, ncol=n_doors)
  idxs <- matrix(c(data[1:length(data)-1], data[2:length(data)]), nrow=2, byrow=TRUE)
  for(i in 1:ncol(idxs)){
    mat[idxs[1,i],idxs[2,i]] <- mat[idxs[1,i],idxs[2,i]] + 1
  }
  mat
}

p_st1_gs <- function(counts_matrix, n_doors){
  # convert the counts matrix into the row probabilities
  denom <- matrix(rep(rowSums(counts_matrix), n_doors), nrow=n_doors, byrow=FALSE)
  out <- counts_matrix / denom
  out[is.na(out)] = 0
  out
}

sample_non_consec <- function(observed_data){
  
  n = length(observed_data) # this is the length of the sequence we will generate
  out = rep(0, times=n)
  out[1] <- sample(observed_data, 1)
  values <- unique(observed_data)
  for (i in 2:length(out)){
    poss_values <- setdiff(values, out[i - 1])
    out[i] <- sample(poss_values, 1)
  }
  out
}

generate_null_for_one_person <- function(observed_data, n_samples = 1000, n_doors = 16){
  
  null_seqs <- replicate(n_samples, sample_non_consec(observed_data), simplify=FALSE)
  null_counts <- lapply(null_seqs, data_2_counts_matrix, n_doors = n_doors)
  null_ps <- lapply(null_counts, p_st1_gs, n_doors = n_doors)
  null_Rs <- apply(do.call(rbind, lapply(null_ps, function(x) apply(x, 1, H))), 1, sum)
  null_Rs
}