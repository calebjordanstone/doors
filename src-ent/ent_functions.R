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

get_context_swch_idx_per_prsn <- function(dat, subN){
  # ugly function to do the sorting/splitting of data as follows:
  ## now how to split the data up? First, by context.
  ## Second, we don't want to artificially inflate the routine from
  ## each context with the times that the context has switched but 
  ## the participant still thinks they are in the other context.
  ## So, I will take the data:
  ## when there is a context switch
  ##     -- find next trial where cc is a 1. 
  ##     -- trials between switch and 1 go to previous context
  ##     -- trials cc to end go to current context  
  print(paste("sub =", subN))
  tmp <- dat %>% filter(sub == subN)
  
  # first find context switches
  # select current context is the variable that breaks the data how I want
  swch_idx <- which(diff(tmp$context_assign_ent) != 0) + 1 # get points at which 
  # there is a context switch
  # now add the last trial
  swch_idx <- c(swch_idx, nrow(tmp))
  # now for each switch index row, find the next row that has a 1 in the 
  # door_cc column
  # first make the swch_idx for the starts and ends
  nswch = length(swch_idx)
  swch_strs <- swch_idx[1:nswch-1]
  swch_ends <- swch_idx[2:nswch]
  
  # get indexes for where the new context number should be replaced with the old one
  fll_idxs <- rep(0, length(swch_strs))
  for (i in 1:length(swch_strs)){
    fll_idxs[i] <- swch_strs[i] + which(diff(tmp[swch_strs[i]:(swch_ends[i]-1), "door_cc"]) > 0 )[1] - 1
  }       
  rms <- which(is.na(fll_idxs))  
  if (any(rms)){
    swch_strs <- swch_strs[-rms]
    fll_idxs <- fll_idxs[-rms]
  }
  for (i in 1:length(swch_strs)){
    tmp$context_assign_ent[swch_strs[i]:fll_idxs[i]] <- tmp$context_assign_ent[swch_strs[i]-1]
  }
  
  tmp
}