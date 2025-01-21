#################################################
#######################################################
##### task switching session specific outlier situ
#######################################################
# all the variables have wild outliers, so am 
# going to explore with boxplots 
hsts <- apply(betas[,4:8], 2, hist)
# I can see that there are a couple of  wild outliers
# will confirm with boxplots
apply(betas[,4:8], 2, boxplot) # yep, confirms that there are 2 super weird cases, which I 
# shall remove
mu_idx <- which(betas$mu <= -5.0e+14) # first I get the real extreme datapoints out
betas <- betas %>% filter(!sub %in% mu_idx)
# now I can run the regular mean +/- 3 ds