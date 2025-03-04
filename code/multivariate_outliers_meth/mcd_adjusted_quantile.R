library(rrcov)


mcd_adjusted_quantile <- function(dt){
  #set number of rows and number of columns
  n <- nrow(dt)
  p <- ncol(dt)
  # get a robust estimate of location and scatter using the fast mcd algorithm
  # recommended h = [(n+p+1)/2] for breakdown value of  (n-h+1)/n 
  mcd_estimate <- rrcov::CovMcd(dt, alpha = ((n + p + 1)/2)/n)  
  # set cutoff
  cutoff <- adjusted_quantile(dt = dt, distances = mcd_estimate@mah)
  
  # find outliers
  outlier_indices <-  which(mcd_estimate@mah > cutoff)
  #  n_outliers <- sum(mcd_dist >= cutoff)
  
  return(list(out_in = outlier_indices, 
              dist = mcd_estimate@mah, 
              cutt = cutoff))
  
}


## function to identify outliers based on the mcd but using 
## the adjusted cutoff of 


# adjusted threshold is estimated adaptively from the data, defined for robust MCD Mahalanobis distances, 
#Filmozer et al. [2005]
adjusted_quantile <- function(dt, distances){
  
  n <- nrow(dt)
  p <- ncol(dt)
  #value of delta
  deltan <- qchisq(p = 0.975, df = p)
  
  # order the distances
  ord_dist <- sort(distances)
  
  # calculate the empirical distribution of the values of the squared distances
  pchisq_ord_dist <- pchisq(ord_dist,p)
  
  # find the difference between the empirical distribution and theoretical distribution
  dif <- pchisq_ord_dist - ((.5:n)/n)
  
  # first condition: if ordered distance more than the value of deltan
  # Second condition: if the difference values more than zero
  cond <- (ord_dist >= deltan) & (dif > 0)
  alphan <-  ifelse(sum(cond) == 0, 0, max(dif[cond]) )
  return(max(ord_dist[n - ceiling(n*alphan)], deltan))
}