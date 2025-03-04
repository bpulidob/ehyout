require(rrcov) # for ogk estimate algortithm

outlier_ogk <- function(dt){
  #set number of rows and number of columns
  n <- nrow(dt)
  p <- ncol(dt)
  # get a robust estimate of location and scatter using the fast mcd algorithm
  # recommended h = [(n+p+1)/2] for breakdown value of  (n-h+1)/n 
  ogk_estimate <- rrcov::CovOgk(dt)  
  
  # compute the robust distance
  ogk_dist <- sqrt(mahalanobis(dt, ogk_estimate@center, ogk_estimate@cov))
  
  # set cutoff
  cutoff <- sqrt(qchisq(p = 0.975, df = p))
  
  # find outliers
  outlier_indices <-  which(ogk_dist > cutoff)
  #  n_outliers <- sum(mcd_dist >= cutoff)
  
  return(list("values"=ogk_dist, "outliers"=outlier_indices))
}