require(rrcov) # for fast mcd algortithm

outlier_mcd <- function(dt){
  #set number of rows and number of columns
  p <- ncol(dt)
  # get a robust estimate of location and scatter using the fast mcd algorithm
  # recommended h = [(n+p+1)/2] for breakdown value of  (n-h+1)/n 
  mcd_estimate <- rrcov::CovMcd(dt, alpha = 0.5)  
  # set cutoff
  cutoff <- sqrt(qchisq(p = 0.975, df = p))
  
  # find outliers
  outlier_indices <-  which(sqrt(mcd_estimate@mah) > cutoff)
  return(list("values"=sqrt(mcd_estimate@mah), "outliers"=outlier_indices))
  
}