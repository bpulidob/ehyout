outlier_mahalanobis <- function(dt){
  # number of columns
  p <- ncol(dt)
  # Mahalanobis distance
  mahal_estimate <- mahalanobis(dt, 
                                apply(dt, 2, function(x) mean(x, trim=0.1)),
                                robust::covRob(dt)$cov)
  # set cutoff
  cutoff <- sqrt(qchisq(p = 0.975, df = p))
  # find outliers
  outlier_indices <-  which(sqrt(mahal_estimate) > cutoff)
  return(list("values"=sqrt(mahal_estimate), "outliers"=outlier_indices))
}
