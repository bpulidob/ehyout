### require(robustbase)

outlier_comsajesh <- function(dt){
  #set number of rows and number of columns
  p <- ncol(dt)
  # get a robust estimate of comedian location and scatter from robustbase
  comsajesh_estimate <- robustbase::covComed(dt, n.iter = 2, reweight = T)  
  # set cutoff
  cutoff <- 1.4826 * (qchisq(p = 0.975, df = p) * median(comsajesh_estimate$mah))/qchisq(p = 0.5, df = p)
  # find outliers
  outlier_indices <-  which(comsajesh_estimate$mah > cutoff)
  return(list("values"=comsajesh_estimate$mah, "outliers"=outlier_indices))
}
