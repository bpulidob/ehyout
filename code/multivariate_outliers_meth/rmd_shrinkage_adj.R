# adjusted cutoff
rmdv62adj <- function(dt){
  dt <- as.matrix(dt)
  mX <- ncol(dt)
  nX <- nrow(dt)
  
  qchisq <- qchisq(p = 0.975, mX-1)
  locationMM1 <- integrald(dt)
  locationShMM2 <- shrink_median_boot(dt, medx = locationMM1, option = 1)
  
  dispersionShMM <- cov_shrinkage3(dt, median_x = locationShMM2)
  dispersionShMM21 <- dispersionShMM$sigma2_sh
  
  #
  xbar <- locationShMM2
  # Cov clasica
  S_XX <- dispersionShMM21;
  distmaha <- mahalanobis(dt, xbar, S_XX ) # squared mahalanobis distance
  # get maximum difference p_n
  maxdiff <- maxdiffcalc(dist = distmaha, n = nX, p = mX) ## p_n
  # create the value of the pcrit
  pcrit <- ifelse(mX <= 10,
                  (.4686 - (.0278*mX))/sqrt(nX), # what will happen if p <= 10
                  (.0219 - (.0005*mX))/sqrt(nX)) # what will happen if p > 10
  
  if(maxdiff <= pcrit){
    # set cutoff to infinity here.
    return(NULL) # no outliers here so no index to return
  }else{
    # (1-p_n) quantile of the empirical distribution of the
    # squared robust Mahalanobis distance based on shrinkage.
    # I am not sure how to compute this. Do you know that is will be?
    cutoff <- quantile(distmaha, 1 - maxdiff)
    outlier_indices <- which(distmaha > cutoff)
  }
  return(list("values"=distmaha, "outliers"=outlier_indices))
}

maxdiffcalc <- function(dist, n , p){
  distord <- sort(dist)
  chicdf<- pchisq(distord, p)
  dif <- chicdf - ((0.5:n)/n)
  # I assume that deltan is the 0.975 quartile of the chi quare distribution with p-1 df
  # please confirm that this is true because this variable is not defined in the code
  # that you sent to me.
  deltan <- qchisq(0.975, p-1) 
  i1 <- distord >= deltan #(true and false values)
  i2 <- (dif > 0)  # true and false values
  i3 <- i1 + i2  # implicit conversion of true to 1 and false to 0
  testt <- (i3 == 2)
  if(sum(testt) == 0){
    maxdif <- 0
  } else{
    maxdif <- max(dif[which(testt)])
  }
  return(maxdif)
} 

# ## normal cutoff based on chi square cutoff
# rmdv62 <- function(dt){
#   dt <- as.matrix(dt)
#   mX <- ncol(dt)
#   nX <- nrow(dt)
#   
#   qchisq <- qchisq(p = 0.975, mX-1)
#   locationMM1 <- integrald(dt)
#   locationShMM2 <- shrink_median_boot(dt, medx = locationMM1, option = 1)
#   
#   dispersionShMM <- cov_shrinkage3(dt, median_x = locationShMM2)
#   dispersionShMM21 <- dispersionShMM$sigma2_sh
#   
#   #
#   xbar <- locationShMM2
#   # Cov clasica
#   S_XX <- dispersionShMM21;
#   distmaha <- mahalanobis(dt, xbar, S_XX )
#   outlier_index <- which(distmaha>qchisq)
#   return(outlier_index)
# }
# 
# integrald <- function(data){
#   
#   n <- nrow(data)
#   m <- ncol(data)
#   
#   data <- t(data)
#   
#   range  <- rep(0, n)
#   k  <-  1
#   
#   while( k<=n){
#     dist <- rep(0, n)
#     for (i in 1:n) {
#       dist[i] <- norm(matrix(data[,k] - data[,i], ncol = 1), type = "1")
#     }
#     range[k] <- mean(dist)
#     k <- k+1
#   }
#   range_id <-sort(range, index.return = T)
#   return(data[,range_id$ix[1]])
# }
# 
# shrink_median_boot <- function(X, medx, option){
#   n <- nrow(X)
#   p <- ncol(X)
#   
#   nu_mu <- mean(medx)
#   if (option == 0) {
#     phi <- bootMedian(X,100)   #?????
#   }else{
#     covmatMedian <- cov_median(X, medx)
#     phii <- sum(diag(covmatMedian))
#   }
#   
#   #samp <- 
#   prior <- nu_mu * rep(1,p)
#   gama <- norm(matrix(medx - prior,ncol = 1), type = "2")^2
#   
#   # compute shrinkage constant
#   kapa <- phii/gama
#   shrinkage <- max(0, min(1,kapa))
#   
#   # compute shrinkage estimator
#   location <- shrinkage * prior + (1 - shrinkage) * medx
#   return(location)
# }
# UQ_estimator <- function(y){
#   p <- length(y)
#   ynorm <- sum(abs(y)) # vector L1 norm
#   if(all(y == 0)){
#     U <- rep(0, p)
#     Q <- matrix(0, ncol = p, nrow = p)
#   }else{
#     U <- y/ynorm
#     Q1 <- (1/ynorm) * diag(p)
#     Y <- (y) %*% t(y)
#     Q2 <- ((1/ynorm)^3) * Y
#     Q <- Q1 - Q2
#   }
#   return(list(U = U, Q = Q))
# }
# 
# cov_median <- function(X, medx){
#   if(!is.matrix(X)){
#     X <- as.matrix(X)
#   }
#   n <- nrow(X)
#   p <- ncol(X)
#   
#   ## centre the data with the L1 median
#   Xc <- X - matrix(rep(medx,n), ncol = p, byrow = T)
#   Aaux <- diag(0,p)
#   Baux <- diag(0,p)
#   for (i in 1:n) {
#     y <- Xc[i,]
#     UQ_estimate <- UQ_estimator(y)
#     
#     # Estimator A
#     Aaux <- Aaux + UQ_estimate$Q;
#     
#     # Estimator B
#     Baux <- Baux + (UQ_estimate$U %*% t(UQ_estimate$U))
#     
#   }
#   
#   A <- (1/n) * Aaux
#   B <- (1/n) * Baux
#   iA <- solve(A)
#   covmatrix <- (1/n) * (iA %*% B %*% iA)
#   return(covmatrix)
# }
# 
# 
# cov_shrinkage3 <- function(datax, median_x){
#   m_datacov <- nrow(datax)
#   n_datacov <- ncol(datax)
#   
#   datacentrados_now <- datax - matrix(rep(median_x, m_datacov), nrow = m_datacov, byrow = T)
#   covrob <- matrix(nrow = n_datacov, ncol = n_datacov)
#   for (i in 1:n_datacov) {
#     for (j in 1:n_datacov) {
#       auxx <- datacentrados_now[,i] * datacentrados_now[,j]
#       covrob[i,j] <- median(auxx) 
#     }
#   }
#   
#   auxx1 <- diag(x = 1.198, nrow = n_datacov) + 1
#   sigma1_SH <- auxx1 * covrob
#   
#   t_sh <- m_datacov;
#   sample_sh <- sigma1_SH
#   
#   # compute prior
#   meanvar_sh <- mean(diag(sample_sh)); #(traza(Scom*)/N)
#   prior_sh <- meanvar_sh*diag(nrow = n_datacov);
#   
#   # what we call p
#   datax <- datacentrados_now;
#   y_sh <- datax^2;
#   phiMat <- t(y_sh) %*% y_sh/t_sh - sample_sh^2
#   phi <- sum(phiMat)
#   
#   # what we call r is not needed for this shrinkage target
#   
#   # what we call c
#   gamma <- norm(sample_sh - prior_sh, type = 'F')^2
#   
#   # compute shrinkage constant
#   kappa <- phi/gamma
#   shrinkage <- max(0,min(1,kappa/t_sh));
#   
#   
#   # compute shrinkage estimator
#   sigma2_SH <- shrinkage * prior_sh + (1-shrinkage) * sample_sh
#   
#   return(list(sigma1_sh = sigma1_SH, sigma2_sh = sigma2_SH))
# }