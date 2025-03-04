outliers_det <- function(deterministic, n, outlier_rate){
  if(outlier_rate <= 0 | outlier_rate >= 1){
    stop("Oulier rate should be a positive value between 0 and 1 (not included)")
  }
  if(deterministic) {
    true_outliers <- sort(sample(1:n, round(n*outlier_rate)))
    n_outliers <- length(true_outliers)
  } else{
    # find outliers using binomial distribution
    true_outliers <- which(rbinom(n, 1, outlier_rate) == 1)
    while (length(true_outliers) == 0) {
      true_outliers <- which(rbinom(n, 1, outlier_rate) == 1)
    }
  }
  return(true_outliers)
}

hm_dat <- function(n = 200, outlier_rate = 0.1, p=500, deterministic = TRUE, seed = NULL) {
  
  set.seed(seed)
  true_outliers <- outliers_det(deterministic, n, outlier_rate)
  non_outliers <- setdiff(1:n, true_outliers)
  n_out <- length(true_outliers)
  
  a <- rnorm(n-n_out, 5, 2)
  b <- rnorm(n_out, 5, sqrt(3))
  t <- seq(0,1,length.out=p)
  
  inclass <- t(simplify2array(lapply(a, function(x) {x + 0.05 * t *p + sin(pi * (t)^2)})))
  outclass <- t(simplify2array(lapply(b, function(x) {x + 0.05 * t *p + cos(20 * pi * t)})))
  data <- matrix(nrow = n, ncol = p)
  data[non_outliers,] <- inclass
  data[true_outliers,] <- outclass
  data <- t(apply(data,1,function(x) {x + runif(p,-0.01,0.01)}))
  return(list("data" = data, "true_outliers" = true_outliers))
}

am_dat <- function(n = 100, outlier_rate = 0.1, p = 150, vers = 1, deterministic = TRUE, seed = NULL) {
  
  set.seed(seed)
  true_outliers <- outliers_det(deterministic, n, outlier_rate)
  non_outliers <- setdiff(1:n, true_outliers)
  
  t <- seq(0, 1, length.out = p)
  K <- 100
  mX <- t * (1 - t)
  
  rho <- ifelse(1:K < 4, 1 / (1:K + 1), 1 / (1:K + 1)^2)
  sqrt_rho <- sqrt(rho)
  
  theta <- sapply(1:K, function(k) {
    if (k %% 2 == 0) sqrt(2) * sin(k * pi * t)
    else if (k %% 2 != 0 && k != 1) sqrt(2) * cos((k - 1) * pi * t)
    else rep(1, p)
  }) %>% t()
  
  m <- sqrt(rho)*theta
  
  if (vers == 1){
    mY <- mX + colSums(m[1:4,])
  } else if (vers == 2){
    mY <- mX + colSums(m[5:K,])
  }
  
  data <- matrix(nrow = n, ncol = p)
  n1 <- length(true_outliers)
  
  zX <- matrix(rnorm((n-n1)*K), n - n1, K) * sqrt_rho 
  zY <- matrix(rnorm(n1*K), n1, K) * sqrt_rho
  
  uX <- t(t(zX %*% theta) + mX)
  uY <- t(t(zY %*% theta) + mY)
  
  uX <- mX+uX
  uY <- mY+uY
  
  data[non_outliers,] <- uX
  data[true_outliers,] <- uY
  return(list("data" = data, "true_outliers" = true_outliers))
}

sn_data1 <- function(n = 200, outlier_rate = 0.1, p = 101, deterministic = TRUE, seed = NULL){
  
  set.seed(seed)
  true_outliers <- outliers_det(deterministic, n, outlier_rate)
  non_outliers <- setdiff(1:n, true_outliers)
  n_out <- length(true_outliers)
  
  t <- seq(0, 1, length = p)
  data <- matrix(nrow = n, ncol = p)
  d_matrix <- as.matrix(dist(t, upper = T, diag = T)) 
  covfun <- 0.2*exp(-d_matrix/0.3)
  L <- chol(covfun)
  e <- matrix(rnorm(n*p), nrow = p, ncol = n)
  
  data[non_outliers,] <- rnorm(n-n_out,0,2) + rexp(n-n_out)%*%t(atan(t)) + t(t(L)%*%e[,non_outliers])
  data[true_outliers, ] <- 1 - 2*atan(t) + t(t(L)%*%e[,true_outliers])
  
  return(list("data"=data, "true_outliers" = true_outliers))
}

sn_data2 <- function(n = 200, outlier_rate = 0.1, p = 101, deterministic = TRUE, seed = NULL){
  
  set.seed(seed)
  true_outliers <- outliers_det(deterministic, n, outlier_rate)
  non_outliers <- setdiff(1:n, true_outliers)
  n_out <- length(true_outliers)
  
  t <- seq(0, 1, length = p)
  data <- matrix(nrow = n, ncol = p)
  d_matrix <- as.matrix(dist(t, upper = T, diag = T)) 
  covfun <- 0.2*exp(-d_matrix/0.3)
  L <- chol(covfun)
  e <- matrix(rnorm(n*p), nrow = p, ncol = n)
  
  data[non_outliers,] <- rnorm(n-n_out,0,2) + rexp(n-n_out)%*%t(atan(t)) + t(t(L)%*%e[,non_outliers])
  data[true_outliers, ] <- ifelse(t <= 0.5, 0.5+log(2)*atan(t), -0.5+log(2)*atan(t)) + t(t(L)%*%e[,true_outliers])
  
  return(list("data"=data, "true_outliers" = true_outliers))
}

jv_data1 <- function(n = 200, outlier_rate = 0.1, p= 101, deterministic = TRUE, seed = NULL){
  
  set.seed(seed)
  true_outliers <- outliers_det(deterministic, n, outlier_rate)
  non_outliers <- setdiff(1:n, true_outliers)
  
  t <- seq(0, 1, length = p)
  data <- matrix(nrow = n, ncol = p)
  d_matrix <- as.matrix(dist(t, upper = T, diag = T)) 
  covfun <- exp(-d_matrix)
  L <- chol(covfun)
  covfun2 <- 6*exp(-d_matrix^0.1)
  L2 <- chol(covfun2)
  e <- matrix(rnorm(n*p), nrow = p, ncol = n)
  
  data[non_outliers,] <- t(t(L)%*%e[,non_outliers])
  data[true_outliers, ] <- t(t(L2)%*%e[,true_outliers])
  
  return(list("data"=data, "true_outliers" = true_outliers))
}

jv_data2 <- function(n = 200, outlier_rate = 0.1, p = 101, deterministic = TRUE, seed = NULL){
  
  set.seed(seed)
  true_outliers <- outliers_det(deterministic, n, outlier_rate)
  non_outliers <- setdiff(1:n, true_outliers)
  
  t <- seq(0, 1, length = p)
  data <- matrix(nrow = n, ncol = p)
  d_matrix <- as.matrix(dist(t, upper = T, diag = T)) 
  covfun <- exp(-d_matrix)
  L <- chol(covfun)
  e <- matrix(rnorm(n*p), nrow = p, ncol = n)
  
  data[non_outliers,] <- t(2*sin(15*pi*t) + t(L)%*%e[,non_outliers])
  data[true_outliers, ] <- t(2*sin(15*pi*t+4) + t(L)%*%e[,true_outliers])
  
  return(list("data"=data, "true_outliers" = true_outliers))
}

jv_data3 <- function(n = 200, outlier_rate = 0.1, p = 101, k=0.1, deterministic = TRUE, seed = NULL){
  
  set.seed(seed)
  true_outliers <- outliers_det(deterministic, n, outlier_rate)
  non_outliers <- setdiff(1:n, true_outliers)
  
  t <- seq(0, 1, length = p)
  data <- matrix(nrow = n, ncol = p)
  d_matrix <- as.matrix(dist(t, upper = T, diag = T)) 
  covfun <- exp(-d_matrix)
  L <- chol(covfun)
  covfun2 <- 0.1*exp(-(d_matrix^0.1)/4)
  L2 <- chol(covfun2)
  e <- matrix(rnorm(n*p), nrow = p, ncol = n)
  
  data[non_outliers,] = t(0.1 + atan(t) + t(L)%*%e[,non_outliers])
  data[true_outliers,] = t(atan(t) + t(L2)%*%e[,true_outliers])
  
  return(list("data"=data, "true_outliers" = true_outliers))
}

jv_data4 <- function(n = 200, outlier_rate = 0.1, p = 101, k=0.1, deterministic = TRUE, seed = NULL){
  
  set.seed(seed)
  true_outliers <- outliers_det(deterministic, n, outlier_rate)
  non_outliers <- setdiff(1:n, true_outliers)
  
  t <- seq(0, 1, length = p)
  data <- matrix(nrow = n, ncol = p)
  d_matrix <- as.matrix(dist(t, upper = T, diag = T)) 
  covfun <- exp(-d_matrix)
  L <- chol(covfun)
  covfun2 <- 0.1*exp(-(d_matrix^0.1)/4)
  L2 <- chol(covfun2)
  e <- matrix(rnorm(n*p), nrow = p, ncol = n)
  
  data[non_outliers,] = t(30*t*(1-t)^(3/2) + t(L)%*%e[,non_outliers])
  data[true_outliers,] = t(30*t*(1-t)^(3/2) + t(L2)%*%e[,true_outliers])
  
  return(list("data"=data, "true_outliers" = true_outliers))
}

jv_data5 <- function(n = 200, outlier_rate = 0.1, p = 101, k=0.1, deterministic = TRUE, seed = NULL){
  
  set.seed(seed)
  true_outliers <- outliers_det(deterministic, n, outlier_rate)
  non_outliers <- setdiff(1:n, true_outliers)
  
  t <- seq(0, 1, length = p)
  data <- matrix(nrow = n, ncol = p)
  d_matrix <- as.matrix(dist(t, upper = T, diag = T)) 
  covfun <- exp(-d_matrix)
  L <- chol(covfun)
  covfun2 <- 0.1*exp(-(d_matrix^0.1)/4)
  L2 <- chol(covfun2)
  e <- matrix(rnorm(n*p), nrow = p, ncol = n)
  u <- runif(p, 0.25, 0.5)
  
  data[non_outliers,] = t(t(L)%*%e[,non_outliers])
  data[true_outliers,] = t(0.1*sin(40*(t + u)*pi) + t(L2)%*%e[,true_outliers])
  
  return(list("data"=data, "true_outliers" = true_outliers))
}

sphere_dat <- function(n = 200, outlier_rate = 0.1, nbasis = 8, gridlength = 100,
                       snr = 20, scale_out = 1.1, rot =TRUE, seed = NULL){
  
  set.seed(seed)
  
  grid <- seq(0, 1, length.out = gridlength)
  basis <- splines::bs(grid, df = nbasis, intercept = TRUE)
  
  n_in <- floor(n * (1 - outlier_rate))
  n_out <- n - n_in
  coef_inlier <- {
    # generate coef vectors on circle in the plane defined by the first 2 dims,
    # then rotate randomly into basis-dim space
    # (idea: any orthonormal matrix defines a rotation around the origin)
    random_rotation <- matrix(runif(nbasis^2), nbasis, nbasis) |> qr() |> qr.Q()
    dim1 <- rnorm(n_in)
    dim2 <- rnorm(n_in)
    tmp <- cbind(dim1, dim2,
                 matrix(0, nrow = n_in, ncol = nbasis - 2))
    tmp <- tmp/sqrt(rowSums(tmp^2))
    tmp %*% random_rotation
  }
  inlier <- coef_inlier %*% t(basis)
  # pairs(coef_inlier)
  
  coef_outlier <- {
    # generate coef vectors on circle in the plane defined by the first 2 dims,
    # then rotate randomly into basis-dim space
    
    tmp <- cbind(dim1[1:n_out], dim2[1:n_out],
                 matrix(0, nrow = n_out, ncol = nbasis - 2))
    
    if(rot){
      out_rotation <- {
        new_rotation <- matrix(runif(nbasis^2), nbasis, nbasis) |> qr() |> qr.Q()
        (random_rotation %*% (scale_out*new_rotation)) |> qr() |> qr.Q()
      }
      tmp <- tmp/sqrt(rowSums(tmp^2))
    } else{
      tmp <- tmp/sqrt(rowSums(tmp^2)) * scale_out
      out_rotation <- random_rotation
    }
    
    tmp %*% out_rotation
  }
  outlier <- coef_outlier %*% t(basis)
  data <- rbind(inlier, outlier)
  if (snr != Inf) {
    noise <- rnorm(n * gridlength, sd = sqrt(sd(data)^2/snr)) |>
      matrix(nrow = n, ncol = gridlength)
    data <- data + noise
  }
  list("data" = data, true_outliers = (n_in+1):n)
}

simulation_model10 <- function(n=100, p = 50, outlier_rate = 0.05, deterministic =TRUE, seed= NULL, plotdt = F){
  
  set.seed(seed)
  
  f.out151 <- function(u = runif(1,0,1)){            #cont model 2Model
    return(ifelse(u < 0.5, 1,-1))
  }
  f.out152 <- function(u = runif(1,0,1),
                       T1 = runif(1,0.1,0.9),
                       x = t){
    return(list(u = ifelse(u < 0.5, 1,-1),
                i = (x>T1)*(x<(T1+.05))) )
  }
  f.out153 <- function(u = runif(1,0,1),
                       T1 = runif(1,0,1),
                       x = t){
    return(list(u = ifelse(u < 0.5, 1,-1),
                i = (x>=T1)) )
  }
  
  f.out156 <- function(t, th = runif(1,0.25,0.75)){
    4*t + 2*sin(4*(t+th)*pi)
  }
  
  t <- (1:p)/p
  d <- dist(t, upper = T, diag = T)
  d_matrix <- as.matrix(d)
  #  t.cov <- cov.fun2(d.matrix,0.3,1/0.3)
  t.cov <- exp(-d_matrix)
  mu <- 4*t
  L <- chol(t.cov)
  ### Generate Data
  e <- matrix(rnorm(n*p), nrow = p, ncol = n)
  y <- mu+t(L)%*%e
  
  ## Generate
  
  ### Generate Outliers (deterministic=TRUE)
  true_outliers <- sort(sample(1:n, n*outlier_rate))
  #  t.cov <- cov.fun(d.matrix,0.5,1,1)  #covariance function in time
  t.cov <- exp(-d_matrix)
  L <- chol(t.cov) # Cholesky Decomposition
  outt <- c()
  for (i in true_outliers){
    tt <- sample(c(1, 6, 2,  4), 1)
    outt <- c(outt, tt)
    ei <- rnorm(p)
    if(tt == 1){
      y[,i]= (4*t) + (f.out151()*8) +t(L)%*%ei # model 1
    }else if(tt == 2){
      temp <- f.out152(x = t)
      y[,i]= (4*t) + (8 * temp$u * temp$i) + t(L)%*%ei # model 2
    } else if (tt == 6){ 
      
      y[,i] <- f.out156(t)+t(L)%*%ei # model 7
      
    } else{
      t.covsp <- 5*exp(-2*(d_matrix)^(.5))
      Lsp <- chol(t.covsp) # Cholesky Decomposition
      y[,i] <- (4*t)+t(Lsp)%*%ei # model 5
    }
    #Outliers for Model 1
    #y[,i]=f.out1(t,runif(1,0.25,0.75))+t(L)%*%e   #Outliers for Model 2
    #y[,i]=f.out2(t,runif(1,0.25,0.75))+t(L)%*%e   #Outliers for Model 3
  }
  if(plotdt){
    dt <- as.data.frame.matrix(t(y))
    dt <- as.data.frame.matrix(t(as.matrix(dt)))
    dt$x <- seq(0,1, length.out = p)
    out <-  rep(0,n)
    out[true_outliers] <- outt
    
    dt_t <- dt %>% gather(key = "key", value = "value", -x)
    dt_t$outlier <- factor(rep(out, each = p))
    dt_t$key <- factor(dt_t$key)
    
    #col=c(rep("#D55E00",m1/2),rep("#009E73",m1/2),rep(8,n-m1))
    
    print(p <- ggplot(data = filter(dt_t, outlier != "0"), aes(x =x,
                                                               y = value,
                                                               group = key,
                                                               color = outlier)) +
            geom_line(data = filter(dt_t, outlier == "0"),
                      aes(x =x,
                          y = value,
                          group = key ), color = 8, size = 0.3 )+
            geom_line(size = .3) +
            
            xlab("t") +
            ylab("X(t)")+
            ggtitle("Model 15") +
            theme_bw()+
            theme(plot.title = element_text(hjust = 0.5, size = 8),
                  axis.text=element_text(size = 7),
                  axis.title=element_text(size = 8) ))
  } else(
    p <- NULL
  )
  return(list(data = t(y), true_outliers = true_outliers, plot = p))
  
}
