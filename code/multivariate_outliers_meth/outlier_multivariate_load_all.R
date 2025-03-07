get_outliers_multivariate <- function(data_indM, method = c("mcd", "adjq_mcd",
                                                            "ogk", "comedian",
                                                            "rmd_sh", "adj_rmd",
                                                            "mahalanobis", "lof")){
  #method <- match.arg(method)
  
  if(method == "mcd"){
    # MCD
    mcd_indM <- outlier_mcd(data_indM)
    values <- mcd_indM$values
    outliers <- mcd_indM$outliers
  } else if(method == "adjq_mcd"){
    # Adjusted Quantllie MCD
    mcd_adjusted_quantile_indM <- mcd_adjusted_quantile(data_indM)
    values <- mcd_adjusted_quantile_indM$dist
    outliers <-mcd_adjusted_quantile_indM$out_in
  } else if(method == "ogk"){
    # OGK (Ortogonalized Gnanadesikan-Kettenring)
    ogk_indM <- outlier_ogk(data_indM)
    values <- ogk_indM$values
    outliers <- ogk_indM$outliers
  } else if(method == "comedian"){
    # Comedian (Co-Median Location and Scatter "Covariance" Estimator)
    comsajesh_indM <- outlier_comsajesh(data_indM)
    values <- comsajesh_indM$values
    outliers <- comsajesh_indM$outliers
  } else if(method == "rmd_sh"){
    # RMD Shrinkage
    rmdv62_indM <- rmdv62(data_indM)
    values <- rmdv62_indM$values
    outliers <- rmdv62_indM$outliers
  } else if(method == "adj_rmd"){
    # RMD Shrinkage Adjusted
    rmdv62adj_indM <- rmdv62adj(data_indM)
    values <- rmdv62adj_indM$values
    outliers <- rmdv62adj_indM$outliers
  }else if(method == "mahalanobis"){
    # Mahalanobis
    mahal_indM <- outlier_mahalanobis(data_indM)
    values <- mahal_indM$values
    outliers <- mahal_indM$outliers
  } else if(method == "lof"){
    lof_indM <- outlier_lof(data_indM, nrow(data_indM))
    values <- lof_indM$values
    outliers <- lof_indM$outliers
  }else{
    stop("non a valid method")
  }
  
  return(list("values"=values, "outliers"=outliers))
}