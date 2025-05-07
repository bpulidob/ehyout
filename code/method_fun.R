source("code/muod_cpp.R")
source("code/data_generation.R")
source("code/OutGram.R")
source("code/OutGramAdj.R")
path <- "code/multivariate_outliers_meth"
files.sources = list.files(path)
sapply(paste0(path, "/", files.sources), source)


# devtools::install_github("otsegun/fdaoutlier")
library(fdaoutlier)
library(dplyr)
# install.packages("pak", repos = sprintf(
#   "https://r-lib.github.io/p/pak/stable/%s/%s/%s",
#   .Platform$pkgType,
#   R.Version()$os,
#   R.Version()$arch
# ))
# pak::pak("tidyfun/tidyfun")
library(tidyfun)
library(mltools)

calculate_rates <- function(true_outliers, detected_outliers, total_obs) {
  
  # Identify true positives, false positives, true negatives, false negatives
  tp <- sum(detected_outliers %in% true_outliers)
  # print(tp)
  fp <- sum(!detected_outliers %in% true_outliers)
  # print(fp)
  fn <- sum(!(true_outliers %in% detected_outliers))
  #print(fn)
  tn <- total_obs - (tp + fp + fn)
  
  # Calculate true positive rate (TPR) and false positive rate (FPR)
  
  tpr <- tp / (tp + fn)
  tnr <- tn / (tn + fp)
  fpr <- fp / (fp + tn)
  
  # Calculate Accuracy
  
  ac <- (tn+tp)/total_obs
  
  # Calculate Balanced Accuray (BA)
  
  ba <- (tpr+tnr)/2
  
  # Calculate F1 score
  
  # precision <- tp / (tp+fp)
  # f1 <- 2*(precision * tpr) / (precision + tpr) # if precision not null
  f1 <- 2*tp / (2*tp + fn + fp)
  
  # Calculate mcc: Matthews correlation coefficient
  
  s1 <- tp + fp
  s2 <- tp + fn
  s3 <- tn + fp
  s4 <- tn + fn
  mcc_den <- sqrt(s1*s2*s3*s4)
  vmcc <- (tp*tn - fp*fn)/mcc_den
  
  # Return a named list with TPR, FPR, F1 score and MCC
  result <- list(TPR = tpr, FPR = fpr, AC = ac, BA = ba, F1 = f1, MCC = vmcc)
  return(result)
}


add_rank_column <- function(data, var_name) {
  require(dplyr)
  
  # Check if the variable exists in the dataframe
  if (!var_name %in% colnames(data)) {
    stop("Variable not found in the dataframe.")
  }
  
  dnames <- names(data)
  # Add the rank column to the dataframe
  data_var <- data[, var_name]
  vrank <-  rank(-data_var)
  data <- cbind(data, vrank)
  names(data) <- c(dnames, paste0("Rank_", var_name))
  return(data)
}

MMoutlier_detect_comp <- function(sm_d, sm_o){
  
  nr <- nrow(sm_d)
  results_list <- list()
  
  if(typeof(sm_d) == "list"){
    sm_d <- as.matrix(sm_d)
  }
  sm_ind <- indAB(sm_d)
  
  # indAB + multivariate methods
  # Mapping internal names to display names
  multivariate_methods_map <- list(
    mcd = "indAB-MCD",
    adjq_mcd = "indAB-AdjQ_MCD",
    ogk = "indAB-OGK",
    comedian = "indAB-Comedian",
    rmd_sh = "indAB-RMD_sh",
    adj_rmd = "indAB-Adj_RMD",
    mahalanobis = "indAB-Mahalanobis",
    lof = "indAB-LOF"
  )
  internal_multivar_names <- names(multivariate_methods_map)
  
  for(met in internal_multivar_names){
    display_name <- multivariate_methods_map[[met]]
    t0 <- Sys.time()
    multivariate_output <- get_outliers_multivariate(sm_ind, method = met)
    t1 <- Sys.time()
    outlier_time <- as.numeric(t1 - t0, units="secs")
    outlier_rates <- calculate_rates(sm_o, multivariate_output$outliers, nr)
    
    auc_multiv <- Metrics::auc(as.integer(1:nrow(sm_d) %in% sm_o), multivariate_output$values)
    results_list[[display_name]] <- data.frame(
      Method = display_name,
      TPR = outlier_rates$TPR,
      FPR = outlier_rates$FPR,
      F1 = outlier_rates$F1,
      MCC = outlier_rates$MCC,
      AC = outlier_rates$AC,
      BA = outlier_rates$BA,
      AUC = auc_multiv,
      Time = outlier_time,
      stringsAsFactors = FALSE
    )
  }
  
  # Outliergram
  
  t0 <- Sys.time()
  outgram_output <- OutGram(sm_d, plotting = FALSE)
  t1 <- Sys.time()
  outlier_time_og <- as.numeric(t1 - t0, units="secs")
  outlier_rates_og <- calculate_rates(sm_o, outgram_output$outliers, nr)
  auc_og <- Metrics::auc(as.integer(1:nrow(sm_d) %in% sm_o), outgram_output$dist)
  results_list[["OG"]] <- data.frame(Method = "OG", TPR = outlier_rates_og$TPR, 
                                     FPR = outlier_rates_og$FPR, F1 = outlier_rates_og$F1, 
                                     MCC = outlier_rates_og$MCC, AC = outlier_rates_og$AC, 
                                     BA = outlier_rates_og$BA, AUC = auc_og, 
                                     Time = as.numeric(t1-t0, units="secs"), 
                                     stringsAsFactors = FALSE)
  
  # Adjusted Outliergram
  
  t0 <- Sys.time()
  outgramAdj_output <- OutGramAdj(sm_d, plotting = FALSE)
  t1 <- Sys.time()
  outlier_time_aog <- as.numeric(t1 - t0, units="secs")
  outlier_rates_aog <- calculate_rates(sm_o, outgramAdj_output$outliers, nr)
  auc_aog <- Metrics::auc(as.integer(1:nrow(sm_d) %in% sm_o), outgramAdj_output$dist)
  results_list[["AOG"]] <- data.frame(Method = "AOG", TPR = outlier_rates_aog$TPR, 
                                      FPR = outlier_rates_aog$FPR, F1 = outlier_rates_aog$F1, 
                                      MCC = outlier_rates_aog$MCC, AC = outlier_rates_aog$AC, 
                                      BA = outlier_rates_aog$BA, AUC = auc_aog, 
                                      Time = as.numeric(t1-t0, units="secs"), stringsAsFactors = FALSE)
  
  # MS Plot
  
  t0 <- Sys.time()
  generated_outliers <- fdaoutlier::msplot(sm_d, seed = 1221, plot = FALSE)$outliers
  t1 <- Sys.time()
  outlier_time_ms <- as.numeric(t1 - t0, units="secs")
  outlier_rates_ms <- calculate_rates(sm_o, generated_outliers, nr)
  
  results_list[["MSPLT"]] <- data.frame(Method = "MSPLT", TPR = outlier_rates_ms$TPR, 
                                        FPR = outlier_rates_ms$FPR, F1 = outlier_rates_ms$F1, 
                                        MCC = outlier_rates_ms$MCC, AC = outlier_rates_ms$AC, 
                                        BA = outlier_rates_ms$BA, AUC = NA_real_, 
                                        Time = as.numeric(t1-t0, units="secs"), stringsAsFactors = FALSE)
  
  # TVD
  
  t0 <- Sys.time()
  generated_outliers <- fdaoutlier::tvdmss(sm_d, emp_factor_mss = 3)$outliers
  t1 <- Sys.time()
  outlier_time_tvd <- as.numeric(t1 - t0, units="secs")
  outlier_rates_tvd <- calculate_rates(sm_o, generated_outliers, nr)
  results_list[["TVD"]] <- data.frame(Method = "TVD", TPR = outlier_rates_tvd$TPR, 
                                      FPR = outlier_rates_tvd$FPR, F1 = outlier_rates_tvd$F1, 
                                      MCC = outlier_rates_tvd$MCC, AC = outlier_rates_tvd$AC, 
                                      BA = outlier_rates_tvd$BA, AUC = NA_real_, 
                                      Time = as.numeric(t1-t0, units="secs"), stringsAsFactors = FALSE)
  
  # MBD
  
  t0 <- Sys.time()
  bp_output <- fdaoutlier::functional_boxplot(sm_d, depth_method = "mbd")
  t1 <- Sys.time()
  outlier_time_mbd <- as.numeric(t1 - t0, units="secs")
  outlier_rates_mbd <- calculate_rates(sm_o, bp_output$outliers, nr)
  auc_mbd <- Metrics::auc(as.integer(1:nrow(sm_d) %in% sm_o), bp_output$depth_values)
  results_list[["MBD"]] <- data.frame(Method = "MBD", TPR = outlier_rates_mbd$TPR, 
                                      FPR = outlier_rates_mbd$FPR, F1 = outlier_rates_mbd$F1, 
                                      MCC = outlier_rates_mbd$MCC, AC = outlier_rates_mbd$AC, 
                                      BA = outlier_rates_mbd$BA, AUC = auc_mbd, 
                                      Time = as.numeric(t1-t0, units="secs"), stringsAsFactors = FALSE)
  
  # MDS-LOF Methods
  
  t0 <- Sys.time()
  mdslof_output <- outlier_mds_lof(sm_d)
  t1 <- Sys.time()
  outlier_time_mdslof <- as.numeric(t1 - t0, units="secs")
  outlier_rates_mdslof <- calculate_rates(sm_o, mdslof_output$outliers, nr)
  auc_mdslof <- Metrics::auc(as.integer(1:nrow(sm_d) %in% sm_o), mdslof_output$values)
  results_list[["LOF"]] <- data.frame(Method = "LOF", TPR = outlier_rates_mdslof$TPR, 
                                      FPR = outlier_rates_mdslof$FPR, F1 = outlier_rates_mdslof$F1, 
                                      MCC = outlier_rates_mdslof$MCC, AC = outlier_rates_mdslof$AC, 
                                      BA = outlier_rates_mdslof$BA, AUC = auc_mdslof, 
                                      Time = as.numeric(t1-t0, units="secs"), stringsAsFactors = FALSE)
  
  t0 <- Sys.time()
  mdslof5_output <- outlier_mds_lof(sm_d, embed_dim = 5)
  t1 <- Sys.time()
  outlier_time_mdslof5 <- as.numeric(t1 - t0, units="secs")
  outlier_rates_mdslof5 <- calculate_rates(sm_o, mdslof5_output$outliers, nr)
  auc_mdslof5 <- Metrics::auc(as.integer(1:nrow(sm_d) %in% sm_o), mdslof5_output$values)
  results_list[["MDS5LOF"]] <- data.frame(Method = "MDS5LOF", TPR = outlier_rates_mdslof5$TPR, 
                                          FPR = outlier_rates_mdslof5$FPR, F1 = outlier_rates_mdslof5$F1,
                                          MCC = outlier_rates_mdslof5$MCC, AC = outlier_rates_mdslof5$AC, 
                                          BA = outlier_rates_mdslof5$BA, AUC = auc_mdslof5, 
                                          Time = as.numeric(t1-t0, units="secs"), stringsAsFactors = FALSE)
  
  t0 <- Sys.time()
  mdslofl10_output <- outlier_mds_lof(sm_d, metric = "minkowski", p = 10)
  t1 <- Sys.time()
  outlier_time_mdslofl10 <- as.numeric(t1 - t0, units="secs")
  outlier_rates_mdslofl10 <- calculate_rates(sm_o, mdslofl10_output$outliers, nr)
  auc_mdslofl10 <- Metrics::auc(as.integer(1:nrow(sm_d) %in% sm_o), mdslofl10_output$values)
  
  results_list[["LOFl10"]] <- data.frame(Method = "LOFl10", TPR = outlier_rates_mdslofl10$TPR, 
                                         FPR = outlier_rates_mdslofl10$FPR, F1 = outlier_rates_mdslofl10$F1, 
                                         MCC = outlier_rates_mdslofl10$MCC, AC = outlier_rates_mdslofl10$AC, 
                                         BA = outlier_rates_mdslofl10$BA, AUC = auc_mdslofl10, 
                                         Time = as.numeric(t1-t0, units="secs"), stringsAsFactors = FALSE)
  
  t0 <- Sys.time()
  mdslof5l10_output <- outlier_mds_lof(sm_d, metric = "minkowski", p = 10, embed_dim = 5)
  t1 <- Sys.time()
  outlier_time_mdslof5l10 <- as.numeric(t1 - t0, units="secs")
  outlier_rates_mdslof5l10 <- calculate_rates(sm_o, mdslof5l10_output$outliers, nr)
  auc_mdslof5l10 <- Metrics::auc(as.integer(1:nrow(sm_d) %in% sm_o), mdslof5l10_output$values)
  results_list[["MDS5LOFl10"]] <- data.frame(Method = "MDS5LOFl10", TPR = outlier_rates_mdslof5l10$TPR, 
                                             FPR = outlier_rates_mdslof5l10$FPR, F1 = outlier_rates_mdslof5l10$F1, 
                                             MCC = outlier_rates_mdslof5l10$MCC, AC = outlier_rates_mdslof5l10$AC, 
                                             BA = outlier_rates_mdslof5l10$BA, AUC = auc_mdslof5l10, 
                                             Time = as.numeric(t1-t0, units="secs"), stringsAsFactors = FALSE)
  
  # JV
  
  # PWD
  t0 <- Sys.time()
  jv1_output <- outlier_jv(sm_d)
  t1 <- Sys.time()
  outlier_time_pwd <- as.numeric(t1 - t0, units="secs")
  outlier_rates_pwd <- calculate_rates(sm_o, jv1_output$outliers, nr)
  auc_pwd <- Metrics::auc(as.integer(1:nrow(sm_d) %in% sm_o), jv1_output$values)
  results_list[["PWD"]] <- data.frame(Method = "PWD", TPR = outlier_rates_pwd$TPR, 
                                      FPR = outlier_rates_pwd$FPR, F1 = outlier_rates_pwd$F1, 
                                      MCC = outlier_rates_pwd$MCC, AC = outlier_rates_pwd$AC, 
                                      BA = outlier_rates_pwd$BA, AUC = auc_pwd, 
                                      Time = as.numeric(t1-t0, units="secs"), stringsAsFactors = FALSE)
  
  # BP-PWD
  t0 <- Sys.time()
  jv2_output <- outlier_jv(sm_d, magnitude = TRUE)
  t1 <- Sys.time()
  outlier_time_bp_pwd <- as.numeric(t1 - t0, units="secs")
  outlier_rates_bp_pwd <- calculate_rates(sm_o, jv2_output$outliers, nr)
  results_list[["BP-PWD"]] <- data.frame(Method = "BP-PWD", TPR = outlier_rates_bp_pwd$TPR, 
                                         FPR = outlier_rates_bp_pwd$FPR, F1 = outlier_rates_bp_pwd$F1, 
                                         MCC = outlier_rates_bp_pwd$MCC, AC = outlier_rates_bp_pwd$AC, 
                                         BA = outlier_rates_bp_pwd$BA, AUC = NA_real_ , 
                                         Time = as.numeric(t1-t0, units="secs"), stringsAsFactors = FALSE)
  
  # Combine all results
  final_df <- dplyr::bind_rows(results_list)
  return(final_df)
}

MMoutlier_detect_muod <- function(sm_d, sm_o, n_core = 1){
  
  nr <- nrow(sm_d)
  results_list <- list()
  
  # MOUD type methods internal names and their display names
  muod_methods_map <- list(
    rcpp = "MUOD",
    fast = "FST",
    fast_l1 = "FSTL1",
    semifast = "SF"
  )
  
  internal_method_names <- names(muod_methods_map)
  
  for(met in internal_method_names){
    display_name <- muod_methods_map[[met]]
    
    t0 <- Sys.time()
    outliers_type <- get_outliers(t(sm_d), method = met, cut_method = "boxplot",
                                  n_core = n_core)
    t1 <- Sys.time()
    
    otlier_type_time <- as.numeric(t1 - t0, units="secs")
    
    generated_outliers <- unique(c(outliers_type$shape, outliers_type$amplitude, outliers_type$magnitude))
    outlier_type_rates <- calculate_rates(sm_o, generated_outliers, nr)
    
    results_list[[display_name]] <- data.frame(
      Method = display_name,
      TPR = outlier_type_rates$TPR,
      FPR = outlier_type_rates$FPR,
      F1 = outlier_type_rates$F1,
      MCC = outlier_type_rates$MCC,
      AC = outlier_type_rates$AC,
      BA = outlier_type_rates$BA,
      AUC = NA_real_, 
      Time = otlier_type_time,
      stringsAsFactors = FALSE
    )
  }
  
  final_df <- dplyr::bind_rows(results_list)
  return(final_df)
}

# MMoutlier_detect_comp("simulation_model1",  param = c(n = 200, p = 100, outlier_rate = or))

MMoutlier_detect_sim <- function(model, name, param=NULL, nsim = 50, na.rm = TRUE, seed = NULL){
  
  set.seed(seed)
  
  df <- data.frame()
  fun_model <- match.fun(model)
  
  for(s in 1:nsim){
    #print(s)
    param_model <- c(param, seed = s)
    model_out <- rlang::exec(fun_model, !!!param_model)
    sm_d <- model_out$data
    #print(class(sm_d))
    sm_o <- model_out$true_outliers
    df_muod <- MMoutlier_detect_muod(sm_d, sm_o)
    
    #df_other <- MMoutlier_detect_comp(sm_d, sm_o)
    df <- rbind(df, df_muod)
  }
  #for(s in 1:nsim){
  #   print(s)
  
  val <-
    foreach(s = 1:nsim, .combine = rbind) %dopar% {
      
      param_model <- c(param, seed = s)
      model_out <- rlang::exec(fun_model, !!!param_model)
      sm_d <- model_out$data
      sm_o <- model_out$true_outliers
      MMoutlier_detect_comp(sm_d, sm_o)
      # df <- rbind(df, MMoutlier_detect_comp(sm_d, sm_o))
    }
  
  df <- rbind(df, val)
  
  method_order <- c("MUOD", "FST", "FSTL1", "SF",
                    "indAB-MCD", "indAB-AdjQ_MCD",
                    "indAB-OGK","indAB-Comedian", "indAB-RMD_sh", "indAB-Adj_RMD",
                    "indAB-Mahalanobis", "indAB-LOF","OG", "AOG", "MSPLT",
                    "TVD", "MBD", "LOF", "MDS5LOF", "LOFl10", "MDS5LOFl10",
                    "PWD", "BP-PWD")
  
  
  #dataset$variable <- factor(dataset$variable, levels = rev(unique(dataset$variable)), ordered=TRUE)
  df$Method <- factor(df$Method, levels = method_order)
  
  saveRDS(df, file = paste0("results/", name, "_long", ".rds"))
  
  result <- df %>%
    group_by(Method) %>%
    summarise(
      TPR_m = mean(TPR, na.rm = na.rm),
      TPR_med = median(TPR, na.rm = na.rm),
      TPR_sd = sd(TPR, na.rm = na.rm),
      FPR_m = mean(FPR, na.rm = na.rm),
      FPR_med = median(FPR, na.rm = na.rm),
      FPR_sd = sd(FPR, na.rm = na.rm),
      F1_m = mean(F1, na.rm = na.rm),
      F1_med = median(F1, na.rm = na.rm),
      F1_sd = sd(F1, na.rm = na.rm),
      MCC_m = mean(MCC, na.rm = na.rm),
      MCC_med = median(MCC, na.rm = na.rm),
      MCC_sd = sd(MCC, na.rm = na.rm),
      AC_m = mean(AC, na.rm = na.rm),
      AC_med = median(AC, na.rm = na.rm),
      AC_sd = sd(AC, na.rm = na.rm),
      BA_m = mean(BA, na.rm = na.rm),
      BA_med = median(BA, na.rm = na.rm),
      BA_sd = sd(BA, na.rm = na.rm),
      BM_m = mean(2*BA-1, na.rm = na.rm),
      BM_med = median(2*BA-1, na.rm = na.rm),
      BM_sd = sd(2*BA-1, na.rm = na.rm),
      AUC_m = mean(AUC, na.rm = na.rm),
      AUC_med = median(AUC, na.rm = na.rm),
      AUC_sd = sd(AUC, na.rm = na.rm),
      Time_m = mean(Time),
      Time_med = median(Time),
      Time_sd = sd(Time)
    )
  
  return(as.data.frame(result))
}

outlier_lof <- function(data, nrow_data, min_pts_ratio = .75, cutoff = .9){
  
  data_lof <- dbscan::lof(data, minPts = min_pts_ratio*nrow_data)
  data_lof_cutoff <- quantile(data_lof, cutoff)
  
  data_lof_pos <- data.frame(Position = 1:nrow_data,
                             LOF = data_lof) %>%
    arrange(LOF) %>%
    filter(LOF > data_lof_cutoff)
  
  return(list("values"=data_lof, "outliers"=data_lof_pos$Position))
}

outlier_mds_lof <- function(dataset, metric = "euclidean",
                            # min_pts_ratio taken from "geometric perspective on functional outlier detection"
                            min_pts_ratio = .75,
                            embed_dim = 0, cutoff = .9, ...) {
  distances <- as.matrix(dist(dataset, method = metric, ...))
  if(embed_dim > 0){
    #get metric MDS coordinates & use L2-distance in embedding space
    embeddings <- cmdscale(distances, k = embed_dim)
    distances <- as.matrix(dist(embeddings))
  }
  
  distances_lof <- outlier_lof(distances, nrow(distances), min_pts_ratio, cutoff)
  return(list("values"=distances_lof$values,"outliers"=distances_lof$outliers))
}

combinat <- function(n,p){
  if(n<p){
    combinat=0
  }else{
    combinat=exp(lfactorial(n)-(lfactorial(p)+lfactorial(n-p)))
  }
}

outlier_jv <- function(sm_d, magnitude = FALSE){
  
  # Generate an index vector for rows
  idx <- data.frame(pos = 1:nrow(sm_d), out = rep(0, nrow(sm_d)))
  
  data <- t(sm_d)
  
  if(magnitude){
    # magnitude outliers
    fb <- fda::fbplot(data,plot=F)
    m_outliers <- fb$outpoint
    if(length(m_outliers) > 0){
      idx[m_outliers, "out"] <- 1
      # shape outliers
      data <- data[,-m_outliers]
    }
  }
  
  N <- dim(data)[2]
  P <- dim(data)[1]
  
  # fb <- fda::fbplot(data, plot=F)
  # fb_depth <- fb$depth
  # med <- which(fb_depth == max(fb_depth))
  Pw.rank <- as.matrix(t(apply(data,1,rank)))
  n_a <- N-Pw.rank
  n_b <- Pw.rank-1
  Pw.depth <- as.matrix(((n_a*n_b)+N-1)/combinat(N,2))
  
  Pair.Pw.depth=matrix(NaN,nrow =(P-1),ncol = 2*N)
  for (i in 1:N) {
    Pair.Pw.depth[,(2*i-1)]= Pw.depth[1:(P-1),i]
    Pair.Pw.depth[,(2*i)]= Pw.depth[2:P,i]
  }
  
  prod.var <- matrix(0, ncol = 1, nrow = (dim(Pair.Pw.depth)[2]/2))
  for (i in 1:(dim(Pair.Pw.depth)[2]/2)) {
    prod.var[i,]=var(Pair.Pw.depth[,(2*i-1)])*var(Pair.Pw.depth[,(2*i)])
  }
  
  # Bivariate Distribution of Non-Outlying curves
  corr <- matrix(0,ncol = 1,nrow = (dim(Pair.Pw.depth)[2]/2))
  for (i in 1:(dim(Pair.Pw.depth)[2]/2)) {
    if( prod.var[i,]!=0){
      corr[i,] <- cor(Pair.Pw.depth[,(2*i-1)],Pair.Pw.depth[,(2*i)])
    }else{
      corr[i,] <- 0
    }
  }
  
  # Boxplot of sample correlation
  # lw <- boxplot.stats(corr)[[1]][1] # extreme of the lower whisker
  # s_outliers <- as.vector(which(corr <= lw))
  ts.c2 <- matrix(0,ncol = 1,nrow = (dim(Pair.Pw.depth)[2]/2))
  for (i in 1:(dim(Pair.Pw.depth)[2]/2)) {
    if( prod.var[i,]!=0){
      ts.c2[i,] <- abs(corr[i,]-1)/sd(corr)
    }
    else{
      ts.c2[i,] <- abs(-1)/sd(corr)
    }
    
    
  } 
  
  sup_limit <- boxplot.stats(ts.c2)[[1]][5] # extreme of the upper whisker
  s_outliers <- as.vector(which(ts.c2 >= sup_limit))
  
  idx_shape <- idx[idx$out == 0, ]
  idx_shape["index"] <- 1:nrow(idx_shape)
  idx_shape[idx_shape$index %in% s_outliers, "out"] <- 1
  
  if(magnitude){
    outliers <- sort(c(m_outliers, idx_shape[idx_shape$out == 1, "pos"]))
  }else{
    outliers <- idx_shape[idx_shape$out == 1, "pos"]
  }
  
  return(list("values"=ts.c2,"outliers"=outliers))
}