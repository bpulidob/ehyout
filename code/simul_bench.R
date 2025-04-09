library(foreach)
library(doParallel)
library(fda)
library(xtable)

source("~/ehyout/code/indices.R")
source("~/ehyout/code/method_fun.R")
source("~/ehyout/code/multivariate_outliers_meth/outlier_multivariate_load_all.R")

parallel::detectCores() #20
doParallel::registerDoParallel(cores = 15)

no <- 200 # number of observations
nsim = 100 # number of simulations
seed = 1221
det <- TRUE

for(i in 1:10){
  df_or <- data.frame(Method = c("MUOD", "FST", "FSTL1", "SF", "indAB-MCD",
                                 "indAB-AdjQ_MCD", "indAB-Comedian",
                                 "indAB-RMD_sh", "indAB-Adj_RMD",
                                 "indAB-Mahalanobis", "indAB-LOF",
                                 "indAB-OGK","OG", "AOG", "MSPLT",
                                 "TVD", "MBD", "LOF", "MDS5LOF", "LOFl10", "MDS5LOFl10",
                                 "PWD", "BP-PWD"))
  model <- paste0("simulation_model",i)
  for(or in c(0.1, 0.05, 0.01)){
    v_param <- c(n = no, p = 100, deterministic = det, outlier_rate = or)
    df <- MMoutlier_detect_sim(model, name = paste0("table_simulation_model", i, "_", or),
                               param = v_param, nsim = nsim, seed = seed)
    
    # Rank based on mean values
    df <- add_rank_column(df, "MCC_m")
    df <- add_rank_column(df, "AUC_m")
    df <- add_rank_column(df, "BA_m")
    
    # Rank based on meddian values
    df <- add_rank_column(df, "MCC_med")
    df <- add_rank_column(df, "AUC_med")
    df <- add_rank_column(df, "BA_med")
    
    df_or <- cbind(df_or, df[,-1])
    
  }
  
  saveRDS(df_or, file = paste0("results/table_simulation_model", i, "_", nsim, ".rds"))
  latex_table <- xtable(df_or)
  print(latex_table, type = "latex", file = paste0("results/table_simulation_model", i, "_", nsim, ".tex"))
}

# am data
for(v in 1:2){
  
  
  df_or <- data.frame(Method = c("MUOD", "FST", "FSTL1", "SF", "indAB-MCD",
                                 "indAB-AdjQ_MCD", "indAB-Comedian",
                                 "indAB-RMD_sh", "indAB-Adj_RMD",
                                 "indAB-Mahalanobis", "indAB-LOF",
                                 "indAB-OGK","OG", "AOG", "MSPLT",
                                 "TVD", "MBD", "LOF", "MDS5LOF", "LOFl10", "MDS5LOFl10",
                                 "PWD", "BP-PWD"))
  
  for(or in c(0.1, 0.05, 0.01)){
    v_param <- c(n = no, deterministic = det, outlier_rate = or, vers = v)
    df <- MMoutlier_detect_sim(am_dat, name = paste0("table_am", v, "_", or),
                               param = v_param, nsim = nsim, seed = seed)
    
    
    # Rank based on mean values
    df <- add_rank_column(df, "MCC_m")
    df <- add_rank_column(df, "AUC_m")
    df <- add_rank_column(df, "BA_m")
    
    # Rank based on meddian values
    df <- add_rank_column(df, "MCC_med")
    df <- add_rank_column(df, "AUC_med")
    df <- add_rank_column(df, "BA_med")
    
    df_or <- cbind(df_or, df[,-1])
    
    
    
  }
  
  saveRDS(df_or, file = paste0("results/table_am", v, "_", nsim, ".rds"))
  latex_table <- xtable(df_or)
  print(latex_table, type = "latex", file = paste0("results/table_am", v, "_", nsim, ".tex"))
}


df_or <- data.frame(Method = c("MUOD", "FST", "FSTL1", "SF", "indAB-MCD",
                               "indAB-AdjQ_MCD", "indAB-Comedian",
                               "indAB-RMD_sh", "indAB-Adj_RMD",
                               "indAB-Mahalanobis", "indAB-LOF",
                               "indAB-OGK","OG", "AOG", "MSPLT",
                               "TVD", "MBD", "LOF", "MDS5LOF", "LOFl10", "MDS5LOFl10",
                               "PWD", "BP-PWD"))

df_hm_or <- df_or
df_sn_or <- df_or
df_sn2_or <- df_or
df_jv1_or <- df_or
df_jv2_or <- df_or
df_jv3_or <- df_or
df_jv4_or <- df_or
df_jv5_or <- df_or
df_sphere_or <- df_or

for(or in c(0.1, 0.05, 0.01)){
  df_hm <- MMoutlier_detect_sim(hm_dat, name = paste0("table_hm_", or),
                                param = c(n = no, deterministic = det,
                                          outlier_rate = or, p = 100),
                                nsim = nsim, seed = seed)
  
  
  # Rank based on mean values
  df_hm <- add_rank_column(df_hm, "MCC_m")
  df_hm <- add_rank_column(df_hm, "AUC_m")
  df_hm <- add_rank_column(df_hm, "BA_m")
  
  # Rank based on meddian values
  df_hm <- add_rank_column(df_hm, "MCC_med")
  df_hm <- add_rank_column(df_hm, "AUC_med")
  df_hm <- add_rank_column(df_hm, "BA_med")
  
  
  df_hm_or <- cbind(df_hm_or, df_hm[,-1])
  
  
  df_sn <- MMoutlier_detect_sim(sn_data1, name = paste0("table_sn1_", or),
                                param = c(n = no, deterministic = det,
                                          outlier_rate = or),
                                nsim = nsim, seed = seed)
  
  # Rank based on mean values
  df_sn <- add_rank_column(df_sn, "MCC_m")
  df_sn <- add_rank_column(df_sn, "AUC_m")
  df_sn <- add_rank_column(df_sn, "BA_m")
  
  # Rank based on meddian values
  df_sn <- add_rank_column(df_sn, "MCC_med")
  df_sn <- add_rank_column(df_sn, "AUC_med")
  df_sn <- add_rank_column(df_sn, "BA_med")
  
  df_sn_or <- cbind(df_sn_or, df_sn[,-1])
  
  df_sn2 <- MMoutlier_detect_sim(sn_data2, name = paste0("table_sn2_", or),
                                 param = c(n = no, deterministic = det,
                                           outlier_rate = or),
                                 nsim = nsim, seed = seed)
  
  # Rank based on mean values
  df_sn2 <- add_rank_column(df_sn2, "MCC_m")
  df_sn2 <- add_rank_column(df_sn2, "AUC_m")
  df_sn2 <- add_rank_column(df_sn2, "BA_m")
  
  # Rank based on meddian values
  df_sn2 <- add_rank_column(df_sn2, "MCC_med")
  df_sn2 <- add_rank_column(df_sn2, "AUC_med")
  df_sn2 <- add_rank_column(df_sn2, "BA_med")
  
  df_sn2_or <- cbind(df_sn2_or, df_sn2[,-1])
  
  df_jv1 <- MMoutlier_detect_sim(jv_data1, name = paste0("table_jv1_", or),
                                 param = c(n = no, deterministic = det,
                                           outlier_rate = or),
                                 nsim = nsim, seed = seed)
  
  # Rank based on mean values
  df_jv1 <- add_rank_column(df_jv1, "MCC_m")
  df_jv1 <- add_rank_column(df_jv1, "AUC_m")
  df_jv1 <- add_rank_column(df_jv1, "BA_m")
  
  # Rank based on meddian values
  df_jv1 <- add_rank_column(df_jv1, "MCC_med")
  df_jv1 <- add_rank_column(df_jv1, "AUC_med")
  df_jv1 <- add_rank_column(df_jv1, "BA_med")
  
  df_jv1_or <- cbind(df_jv1_or, df_jv1[,-1])
  
  df_jv2 <- MMoutlier_detect_sim(jv_data2, name = paste0("table_jv2_", or),
                                 param = c(n = no, deterministic = det,
                                           outlier_rate = or),
                                 nsim = nsim, seed = seed)
  
  # Rank based on mean values
  df_jv2 <- add_rank_column(df_jv2, "MCC_m")
  df_jv2 <- add_rank_column(df_jv2, "AUC_m")
  df_jv2 <- add_rank_column(df_jv2, "BA_m")
  
  # Rank based on meddian values
  df_jv2 <- add_rank_column(df_jv2, "MCC_med")
  df_jv2 <- add_rank_column(df_jv2, "AUC_med")
  df_jv2 <- add_rank_column(df_jv2, "BA_med")
  
  df_jv2_or <- cbind(df_jv2_or, df_jv2[,-1])
  
  df_jv3 <- MMoutlier_detect_sim(jv_data3, name = paste0("table_jv3_", or),
                                 param = c(n = no, deterministic = det,
                                           outlier_rate = or),
                                 nsim = nsim, seed = seed)
  
  # Rank based on mean values
  df_jv3 <- add_rank_column(df_jv3, "MCC_m")
  df_jv3 <- add_rank_column(df_jv3, "AUC_m")
  df_jv3 <- add_rank_column(df_jv3, "BA_m")
  
  # Rank based on meddian values
  df_jv3 <- add_rank_column(df_jv3, "MCC_med")
  df_jv3 <- add_rank_column(df_jv3, "AUC_med")
  df_jv3 <- add_rank_column(df_jv3, "BA_med")
  
  df_jv3_or <- cbind(df_jv3_or, df_jv3[,-1])
  
  df_jv4 <- MMoutlier_detect_sim(jv_data4, name = paste0("table_jv4_", or),
                                 param = c(n = no, deterministic = det,
                                           outlier_rate = or),
                                 nsim = nsim, seed = seed)
  
  # Rank based on mean values
  df_jv4 <- add_rank_column(df_jv4, "MCC_m")
  df_jv4 <- add_rank_column(df_jv4, "AUC_m")
  df_jv4 <- add_rank_column(df_jv4, "BA_m")
  
  # Rank based on meddian values
  df_jv4 <- add_rank_column(df_jv4, "MCC_med")
  df_jv4 <- add_rank_column(df_jv4, "AUC_med")
  df_jv4 <- add_rank_column(df_jv4, "BA_med")
  
  df_jv4_or <- cbind(df_jv4_or, df_jv4[,-1])
  
  df_jv5 <- MMoutlier_detect_sim(jv_data5, name = paste0("table_jv5_", or),
                                 param = c(n = no, deterministic = det,
                                           outlier_rate = or),
                                 nsim = nsim, seed = seed)
  
  # Rank based on mean values
  df_jv5 <- add_rank_column(df_jv5, "MCC_m")
  df_jv5 <- add_rank_column(df_jv5, "AUC_m")
  df_jv5 <- add_rank_column(df_jv5, "BA_m")
  
  # Rank based on meddian values
  df_jv5 <- add_rank_column(df_jv5, "MCC_med")
  df_jv5 <- add_rank_column(df_jv5, "AUC_med")
  df_jv5 <- add_rank_column(df_jv5, "BA_med")
  
  df_jv5_or <- cbind(df_jv5_or, df_jv5[,-1])
  
  df_sphere <- MMoutlier_detect_sim(sphere_dat, name = paste0("table_sphere_", or),
                                    param = c(n = no, outlier_rate = or, nbasis = 17,
                                              snr = 22, scale_out = 1.3),
                                    nsim = nsim, seed = seed)
  
  # Rank based on mean values
  df_sphere <- add_rank_column(df_sphere, "MCC_m")
  df_sphere <- add_rank_column(df_sphere, "AUC_m")
  df_sphere <- add_rank_column(df_sphere, "BA_m")
  
  # Rank based on median values
  df_sphere <- add_rank_column(df_sphere, "MCC_med")
  df_sphere <- add_rank_column(df_sphere, "AUC_med")
  df_sphere <- add_rank_column(df_sphere, "BA_med")
  
  df_sphere_or <- cbind(df_sphere_or, df_sphere[,-1])
  
}

saveRDS(df_hm_or, file = paste0("results/table_hm", nsim, ".rds"))
latex_table <- xtable(df_hm_or)
print(latex_table, type = "latex", file = paste0("results/table_hm", nsim, ".tex"))

saveRDS(df_sn_or, file = paste0("results/table_sn1", nsim, ".rds"))
latex_table <- xtable(df_sn_or)
print(latex_table, type = "latex", file = paste0("results/table_sn1", nsim, ".tex"))

saveRDS(df_sn2_or, file = paste0("results/table_sn2", nsim, ".rds"))
latex_table <- xtable(df_sn2_or)
print(latex_table, type = "latex", file = paste0("results/table_sn2", nsim, ".tex"))

saveRDS(df_jv1_or, file = paste0("results/table_jv1", nsim, ".rds"))
latex_table <- xtable(df_jv1_or)
print(latex_table, type = "latex", file = paste0("results/table_jv1", nsim, ".tex"))

saveRDS(df_jv2_or, file = paste0("results/table_jv2", nsim, ".rds"))
latex_table <- xtable(df_jv2_or)
print(latex_table, type = "latex", file = paste0("results/table_jv2", nsim, ".tex"))

saveRDS(df_jv3_or, file = paste0("results/table_jv3", nsim, ".rds"))
latex_table <- xtable(df_jv3_or)
print(latex_table, type = "latex", file = paste0("results/table_jv3", nsim, ".tex"))

saveRDS(df_jv4_or, file = paste0("results/table_jv4", nsim, ".rds"))
latex_table <- xtable(df_jv4_or)
print(latex_table, type = "latex", file = paste0("results/table_jv4", nsim, ".tex"))

saveRDS(df_jv5_or, file = paste0("results/table_jv5", nsim, ".rds"))
latex_table <- xtable(df_jv5_or)
print(latex_table, type = "latex", file = paste0("results/table_jv5", nsim, ".tex"))

saveRDS(df_sphere_or, file = paste0("results/table_sphere", nsim, ".rds"))
latex_table <- xtable(df_sphere_or)
print(latex_table, type = "latex", file = paste0("results/table_sphere", nsim, ".tex"))