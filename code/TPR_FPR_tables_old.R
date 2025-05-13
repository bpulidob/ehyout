# Install necessary packages if you haven't already
# install.packages("gt")
# install.packages("patchwork")
# install.packages("gridExtra")

library(gt)
library(patchwork)
library(gridExtra)
library(cowplot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xtable)

summary_table_TPRFPR <- function(outlier_rate = 0.1){

  #or in c(0.1, 0.05, 0.01)

  table_files <-  c("table_simulation_model1_100.rds", "table_simulation_model2_100.rds",
                    "table_simulation_model3_100.rds", "table_simulation_model4_100.rds",
                    "table_simulation_model5_100.rds", "table_simulation_model6_100.rds",
                    "table_simulation_model7_100.rds", "table_simulation_model8_100.rds",
                    "table_simulation_model9_100.rds", "table_simulation_model10_100.rds",
                    "table_hm100.rds",
                    # "table_am1_100.rds", "table_am2_100.rds",
                    "table_jv1100.rds", "table_jv2100.rds", "table_jv3100.rds",
                    "table_jv4100.rds", "table_jv5100.rds",
                    "table_sn1100.rds", "table_sn2100.rds", 
                    "table_sphere100.rds")
                    # "prueba_table_sphere100.rds")

  combined_data <- data.frame()


  for(file_ind in 1:length(table_files)){

    # Read files
    file_read <- readRDS(paste0("results/",table_files[file_ind]))

    length_file_read <- ncol(file_read)
    n_length <- (length_file_read - 1)/3

    # Choose the side of the table that corresponds to the or we want
    if(outlier_rate == 0.1){
      file_read_filter <- file_read[, 1:(n_length+1)]
    } else if(outlier_rate == 0.05){
      file_read_filter <- file_read[, c(1,(n_length+2):(2*n_length+1))]
    } else if(outlier_rate == 0.01){
      file_read_filter <- file_read[, c(1,(2*n_length+2):(3*n_length+1))]
    } else {
      stop("Invalid outlier rate:", outlier_rate)
    }

    file_read_filter <- file_read_filter %>%
      filter(Method %in% c("MUOD", "indAB-OGK", "indAB-Comedian", "OG", "MSPLT", 
                           "TVD", "MBD", "MDS5LOF", "BP-PWD")) %>%
      select(Method, TPR_m, TPR_sd, FPR_m, FPR_sd, Time_m) %>%
      mutate(
        TPR = sprintf("%.2f (%.2f)", TPR_m, TPR_sd),
        FPR = sprintf("%.2f (%.2f)", FPR_m, FPR_sd),
        DGP = paste0("DGP", file_ind)
        # Time = Time_m
      ) %>%
      select(DGP, Method, TPR, FPR)
    #row.names(file_read_filter) <- file_read_filter$Method

    combined_data <- bind_rows(combined_data, file_read_filter)
  }


  return(combined_data)
}

df_0.1 <- summary_table_TPRFPR()
df_0.01 <- summary_table_TPRFPR(0.01)
df_0.05 <- summary_table_TPRFPR(0.05)
df1 <- cbind(df_0.1, df_0.05[,3:4],  df_0.01[,3:4])

latex_table1 <- xtable(df1)
# print(latex_table1, type = "latex", file = paste0("results/summary_table_TPRFPR_long.tex"))




summary_table_indM <- function(var, outlier_rate = 0.1){
  
  #or in c(0.1, 0.05, 0.01)
  
  table_files <-  c("table_simulation_model1_100.rds", "table_simulation_model2_100.rds",
                    "table_simulation_model3_100.rds", "table_simulation_model4_100.rds",
                    "table_simulation_model5_100.rds", "table_simulation_model6_100.rds",
                    "table_simulation_model7_100.rds", "table_simulation_model8_100.rds",
                    "table_simulation_model9_100.rds", "table_simulation_model10_100.rds",
                    "table_hm100.rds",
                    # "table_am1_100.rds", "table_am2_100.rds",
                    "table_jv1100.rds", "table_jv2100.rds", "table_jv3100.rds",
                    "table_jv4100.rds", "table_jv5100.rds",
                    "table_sn1100.rds", "table_sn2100.rds", "table_sphere100.rds")
  
  combined_data <- data.frame(row.names = c("indM-MCD", "indM-AdjQ_MCD", "indM-OGK", "indM-Comedian", "indM-RMD_sh" ))
  
  
  for(file_ind in 1:length(table_files)){
    
    # Read files
    file_read <- readRDS(paste0("results/",table_files[file_ind]))
    
    length_file_read <- ncol(file_read)
    n_length <- (length_file_read - 1)/3
    
    # Choose the side of the table that corresponds to the or we want
    if(outlier_rate == 0.1){
      file_read_filter <- file_read[, 1:(n_length+1)]
    } else if(outlier_rate == 0.05){
      file_read_filter <- file_read[, c(1,(n_length+2):(2*n_length+1))]
    } else if(outlier_rate == 0.01){
      file_read_filter <- file_read[, c(1,(2*n_length+2):(3*n_length+1))]
    } else {
      stop("Invalid outlier rate:", outlier_rate)
    }
    
    file_read_filter <- file_read_filter %>%
      filter(Method %in% c("indAB-Comedian","indAB-MCD", "indAB-AdjQ_MCD", "indAB-OGK",
                           "indAB-RMD_sh")) %>%
       # select(BA_m)
      select(var) 
      # select(FPR_m)
       
    #row.names(file_read_filter) <- file_read_filter$Method
    
    combined_data[,paste0("DGP",file_ind)] <- file_read_filter
  }
  
  
  return(combined_data)
}


st <- summary_table_indM("AUC_m")
AUC_indM <- as.data.frame(t(st))
#AUC_indM <- rbind(AUC_indM, MEAN=colMeans(AUC_indM))
# xtable(AUC_indM, digits = 3)

names(AUC_indM) <- c("FASTMCD", "FASTMCD-Adj", "OGK", "COM", "RMDsh")

AUC_indM_long <- as.data.frame(AUC_indM) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  mutate(type = "Mean AUC")

indM_AUC <- AUC_indM_long %>% ggplot(aes(x = factor(variable, levels= 
                                                      c("FASTMCD", "FASTMCD-Adj", 
                                                        "OGK", "COM",  "RMDsh")),
                                         y = value)) +
  geom_boxplot() +
  # ylim(-1,1) +
  theme_half_open() +
  labs(x = "",y = "Mean AUC")

ggsave(paste0("results/boxplots/indM_AUC_0.1.pdf"), indM_AUC,
       width = 12, height = 6, units = "in")



st <- summary_table_indM("MCC_m")
BA_indM <- as.data.frame(t(st))
#BA_indM <- rbind(BA_indM, MEAN=colMeans(BA_indM))
# xtable(AUC_indM, digits = 3)

names(BA_indM) <- c("FASTMCD", "FASTMCD-Adj", "OGK", "COM", "RMDsh")

BA_indM_long <- as.data.frame(BA_indM) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  mutate(type = "Mean BA")

indM_long <- rbind(AUC_indM_long, BA_indM_long)

indM_long %>% ggplot(aes(x = variable, y = value, fill = type)) +
  geom_boxplot() +
  # ylim(-1,1) +
  theme_half_open() +
  labs(x = "",y = "", fill = "Metric")

st_BA <- summary_table_indM("BA_m")
BA_indM <- t(st_BA)
BA_indM <- rbind(BA_indM, MEAN=colMeans(BA_indM))
xtable(BA_indM, digits = 3)

st_FPR <- summary_table_indM("FPR_m")
FPR_indM <- t(st_FPR)
FPR_indM <- rbind(FPR_indM, MEAN=colMeans(FPR_indM))
xtable(FPR_indM, digits = 3)

summary_table_y <- function(y_var, outlier_rate = 0.1){
  
  #or in c(0.1, 0.05, 0.01)
  
  table_files <-  c("table_simulation_model1_100.rds", "table_simulation_model2_100.rds",
                    "table_simulation_model3_100.rds", "table_simulation_model4_100.rds",
                    "table_simulation_model5_100.rds", "table_simulation_model6_100.rds",
                    "table_simulation_model7_100.rds", "table_simulation_model8_100.rds",
                    "table_simulation_model9_100.rds", "table_simulation_model10_100.rds",
                    "table_hm100.rds",
                    # "table_am1_100.rds", "table_am2_100.rds",
                    "table_jv1100.rds", "table_jv2100.rds", "table_jv3100.rds",
                    "table_jv4100.rds", "table_jv5100.rds",
                    "table_sn1100.rds", "table_sn2100.rds")
                    #, "table_sphere100.rds")
  
  methods_to_include <- c("MUOD", "indAB-OGK", "indAB-Comedian", "OG", "MSPLT", 
                          "TVD", "MBD", "MDS5LOF", "BP-PWD")
  
  master_summary_df <- data.frame(Method = methods_to_include, stringsAsFactors = FALSE)
  
  
  for(file_ind in 1:length(table_files)){
    
    # Read files
    file_read <- readRDS(paste0("results/",table_files[file_ind]))
    
    length_file_read <- ncol(file_read)
    n_length <- (length_file_read - 1)/3
    
    # Choose the side of the table that corresponds to the or we want
    if(outlier_rate == 0.1){
      selected_cols_data  <- file_read[, 1:(n_length+1)]
    } else if(outlier_rate == 0.05){
      selected_cols_data  <- file_read[, c(1,(n_length+2):(2*n_length+1))]
    } else if(outlier_rate == 0.01){
      selected_cols_data  <- file_read[, c(1,(2*n_length+2):(3*n_length+1))]
    } else {
      stop("Invalid outlier rate:", outlier_rate)
    }
    
    data_from_current_file <- selected_cols_data %>%
      filter(Method %in% methods_to_include) %>%
      select(Method, all_of(y_var))
    
    dgp_col_name <- paste0("DGP", file_ind)
    data_from_current_file_renamed <- data_from_current_file %>%
      rename(!!dgp_col_name := all_of(y_var))
    
    master_summary_df <- left_join(master_summary_df, data_from_current_file_renamed, by = "Method")
  }
  
  final_table <- master_summary_df %>%
    tibble::column_to_rownames(var = "Method")
  
  return(final_table)
}
tf <- summary_table_y("Time_m") 
xtable(t(tf), digits = 4)

st_BA <- summary_table_y("BA_m")
st_BAf <- data.frame(t(st_BA))
names(st_BAf) <- c("FASTMUOD", "indM.OGK", "EHyOut", "OG", "MSPLOT", 
                   "TVD", "MBD", "MDS5LOF", "BP-PWD")

st_BAf <- st_BAf %>%
  select( c("FASTMUOD", "EHyOut", "OG", "MSPLOT", "TVD", "MBD", "MDS5LOF", "BP-PWD"))
st_BAf_m <- rbind(st_BAf, MEAN=colMeans(st_BAf))
xtable(st_BAf_m)


st_BM <- summary_table_y("BM_m")
st_BMf <- data.frame(t(st_BM))
names(st_BMf) <- c("FASTMUOD", "indM.OGK", "EHyOut", "OG", "MSPLOT",
                   "TVD", "MBD", "MDS5LOF", "BP-PWD")

st_BMf <- st_BMf %>%
  select( c("FASTMUOD", "EHyOut", "OG", "MSPLOT", "TVD", "MBD", "MDS5LOF", "BP-PWD"))
st_BMf_m <- rbind(st_BMf, MEAN=colMeans(st_BMf))


st_MCC <- summary_table_y("MCC_m")
st_MCCf <- data.frame(t(st_MCC))
names(st_MCCf) <- c("FASTMUOD", "indM.OGK", "EHyOut", "OG", "MSPLOT",
                   "TVD", "MBD", "MDS5LOF", "BP-PWD")

st_MCCf <- st_MCCf %>%
  select( c("FASTMUOD", "EHyOut", "OG", "MSPLOT", "TVD", "MBD", "MDS5LOF", "BP-PWD"))
st_MCCf_m <- rbind(st_MCCf, MEAN=colMeans(st_MCCf))



df_long <- as.data.frame(st_BAf) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Plot the boxplot
BA_bp <- ggplot(df_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  #theme_half_open() +
  labs(x = "",y = "Mean BA")



df_longBM <- as.data.frame(st_BMf) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  mutate(type = "Mean BM")

df_longMCC <- as.data.frame(st_MCCf) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  mutate(type = "Mean MCC")

df_longBM_MCC <- rbind(df_longBM, df_longMCC)
df_longBM_MCC[is.na(df_longBM_MCC)] <- 0

BM_MCC_bp <- ggplot(df_longBM_MCC, aes(x = factor(variable, 
                                                  levels =c("FASTMUOD", "EHyOut", 
                                                            "OG", "MSPLOT", "TVD", 
                                                            "MBD", "MDS5LOF", 
                                                            "BP-PWD")), 
                                       y = value, fill = type)) +
  geom_boxplot() +
  # ylim(-1,1) +
  # theme_half_open() +
  labs(x = "",y = "", fill="Metric")

ggsave(paste0("results/boxplots/bp_BM_MCC_0.1.pdf"), BM_MCC_bp,
       width = 12, height = 6, units = "in")



################################
# Work with tf table

tf2 <- tf

tf2$row_id <- c("FASTMUOD", "indM.OGK", "EHyOut", "OG", "MSPLOT", 
                   "TVD", "MBD", "MDS5LOF", "BP-PWD")

tf2 <- tf2[c(1,3:nrow(tf2)),]

dim(tf2)
# [1]  8 19


# # Create a row identifier
# tf2$row_id <- as.factor(1:nrow(tf2))
# 
# # Reshape the dataframe to long format
# df_long <- tf2 %>%
#   pivot_longer(cols = -row_id, names_to = "variable", values_to = "value")
# 
# # Create the plot
# ggplot(df_long, aes(x = variable, y = value, color = row_id, group = row_id)) +
#   geom_line() +
#   labs(x = "Variable", y = "Value", color = "Row ID") +
#   theme_minimal()



tf2$row_id <- factor(tf2$row_id , levels = c("FASTMUOD", "EHyOut", "OG", "MSPLOT", "TVD", "MBD", "MDS5LOF", "BP-PWD"))

# Reshape the dataframe to long format
df_long <- tf2 %>%
  pivot_longer(cols = -row_id, names_to = "DGP", values_to = "time")

df_long$DGP <- factor(df_long$DGP, levels = paste0("DGP", 1:19))

# Create the plot with logarithmic scale
time_l <- ggplot(df_long, aes(x = DGP, y = time, color = row_id, group = row_id)) +
  geom_line() +
  scale_y_log10() +
  labs(x = "DGPs", y = "ET (log scale)", color = "Method") +
  theme_half_open() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(paste0("results/boxplots/time_compar_0.1.pdf"), time_l,
       width = 12, height = 6, units = "in")
