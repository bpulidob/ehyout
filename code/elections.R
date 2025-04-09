library(foreach)
library(doParallel)
library(fdaoutlier)
library(ehymet)
library(dplyr)
library(tidyr)
library(ggplot2)

source("~/ehyout/code/indices.R")

parallel::detectCores() #20
doParallel::registerDoParallel(cores = 10)

# load OD methodologies
path <- "code/multivariate_outliers_meth"
files.sources = list.files(path)
sapply(paste0(path, "/", files.sources), source)

#load datasets
source("~/ehyout/code/data_generation.R")

## Indices election

indAB_AUC <- function(model, param, nsim=20){
  set.seed(1221)
  val <-
    foreach(s = 1:nsim, .export = ls(), .combine = rbind) %dopar% {
      fun_model <- match.fun(model)
      model_out <- rlang::exec(fun_model, !!!param)
      sm_d <- model_out$data
      sm_o <- model_out$true_outliers
      sm_ind <- ind_all(sm_d)
      out._ <- outlier_mahalanobis(sm_ind %>% dplyr::select(ABEI, ABHI))
      out.d <- outlier_mahalanobis(sm_ind %>% dplyr::select(ABEI_d, ABHI_d))
      out.d2 <- outlier_mahalanobis(sm_ind %>% dplyr::select(ABEI_d2, ABHI_d2))
      out._d <- outlier_mahalanobis(sm_ind %>% dplyr::select(ABEI, ABHI, ABEI_d, ABHI_d))
      out._d2 <- outlier_mahalanobis(sm_ind %>% dplyr::select(ABEI, ABHI, ABEI_d2, ABHI_d2))
      out.dd2 <- outlier_mahalanobis(sm_ind %>% dplyr::select(ABEI_d, ABHI_d, ABEI_d2, ABHI_d2))
      out._dd2 <- outlier_mahalanobis(sm_ind %>% dplyr::select(ABEI, ABHI, ABEI_d, ABHI_d, ABEI_d2, ABHI_d2))
      auc._ <- Metrics::auc(as.integer(1:nrow(sm_d) %in% sm_o), out._$values)
      auc.d <- Metrics::auc(as.integer(1:nrow(sm_d) %in% sm_o), out.d$values)
      auc.d2 <- Metrics::auc(as.integer(1:nrow(sm_d) %in% sm_o), out.d2$values)
      auc._d <- Metrics::auc(as.integer(1:nrow(sm_d) %in% sm_o), out._d$values)
      auc._d2 <- Metrics::auc(as.integer(1:nrow(sm_d) %in% sm_o), out._d2$values)
      auc.dd2 <- Metrics::auc(as.integer(1:nrow(sm_d) %in% sm_o), out.dd2$values)
      auc._dd2 <- Metrics::auc(as.integer(1:nrow(sm_d) %in% sm_o), out._dd2$values)
      data.frame(ABoutlier._ = auc._, ABoutlier.d = auc.d, ABoutlier.d2 = auc.d2,
                 ABoutlier._d = auc._d, ABoutlier._d2 = auc._d2, ABoutlier.dd2 = auc.dd2,
                 ABoutlier._dd2 = auc._dd2)
    }
  return(val)
}

datasets_models <- c(paste0("simulation_model", seq(1, 10)), "hm_dat", "am_dat",
                     "sn_data1", "sn_data2", "jv_data1", "jv_data2", "jv_data3",
                     "jv_data4", "jv_data5", "sphere_dat")

nsim <- 100

df_auc_models <- data.frame()

for(i in 1:length(datasets_models)){
  set.seed(1221)
  result <- indAB_AUC(datasets_models[i], param = c(n = 200, outlier_rate = 0.1), nsim = nsim)
  df_auc_models <- rbind(df_auc_models, result)
}

df_auc_models["model"] <- rep(datasets_models, each=nsim)
saveRDS(df_auc_models, "results/data/election_indices.rds")

# Compute mean of each column grouped by 'model'
election_summary <- election_indices %>%
  group_by(model) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
election_sd <- election_indices %>%
  group_by(model) %>%
  summarise(across(everything(), \(x) sd(x, na.rm = TRUE)))

#Boxplot for indices combinations
election_summary_long <- election_summary %>%
  pivot_longer(cols = -model, names_to = "variable", values_to = "mean_value") %>%
  mutate(variable = factor(variable, levels = names(election_summary)[-1]))

election_ind_bp <- ggplot(election_summary_long, aes(x = variable, y = mean_value)) +
  geom_boxplot() +
  labs(x = "", y = "Mean AUC") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plot_election_ind_bp.pdf", 
       plot = election_ind_bp,
       path = "results/plots", width = 7, height = 4, device = "pdf")



## Method election

metAB_AUC <- function(model, param, nsim=20){
  set.seed(1221)
  val <-
    foreach(s = 1:nsim, .export = ls(), .combine = rbind) %dopar% {
      fun_model <- match.fun(model)
      model_out <- rlang::exec(fun_model, !!!param)
      sm_d <- model_out$data
      sm_o <- model_out$true_outliers
      sm_ind <- ind_all(sm_d) %>% dplyr::select(ABEI, ABHI, ABEI_d, ABHI_d, ABEI_d2, ABHI_d2)
      sm_out <- as.integer(1:nrow(sm_d) %in% sm_o)

      out.mcd <- get_outliers_multivariate(sm_ind, method = "mcd")
      out.adjq_mcd <- get_outliers_multivariate(sm_ind, method = "adjq_mcd")
      out.ogk <- get_outliers_multivariate(sm_ind, method = "ogk")
      out.comedian <- get_outliers_multivariate(sm_ind, method = "comedian")
      out.rmd_sh <- get_outliers_multivariate(sm_ind, method = "rmd_sh")

      auc.mcd <- Metrics::auc(sm_out, out.mcd$values)
      auc.adjq_mcd <- Metrics::auc(sm_out, out.adjq_mcd$values)
      auc.ogk <- Metrics::auc(sm_out, out.ogk$values)
      auc.comedian <- Metrics::auc(sm_out, out.comedian$values)
      auc.rmd_sh <- Metrics::auc(sm_out, out.rmd_sh$values)

      data.frame(FASTMCD = auc.mcd, FASTMCDAdj = auc.adjq_mcd, OGK = auc.ogk,
                 COM = auc.comedian, RMDsh = auc.rmd_sh)
    }
  return(val)
}

nsim <- 200

Sys.time()
df_auc_models_met <- data.frame()

for(i in 1:length(datasets_models)){
  set.seed(777)
  result <- metAB_AUC(datasets_models[i], param = c(n = 200, outlier_rate = 0.1), nsim = nsim)
  df_auc_models_met <- rbind(df_auc_models_met, result)
}

df_auc_models_met["model"] <- rep(datasets_models, each=nsim)
saveRDS(df_auc_models_met, "results/data/election_OM.rds")
Sys.time()
#Lo he lanzado "2025-03-28 13:12:58 CET"
# Terminado> "2025-03-28 13:20:44 CET"

election_OM <- readRDS("~/ehyout/results/data/election_OM.rds")
# Compute mean of each column grouped by 'model'
election_summary <- election_OM %>%
  group_by(model) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))

#Boxplot for indices combinations
election_summary_long <- election_summary %>%
  pivot_longer(cols = -model, names_to = "variable", values_to = "mean_value") %>%
  mutate(variable = factor(variable, levels = names(election_summary)[-1]))

election_OM_bp <- ggplot(election_summary_long, aes(x = variable, y = mean_value)) +
  geom_boxplot() +
  labs(x = "", y = "Mean AUC") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plot_election_ind_bp.pdf", 
       plot = election_ind_bp,
       path = "results/plots", width = 7, height = 4, device = "pdf")
