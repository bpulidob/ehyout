library(gridExtra)
library(ggplot2)
source("~/ehyout/code/plots.R")
source("~/ehyout/code/data_generation.R")

jv1_0.1 <- lines_plot(jv_data1, param = c(n = 200, deterministic = TRUE, 
                               outlier_rate = 0.1, p = 100, seed = 1),  
                      seed = 1221, title = "Original data")
jv1_d_0.1 <- lines_plot(jv_data1, param = c(n = 200, deterministic = TRUE, 
                                          outlier_rate = 0.1, p = 100, seed = 1),  
                          seed = 1, order = 1, title = "First derivatives")
jv1_d2_0.1 <- lines_plot(jv_data1, param = c(n = 200, deterministic = TRUE, 
                                            outlier_rate = 0.1, p = 100, seed = 1),  
                        seed = 1, order = 2, title = "Second derivatives")

ggsave("plot_jv1_0.1.pdf", 
       plot = gridExtra::grid.arrange(jv1_0.1, jv1_d_0.1, jv1_d2_0.1, 
                                      nrow = 1, ncol = 3),
       path = "results/plots", width = 7, height = 2.75, device = "pdf")

jv1_MEI <- ind_plot(jv_data1, param = c(n = 200, deterministic = TRUE, 
                                        outlier_rate = 0.1, p = 100, seed = 1), 
                    seed = 1, type="MEI")
jv1_MEId <- ind_plot(jv_data1, param = c(n = 200, deterministic = TRUE, 
                                         outlier_rate = 0.1,  p = 100, seed = 1), 
                     seed = 1, type="MEI_d")
jv1_MEId2 <- ind_plot(jv_data1, param = c(n = 200, deterministic = TRUE, 
                                          outlier_rate = 0.1, p = 100, seed = 1), 
                      seed = 1, type="MEI_d2")

jv1_ABEI <- ind_plot(jv_data1, param = c(n = 200, deterministic = TRUE, 
                                         outlier_rate = 0.1, p = 100, seed = 1), 
                     seed = 1, type="ABEI")
jv1_ABEId <- ind_plot(jv_data1, param = c(n = 200, deterministic = TRUE, 
                                          outlier_rate = 0.1, p = 100, seed = 1), 
                      seed = 1, type="ABEI_d")
jv1_ABEId2 <- ind_plot(jv_data1, param = c(n = 200, deterministic = TRUE, 
                                           outlier_rate = 0.1, p = 100, seed = 1),
                       seed = 1, type="ABEI_d2")

gridExtra::grid.arrange(jv1_MEI, jv1_MEId, jv1_MEId2, 
                        jv1_ABEI, jv1_ABEId, jv1_ABEId2, 
                        nrow = 2, ncol = 3)

ggsave("plot_MEIvsABEI_jv1.pdf", 
       plot = gridExtra::grid.arrange(jv1_MEI, jv1_MEId, jv1_MEId2, 
                                      jv1_ABEI, jv1_ABEId, jv1_ABEId2, 
                                      nrow = 2, ncol = 3),
       path = "results/plots", width = 7, height = 4.5, device = "pdf")

jv1_AB <- ind_plot_combine2(jv_data1, param = c(n = 200, deterministic = TRUE,
                                                outlier_rate = 0.1, p = 100, seed = 1221),
                            seed = 1221, type="_")
ggsave("plot_AB_jv1.pdf", plot = jv1_AB,
       path = "results/plots", width = 7, height = 4, device = "pdf")



# Data plots

for(i in 1:10){
  lplt <- lines_plot(paste0("simulation_model",i), param = c(n = 200, deterministic = TRUE, 
                                    outlier_rate = 0.1, p = 100, seed = 1),
                   seed = 1221)
  ggsave(paste0("results/plots/lineplots/lp_simulation_model", i, "_0.1.pdf"), lplt, width = 10, height = 6, units = "in")
}

lplt <- lines_plot(hm_dat,
                   param = c(n = 200, deterministic = TRUE, 
                             outlier_rate = 0.1, p = 100, seed = 1), seed = 1221)
ggsave(paste0("results/plots/lineplots/hm_0.1.pdf"), lplt, width = 10, height = 6, units = "in")

lplt <- lines_plot(sn_data1,
                   param = c(n = 200, deterministic = TRUE,
                             outlier_rate = 0.1, p = 100, seed = 1),  seed = 1221)
ggsave(paste0("results/plots/lineplots/sn1_0.1.pdf"), lplt, width = 10, height = 6, units = "in")

lplt <- lines_plot(sn_data2,
                   param = c(n = 200, deterministic = TRUE,                                      
                             outlier_rate = 0.1, p = 100, seed = 1), seed = 1221)
ggsave(paste0("results/plots/lineplots/sn2_0.1.pdf"), lplt, width = 10, height = 6, units = "in")

lplt <- lines_plot(jv_data1,
                   param = c(n = 200, deterministic = TRUE,                                      
                             outlier_rate = 0.1, p = 100, seed = 1),  seed = 1221)
ggsave(paste0("results/plots/lineplots/jv1_0.1.pdf"), lplt, width = 10, height = 6, units = "in")

lplt <- lines_plot(jv_data2,
                   param = c(n = 200, deterministic = TRUE,                                      
                             outlier_rate = 0.1, p = 100, seed = 1), seed = 1221)
ggsave(paste0("results/plots/lineplots/jv2_0.1.pdf"), lplt, width = 10, height = 6, units = "in")

lplt <- lines_plot(jv_data3,
                   param = c(n = 200, deterministic = TRUE,                                      
                             outlier_rate = 0.1, p = 100, seed = 1),  seed = 1221)
ggsave(paste0("results/plots/lineplots/jv3_0.1.pdf"), lplt, width = 10, height = 6, units = "in")

lplt <- lines_plot(jv_data4,
                   param = c(n = 200, deterministic = TRUE,                                      
                             outlier_rate = 0.1, p = 100, seed = 1), seed = 1221)
ggsave(paste0("results/plots/lineplots/jv4_0.1.pdf"), lplt, width = 10, height = 6, units = "in")

lplt <- lines_plot(jv_data5,
                   param = c(n = 200, deterministic = TRUE,                                      
                             outlier_rate = 0.1, p = 100, seed = 1),  seed = 1221)
ggsave(paste0("results/plots/lineplots/jv5_0.1.pdf"), lplt, width = 10, height = 6, units = "in")

lplt <- lines_plot(sphere_dat,  
                   param = c(n = 200, outlier_rate = 0.1),  seed = 1224)
ggsave(paste0("results/plots/lineplots/sphere_0.1.pdf"), lplt, width = 10, height = 6, units = "in")
