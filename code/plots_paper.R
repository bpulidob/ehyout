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

# Data generation

# covfunexp <- function(gridpoints, alpha, beta, nu){
#   d_matrix <- as.matrix(dist(gridpoints, upper = T, diag = T))
#   return(alpha*exp(-beta*(d_matrix^nu)))
# }

# set.seed(123)
# 
# n <- 50
# p <- 100
# cov_alpha <- 1
# cov_beta <- 1
# cov_nu <- 1
# mu <- 4
# tt <- seq(0, 1, length.out = p)
# covfun <- covfunexp(tt, cov_alpha, cov_beta, cov_nu)
# muu <- mu * tt
# L <- chol(covfun)
# e <- matrix(rnorm(n * p), nrow = p, ncol = n)
# y <- muu + t(L) %*% e
# y <- t(y)
# yl7 <- y[7,]
# y <- as.data.frame(y)
# y_ind <- ind_all(y)
# y_ind$ABEI[7] #52.13895
# 
# # colnames(y) <- tt
# 
# # MAGNITUDE
# y2 <- y
# y2[51, ] <- 3*y[7, ]
# 
# y2_ind <- ind_all(y2)
# y2_ind$ABEI[7] #58.08825
# y2_ind$ABEI[51] #0.449684
# p1 <- plt_curves_out(y2, 0.5, 6.5, "y(t) = 3x(t)")
# p2 <- plt_indices_out(y2)
# p3 <- plt_indices_out(y2, "ABHI")
# 
# y2b <- y
# y2b[7, ] <- 3*y[7, ]
# y2b_ind <- ind_all(y2b)
# y2b_ind$ABEI[7] # 0.4586777
# 
# # SHAPE
# y3 <- y
# y3[51, ] <- y[7,]+(1.5*tt+0.5)^2
# p4 <- plt_curves_out(y3,  0.3, 5, "y(t) = x(t) + h(t)")
# p5 <- plt_indices_out(y3)
# p6 <- plt_indices_out(y3,"ABHI")
# plt_indices_out(y3,"MEI")
# # AMPLITUDE -
# y4 <- y
# y4[51, ] <- y[7, ]-3
# p7 <- plt_curves_out(y4, 0.75, -2, "y(t) = x(t) - 3")
# p8 <- plt_indices_out(y4)
# p9 <- plt_indices_out(y4, "ABHI")
# plt_indices_out(y4, "MHI")
# # AMPLITUDE +
# y5 <- y
# y5[51, ] <- y[7, ]+3
# p10 <- plt_curves_out(y5, 0.4, 6, "y(t) = x(t) + 3")
# p11 <- plt_indices_out(y5)
# p12 <- plt_indices_out(y5, "ABHI")
# plt_indices_out(y5, "MEI")
