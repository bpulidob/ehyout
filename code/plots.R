source("~/ehyout/code/indices.R")
library(fdaoutlier)
library(dplyr)
library(tidyr)
#library(tidyfun)

lines_plot <- function(model, param = NULL, seed = NULL, order = 0, title = NULL){
  set.seed(seed)
  fun_model <- match.fun(model)
  model_out <- rlang::exec(fun_model, !!!param)
  
  sm_tf <- tfd(data = model_out$data, evaluator = tf_approx_spline)
  
  if (order == 0) {
    sm_tf <- as.matrix(sm_tf)
    dat_df <- as.data.frame(sm_tf) %>%
      mutate(id = row_number()) %>%  # Assign a unique ID for each curve
      pivot_longer(cols = -id, names_to = "x", values_to = "y") %>%
      mutate(x = rep(seq(0, 1, length.out = ncol(sm_tf)), times = nrow((sm_tf)))) 
  } else if (order == 1 | order == 2) {
    
    
    sm_tf_deriv <- as.matrix(tf_derive(sm_tf, order = order))  # Second derivative for order = 2
    
    dat_df <- as.data.frame(sm_tf_deriv) %>%
      mutate(id = row_number()) %>%  # Assign a unique ID for each curve
      pivot_longer(cols = -id, names_to = "x", values_to = "y") %>%
      mutate(x = rep(seq(0, 1, length.out = ncol(sm_tf_deriv)), times = nrow(sm_tf_deriv)))
  } else {
    stop("Invalid order value. Please provide either 0, 1, or 2.")
  }
  
  # Add outlier information
  dat_df <- dat_df %>%
    mutate(outlier = id %in% model_out$true_outliers)
  
  # Create the plot
  plt <- ggplot(dat_df, aes(x = x, y = y, group = id, color = factor(outlier))) + 
    geom_line(linewidth=0.2) +
    scale_color_manual(values=c("#999999", "#FF0000")) +
    #theme_classic(base_size = 5) +
    theme_minimal(base_size = 5) +
    theme(legend.position = "none") +
    ggtitle(title)
  
  return(plt)
}

ind_plot <- function(model, param = NULL, seed = NULL,
                     type= c("MEI", "MEI_d", "MEI_d2", "MHI", "MHI_d", "MHI_d2", 
                             "ABEI", "ABHI", "ABEI_d", "ABHI_d", "ABEI_d2", 
                             "ABHI_d2")){
  library(ehymet) #indices definition
  
  set.seed(seed)
  
  fun_model <- match.fun(model)
  model_out <- rlang::exec(fun_model, !!!param)
  
  sm_tf <-  ind_all(model_out$data)
  sm_tf <- dplyr::mutate(sm_tf,
                         out = 1:nrow(sm_tf) %in% model_out$true_outliers)
  
  sm_ord <- dplyr::arrange(sm_tf, get(type))
  
  ylab_text <- dplyr::case_when(
    grepl("_d2$", type) ~ paste0(sub("_.*", "", type), " (second derivatives)"),
    grepl("_d$", type) ~ paste0(sub("_.*", "", type), " (first derivatives)"),
    TRUE ~ paste0(type, " (original data)")
  )
  
  plt <- ggplot2::ggplot(sm_ord) + 
    ggplot2::geom_point(aes(x=1:nrow(sm_tf),y=get(type), colour=factor(out)),
                        size=0.2)+ 
    ggplot2::scale_color_manual(values=c("#999999", "#FF0000"))+
    ggplot2::xlab("Order") +
    ggplot2::ylab(ylab_text) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
  
  return(plt)
}

ind_plot_combine2 <- function(model, param = NULL, seed = NULL, type = c("_", "d", "d2")) {
  
  set.seed(seed)
  
  fun_model <- match.fun(model)
  model_out <- rlang::exec(fun_model, !!!param)
  
  sm_tf <-  ind_all(model_out$data)
  sm_tf <- dplyr::mutate(sm_tf,
                         out = 1:nrow(sm_tf) %in% model_out$true_outliers)
  
  if(type == "_"){
    plt <- ggplot(sm_tf) +
      geom_point(aes(x=ABEI,y=ABHI, colour=factor(out)), size=0.5)+
      scale_color_manual(values=c("#999999", "#FF0000"))+
      theme_minimal() +
      theme(legend.position = "none")
  } else if(type == "d"){
    plt <- ggplot(sm_tf) +
      geom_point(aes(x=ABEI_d,y=ABHI_d, colour=factor(out)), size=0.5)+
      scale_color_manual(values=c("#999999", "#FF0000"))+
      theme_classic() +
      theme(legend.position = "none")
  } else if(type == "d2"){
    plt <- ggplot(sm_tf) +
      geom_point(aes(x=ABEI_d2,y=ABHI_d2, colour=factor(out)), size=0.5)+
      scale_color_manual(values=c("#999999", "#FF0000"))+
      theme_classic() +
      theme(legend.position = "none")
  } else{
    stop("Invalid type name")
  }
  return(plt)
}

# plt_curves_out <- function(data, pos_x = 0, pos_y = 0, text = ""){
#   
#   df_long_y <- data %>%
#     mutate(id = row_number()) %>%
#     pivot_longer(cols = -id, names_to = "x", values_to = "y") %>%
#     mutate(x=rep(seq(0, 1, length.out = ncol(data)), times = nrow((data))))
#   
#   plt_y <- df_long_y %>% ggplot(aes(x = x, y = y, group = id)) +
#     geom_line(color="gray", linewidth = 0.1) +
#     geom_line(data=filter(df_long_y, id == 7),color = "black", linewidth = 0.5) +
#     geom_line(data=filter(df_long_y, id == 51),color = "red", linewidth = 0.5) +
#     geom_text(aes(x = pos_x, y = pos_y, label = text), color = "red", size = 15) +
#     theme_classic() +
#     theme(legend.position = "none")
#   return(plt_y)
# }
# 
# plt_indices_out <- function(data, type = "ABEI"){
#   
#   df <-  ind_all(data) %>%
#     mutate(id = row_number()) %>%
#     arrange(get(type)) %>%
#     mutate(x = row_number())
#     
#   
#   plt_indM <- df %>%
#     ggplot(aes(x = x, y = get(type))) +
#     geom_point(color="gray", size=1) +
#     geom_point(data=filter(df, id==7), color = "black",size=1) +
#     geom_point(data=filter(df, id == 51),color = "red",size=1) +
#     theme_classic() +
#     xlab("Order") +
#     ylab(type) +
#     theme(legend.position = "none")
#   return(plt_indM)
# }
