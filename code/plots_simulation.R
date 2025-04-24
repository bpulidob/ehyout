library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)
library(patchwork) # For combining plots
library(ggrepel) # For non-overlapping labels in scatter plots
library(viridis) # For colorblind-friendly palettes

# Data Loading and Preparation

# --- Define DGPs and Methods ---
# List of your DGP source files (ensure 'EHyOut' results are in these)
DGP_FILES <- c("table_simulation_model1_100.rds", "table_simulation_model2_100.rds",
               "table_simulation_model3_100.rds", "table_simulation_model4_100.rds",
               "table_simulation_model5_100.rds", "table_simulation_model6_100.rds",
               "table_simulation_model7_100.rds", "table_simulation_model8_100.rds",
               "table_simulation_model9_100.rds", "table_simulation_model10_100.rds"
               # "table_hm100.rds",
               # "table_jv1100.rds", "table_jv2100.rds", "table_jv3100.rds",
               # "table_jv4100.rds", "table_jv5100.rds",
               # "table_sn1100.rds", "table_sn2100.rds",
               # "prueba_table_sphere100.rds"
               ) # Total 19 files/DGPs

# Methods to include in the comparison (ensure names match EXACTLY in your files)
# IMPORTANT: Added "EHyOut". Corrected "indM-OGK" if it should be just "OGK", etc. Verify these names!
METHODS_TO_KEEP <- c("indAB-Comedian", # Your method!
                     "MUOD",   # Assuming this is the intended name for comparison
                     "OG",     # Or "indM-OGK"? Choose one. Assuming base OG is better comparison.
                     "MSPLT",  # Check if it's MSPLT or MSPLOT
                     "TVD",
                     "MBD",
                     "MDS5LOF",# Assuming MDS5LOF is the specific MDS-LOF variant
                     "BP-PWD"
)


#' Load and Prepare Simulation Data for a Specific Outlier Rate
#'
#' Reads data from specified RDS files for multiple DGPs, extracts results
#' for a given outlier rate, filters methods, and returns numeric metrics.
#'
#' @param outlier_rate Numeric. The outlier proportion (e.g., 0.1, 0.05, 0.01).
#' @param dgp_files Character vector. List of RDS file names containing results.
#' @param results_path Character. Path to the directory containing the RDS files.
#' @param methods_to_keep Character vector. Names of methods to include.
#'
#' @return A data frame with columns DGP, Method, and numeric metrics
#'         (TPR_m, FPR_m, MCC_m, BA_m, Time_m, etc., depending on availability).
#'         Returns NULL if a required column is missing.
load_and_prepare_data <- function(outlier_rate,
                                  dgp_files = DGP_FILES,
                                  results_path = "results/",
                                  methods_to_keep = METHODS_TO_KEEP) {
  
  combined_data_list <- list() # More efficient to store in list first
  
  required_metrics_mean <- c("TPR_m", "FPR_m", "MCC_m", "BA_m", "Time_m")
  required_metrics_median <- c("TPR_med", "FPR_med", "MCC_med", "BA_med", "Time_med")
  # Add _sd columns if needed for error bars later, e.g., Time_sd, MCC_sd
  required_metrics_sd <- c("MCC_sd", "BA_sd", "Time_sd")
  
  for (file_ind in 1:length(dgp_files)) {
    file_path <- file.path(results_path, dgp_files[file_ind])
    if (!file.exists(file_path)) {
      warning("File not found: ", file_path, ". Skipping.")
      next
    }
    
    # Read file
    file_read <- readRDS(file_path)
    
    # --- Determine column indices for the selected outlier rate ---
    # Assumes first column is index/row number, second is Method
    n_metrics_total <- ncol(file_read) - 1 # Total metrics columns (excluding Method)
    if (n_metrics_total %% 3 != 0) {
      warning("Unexpected number of columns in ", dgp_files[file_ind], ". Column structure might be wrong. Assuming 3 blocks.")
      # You might need more robust logic here if column counts vary wildly
    }
    n_metrics_per_block <- n_metrics_total / 3
    
    start_col <- NULL
    if (outlier_rate == 0.10) {
      start_col <- 2 # Start after Method column
      end_col <- start_col + n_metrics_per_block - 1
    } else if (outlier_rate == 0.05) {
      start_col <- 2 + n_metrics_per_block
      end_col <- start_col + n_metrics_per_block - 1
    } else if (outlier_rate == 0.01) {
      start_col <- 2 + 2 * n_metrics_per_block
      end_col <- start_col + n_metrics_per_block - 1
    } else {
      stop("Invalid outlier rate: ", outlier_rate)
    }
    
    # Select the relevant block of columns including 'Method' (column 1 if 1-based)
    # Using indices based on example table (col 1 is index, col 2 is Method)
    if (names(file_read)[1] == "Method") { # If Method is the very first column
      method_col_index <- 1
      metric_indices <- (start_col-1):(end_col-1) # Adjust indices if method is col 1
      data_subset <- file_read[, c(method_col_index, metric_indices)]
      # Clean column names (remove potential .1, .2 suffixes if structure is different)
      # Basic cleaning: remove trailing .number suffix
      base_colnames <- stringr::str_remove(names(data_subset)[-1], "\\.\\d+$")
      names(data_subset) <- c("Method", base_colnames)
      
    } else if (names(file_read)[2] == "Method") { # If Method is the second column
      method_col_index <- 2
      metric_indices <- start_col:end_col # Indices of the metric columns
      data_subset <- file_read[, c(method_col_index, metric_indices)]
      # Clean column names
      base_colnames <- stringr::str_remove(names(data_subset)[-1], "\\.\\d+$")
      names(data_subset) <- c("Method", base_colnames)
    } else {
      stop("Cannot find 'Method' column in expected position (1st or 2nd) in file: ", dgp_files[file_ind])
    }
    
    
    # --- Check if required metric columns exist ---
    available_cols <- names(data_subset)
    required_numeric_cols <- c(required_metrics_mean, required_metrics_median, required_metrics_sd)
    missing_cols <- setdiff(required_numeric_cols, available_cols)
    
    if (length(missing_cols) > 0) {
      warning("Missing required columns in ", dgp_files[file_ind], " for OR=", outlier_rate, ": ",
              paste(missing_cols, collapse=", "), ". Skipping this file for this OR.")
      next # Skip this file if essential metrics are missing
    }
    
    # --- Filter methods and select final numeric columns ---
    data_filtered <- data_subset %>%
      filter(Method %in% methods_to_keep) %>%
      # Select only the numeric columns we definitely need for plots + Method
      select(Method, all_of(required_numeric_cols)) %>%
      # Convert all selected metric columns to numeric, coercing errors to NA
      mutate(across(-Method, ~as.numeric(as.character(.)))) %>%
      # Handle potential NaN MCCs (replace with -1, 0, or NA based on preference)
      mutate(MCC_m = if_else(is.nan(MCC_m), -1, MCC_m),
             MCC_med = if_else(is.nan(MCC_med), -1, MCC_med)) %>%
      mutate(DGP = paste0("DGP", file_ind)) # Add DGP identifier
    
    combined_data_list[[length(combined_data_list) + 1]] <- data_filtered
  }
  
  if (length(combined_data_list) == 0) {
    warning("No data loaded successfully for outlier rate: ", outlier_rate)
    return(NULL)
  }
  
  # Combine all DGPs into one data frame
  final_data <- bind_rows(combined_data_list)
  
  # Convert Method and DGP to factors for consistent plotting order
  final_data <- final_data %>%
    mutate(
      Method = factor(Method, levels = methods_to_keep), # Use provided order
      DGP = factor(DGP)
    )
  
  return(final_data)
}

TARGET_OR_MAIN <- 0.05

# Load data for the target OR
main_paper_data <- load_and_prepare_data(outlier_rate = TARGET_OR_MAIN)


  
#' Plot Summary Boxplots Across DGPs
#'
#' Creates boxplots showing the distribution of a chosen metric across all DGPs
#' for each method.
#'
#' @param data A data frame from `load_and_prepare_data`.
#' @param metric_col Character. The name of the column to plot (e.g., "MCC_med", "BA_m").
#' @param title Character. The main title for the plot.
#' @param ylab Character. Y-axis label.
#'
#' @return A ggplot object.
plot_summary_boxplots <- function(data, metric_col, title, ylab) {
  if (!metric_col %in% names(data)) {
    stop("Metric column '", metric_col, "' not found in data.")
  }
  
  p <- ggplot(data, aes(x = Method, y = .data[[metric_col]], fill = Method)) +
    geom_boxplot(outlier.alpha = 0.6, notch = FALSE) + # Notches can be messy with many groups
    scale_fill_viridis_d(guide = "none") + # Colorblind friendly palette
    labs(
      title = title,
      subtitle = paste("Distribution across", n_distinct(data$DGP), "DGPs"),
      x = "Method",
      y = ylab
    ) +
    theme_bw(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

# Plot 1: Boxplot of Median MCC
plot_main_box_mcc <- plot_summary_boxplots(
  data = main_paper_data,
  metric_col = "MCC_med", # Use median MCC - often robust
  title = paste("Method Performance Comparison (Median MCC, OR =", TARGET_OR_MAIN, ")"),
  ylab = "Median MCC per DGP"
)
print(plot_main_box_mcc)
# ggsave(paste0("main_boxplot_mcc_med_or", TARGET_OR_MAIN*100, ".png"), plot_main_box_mcc, width = 8, height = 5)

# Plot 1: Boxplot of Median BA
plot_main_box_ba <- plot_summary_boxplots(
  data = main_paper_data,
  metric_col = "BA_med", # Use median MCC - often robust
  title = paste("Method Performance Comparison (Median MCC, OR =", TARGET_OR_MAIN, ")"),
  ylab = "Median BA per DGP"
)
print(plot_main_box_ba)

# Optional Plot 4: Boxplot of Mean FPR (to show false alarms)
plot_main_box_fpr <- plot_summary_boxplots(
  data = main_paper_data,
  metric_col = "FPR_m",
  title = paste("False Positive Rate Distribution (Mean FPR, OR =", TARGET_OR_MAIN, ")"),
  ylab = "Mean FPR per DGP"
)
print(plot_main_box_fpr) # Useful for discussion but maybe SM




#' Plot Performance vs. Execution Time
#'
#' Creates a scatter plot comparing average performance (e.g., MCC, BA) against
#' average execution time, aggregated across DGPs.
#'
#' @param data A data frame from `load_and_prepare_data`.
#' @param perf_metric Character. Column name for the performance metric (e.g., "MCC_m").
#' @param time_metric Character. Column name for the time metric (e.g., "Time_m").
#' @param perf_label Character. Label for the performance axis.
#' @param title Character. Plot title.
#'
#' @return A ggplot object.
plot_performance_vs_time <- function(data, perf_metric, time_metric, perf_label, title) {
  
  if (!perf_metric %in% names(data) || !time_metric %in% names(data)) {
    stop("Performance or time metric column not found in data.")
  }
  
  # Aggregate performance and time across DGPs
  agg_summary <- data %>%
    group_by(Method) %>%
    summarise(
      Mean_Perf = mean(.data[[perf_metric]], na.rm = TRUE),
      Mean_Time = mean(.data[[time_metric]], na.rm = TRUE),
      SD_Time = sd(.data[[time_metric]], na.rm = TRUE), # For context maybe
      .groups = 'drop' # Drop grouping
    )
  
  p <- ggplot(agg_summary, aes(x = Mean_Time, y = Mean_Perf, color = Method)) +
    geom_point(size = 4, alpha = 0.8) +
    # Use ggrepel for potentially overlapping labels
    geom_text_repel(aes(label = Method), size = 3.5, max.overlaps = Inf,
                    box.padding = 0.5, point.padding = 0.5) +
    scale_x_log10(labels = scales::label_number_auto()) + # Log scale usually needed for time
    scale_color_viridis_d() +
    labs(
      title = title,
      subtitle = paste("Values averaged over", n_distinct(data$DGP), "DGPs"),
      x = "Mean Execution Time (log scale, seconds)", # Assuming time is in seconds
      y = paste("Mean", perf_label, "(across DGPs)"),
      color = "Method"
    ) +
    theme_bw(base_size = 12) +
    theme(legend.position = "none") # Labels directly on plot
  
  return(p)
}

# Plot 2: Performance (Mean MCC) vs Time
plot_main_perf_time <- plot_performance_vs_time(
  data = main_paper_data,
  perf_metric = "MCC_m",    # Use mean MCC for aggregation
  time_metric = "Time_m",   # Use mean Time
  perf_label = "MCC",
  title = paste("Performance (Mean MCC) vs. Execution Time (OR =", TARGET_OR_MAIN, ")")
)
print(plot_main_perf_time)
# ggsave(paste0("main_perf_time_mcc_or", TARGET_OR_MAIN*100, ".png"), plot_main_perf_time, width = 8, height = 6)

# Plot 2: Performance (Mean BA) vs Time
plot_main_perf_time_BA <- plot_performance_vs_time(
  data = main_paper_data,
  perf_metric = "BA_m",    # Use mean MCC for aggregation
  time_metric = "Time_m",   # Use mean Time
  perf_label = "BA",
  title = paste("Performance (Mean BA) vs. Execution Time (OR =", TARGET_OR_MAIN, ")")
)
print(plot_main_perf_time_BA)


#' Plot Performance Heatmap Across Methods and DGPs
#'
#' Creates a heatmap showing a specific performance metric for each method
#' across all DGPs.
#'
#' @param data A data frame from `load_and_prepare_data`.
#' @param metric_col Character. The column name of the metric to plot (e.g., "MCC_m").
#' @param title Character. Plot title.
#' @param fill_label Character. Label for the color scale legend.
#' @param high_is_better Logical. If TRUE, uses a palette where darker/higher values are better.
#'
#' @return A ggplot object.
plot_heatmap <- function(data, metric_col, title, fill_label, high_is_better = TRUE) {
  
  if (!metric_col %in% names(data)) {
    stop("Metric column '", metric_col, "' not found in data.")
  }
  
  # Determine color palette direction
  palette_option <- "viridis" # Default good option
  palette_direction <- if (high_is_better) 1 else -1
  
  # Determine text color based on fill brightness (simple approach)
  data <- data %>% mutate(text_color = ifelse(.data[[metric_col]] > 0.6, "white", "black")) # Adjust threshold as needed
  
  p <- ggplot(data, aes(x = Method, y = fct_rev(DGP), fill = .data[[metric_col]])) +
    geom_tile(color = "white", linewidth = 0.3) +
    geom_text(aes(label = sprintf("%.2f", .data[[metric_col]]), color = text_color), size = 2.5, na.rm = TRUE) +
    scale_fill_viridis_c(option = palette_option, direction = palette_direction,
                         na.value = "grey80", limits = c(floor(min(data[[metric_col]], na.rm=TRUE)*10)/10, 1)) + # Adjust limits if needed
    scale_color_identity() + # Use the text_color column directly
    labs(
      title = title,
      subtitle = paste("Performance on", n_distinct(data$DGP), "DGPs"),
      x = "Method",
      y = "Data Generation Process (DGP)",
      fill = fill_label
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      legend.position = "right"
    )
  
  return(p)
}


# Plot 3: Heatmap of Mean MCC
plot_main_heatmap_mcc <- plot_heatmap(
  data = main_paper_data,
  metric_col = "MCC_m",
  title = paste("Mean MCC Across Methods and DGPs (OR =", TARGET_OR_MAIN, ")"),
  fill_label = "Mean MCC",
  high_is_better = TRUE
)
print(plot_main_heatmap_mcc)
# ggsave(paste0("main_heatmap_mcc_or", TARGET_OR_MAIN*100, ".png"), plot_main_heatmap_mcc, width = 9, height = 10)


# Plot 3: Heatmap of Mean BA
plot_main_heatmap_ba <- plot_heatmap(
  data = main_paper_data,
  metric_col = "BA_m",
  title = paste("Mean BA Across Methods and DGPs (OR =", TARGET_OR_MAIN, ")"),
  fill_label = "Mean BA",
  high_is_better = TRUE
)
print(plot_main_heatmap_ba)
