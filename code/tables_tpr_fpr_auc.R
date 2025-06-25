library(dplyr)
library(tidyr)
library(kableExtra)

table_files_names <- c("table_simulation_model1", "table_simulation_model2",
                       "table_simulation_model3", "table_simulation_model4",
                       "table_simulation_model5", "table_simulation_model6",
                       "table_simulation_model7", "table_simulation_model8",
                       "table_simulation_model9", "table_simulation_model10",
                       "table_hm", "table_jv1", "table_jv2", "table_jv3",
                       "table_jv4", "table_jv5", "table_sn1", "table_sn2", # Corrected from sn2
                       "table_sphere")

# Generate the DGP names corresponding to the file list
dgp_names <- paste0("DGP", 1:length(table_files_names))

# Other parameters
data_directory <- "results/"
alphas <- c(0.01, 0.05, 0.1)

# Method filtering and renaming setup
original_method_names <- c("MUOD",  "indAB-Comedian", "OG", "MSPLT", "TVD", "MBD", "MDS5LOF", "BP-PWD")
new_method_names      <- c("FASTMUOD",  "EHyOut", "OG", "MSPLOT", "TVD", "MBD", "MDS5LOF", "BP-PWD") # Note: MSPLT -> MSPLOT
method_rename_map <- setNames(new_method_names, original_method_names)


# --- MAIN LOOP TO GENERATE TABLES ---

# We loop using an index `i` to access both file names and DGP names
for (i in seq_along(table_files_names)) {
  
  # Get the specific file name and DGP label for this iteration
  current_file_base_name <- table_files_names[i]
  current_dgp_label <- dgp_names[i]
  
  # cat(paste("--- Processing:", current_dgp_label, "(file:", current_file_base_name, ") ---\n"))
  
  summaries_list <- list()
  
  # Loop through each alpha value to load and process data
  for (alpha_val in alphas) {
    file_path <- file.path(data_directory, paste0(current_file_base_name, "_", alpha_val, "_long.rds"))
    
    # Check if file exists to prevent errors
    if (!file.exists(file_path)) {
      warning(paste("File not found, skipping:", file_path))
      next
    }
    
    raw_data <- readRDS(file_path)
    
    summary_df <- raw_data %>%
      filter(Method %in% original_method_names) %>%
      mutate(Method = recode(Method, !!!method_rename_map)) %>%
      # Ensure the final table has the methods in your desired order
      mutate(Method = factor(Method, levels = new_method_names)) %>%
      group_by(Method) %>%
      summarise(
        TPR_median = median(TPR, na.rm = TRUE),
        TPR_sd = sd(TPR, na.rm = TRUE),
        FPR_median = median(FPR, na.rm = TRUE),
        FPR_sd = sd(FPR, na.rm = TRUE),
        AUC_median = median(AUC, na.rm = TRUE),
        AUC_sd = sd(AUC, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(Alpha = alpha_val)
    
    summaries_list[[as.character(alpha_val)]] <- summary_df
  }
  
  # Skip to next iteration if no data was processed (e.g., all files missing)
  if (length(summaries_list) == 0) {
    cat("No data files found for this DGP. Skipping table generation.\n\n")
    next
  }
  
  combined_summary <- bind_rows(summaries_list)
  
  # Skip if filtering resulted in an empty data frame
  if (nrow(combined_summary) == 0) {
    cat("No data for specified methods found in files for this DGP. Skipping table generation.\n\n")
    next
  }
  
  # Reshape data for the final table layout
  pivoted_data <- combined_summary %>%
    arrange(Method) %>% # Sort rows based on the factor level order
    mutate(
      TPR = sprintf("%.3f (%.3f)", TPR_median, TPR_sd),
      FPR = sprintf("%.3f (%.3f)", FPR_median, FPR_sd),
      AUC = sprintf("%.3f (%.3f)", AUC_median, AUC_sd)
    ) %>%
    select(Method, Alpha, TPR, FPR, AUC) %>%
    pivot_wider(
      id_cols = Method,
      names_from = Alpha,
      values_from = c(TPR, FPR, AUC)
    )
  
  # Define column order
  sorted_alphas <- sort(alphas)
  column_order <- c(
    "Method",
    as.vector(outer(c("TPR", "FPR", "AUC"), sorted_alphas, paste, sep = "_"))
  )
  pivoted_data <- pivoted_data %>%
    select(all_of(column_order))
  
  # Define the multi-level header
  header_spec <- c(" ", setNames(rep(3, length(sorted_alphas)), paste("$\\alpha$ =", sorted_alphas)))
  
  latex_table <- kable(
    pivoted_data,
    format = "latex",
    booktabs = TRUE,
    # MODIFIED: Use standard table, not longtable
    longtable = FALSE, 
    # MODIFIED: Use the DGP label for caption and label
    caption = paste("Results of outlier detection methodologies for", current_dgp_label, 
                    "and different outliers proportions ($\alpha$). 
                    The values correspond to the mean (sd) of TPR, FPR and AUC for 100 simulations."),
    # label = paste0("tab:", current_dgp_label),
    # The sub-headers (second row of the header)
    col.names = c("Method", rep(c("TPR", "FPR", "AUC"), length(sorted_alphas))),
    align = c("l", rep("c", ncol(pivoted_data) - 1)),
    # Use escape = FALSE to render the \alpha in the header
    escape = FALSE 
  ) %>%
    # Use `add_header_above` for the top-level header
    add_header_above(header_spec) 
  
  # MODIFIED: The `footnote()` call has been completely removed.
  
  print(latex_table)
  cat("\n\n") 
}
