library(dplyr)    # For data manipulation (bind_rows, mutate, filter, group_by)
library(purrr)    # For functional programming (map_dfr)
library(tidyr)    # For reshaping data (pivot_longer)
library(ggplot2)  # For plotting
library(stringr)  # For string manipulation (optional, for cleaning names)
library(patchwork)
library(RColorBrewer)

table_files <- c("table_simulation_model1_0.1_long.rds", "table_simulation_model2_0.1_long.rds",
                 "table_simulation_model3_0.1_long.rds", "table_simulation_model4_0.1_long.rds",
                 "table_simulation_model5_0.1_long.rds", "table_simulation_model6_0.1_long.rds",
                 "table_simulation_model7_0.1_long.rds", "table_simulation_model8_0.1_long.rds",
                 "table_simulation_model9_0.1_long.rds", "table_simulation_model10_0.1_long.rds",
                 "table_hm_0.1_long.rds",
                 "table_jv1_0.1_long.rds", "table_jv2_0.1_long.rds", "table_jv3_0.1_long.rds",
                 "table_jv4_0.1_long.rds", "table_jv5_0.1_long.rds",
                 "table_sn1_0.1_long.rds", "table_sn2_0.1_long.rds",
                 "table_sphere_0.1_long.rds"
)
table_files_full_path <- file.path("results/", table_files)
dgp_names <- paste0("DGP", 1:length(table_files_full_path))

names(table_files_full_path) <- dgp_names

all_results_raw <- map_dfr(table_files_full_path, readRDS, .id = "DGP")

# Define the original method names to keep and their desired new names
original_method_names <- c("MUOD",  "indAB-Comedian", "OG", "MSPLT", "TVD", "MBD", "MDS5LOF", "BP-PWD")
new_method_names      <- c("FASTMUOD",  "EHyOut", "OG", "MSPLOT", "TVD", "MBD", "MDS5LOF", "BP-PWD") # Note: MSPLT -> MSPLOT

# Create a mapping for renaming (useful for recode)
rename_map <- setNames(new_method_names, original_method_names)

results_filtered_renamed <- all_results_raw %>%
  filter(Method %in% original_method_names) %>%
  mutate(Method = recode(Method, !!!rename_map))

results_long <- results_filtered_renamed %>%
  group_by(DGP, Method) %>%
  mutate(SimulationID = row_number()) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(TPR, FPR, F1, MCC, AC, BA, AUC, Time), # List all metric columns
    names_to = "Metric",
    values_to = "Value"
  )

method_levels <- c("EHyOut", "FASTMUOD", "OG", "MSPLOT", "TVD", "MBD", "MDS5LOF", "BP-PWD")

metric_levels <- c("TPR", "FPR", "AUC", "BA", "F1", "MCC", "AC", "Time")

results_long <- results_long %>%
  mutate(
    # Use the new method names and the defined order
    Method = factor(Method, levels = method_levels),
    DGP = factor(DGP, levels = dgp_names),
    Metric = factor(Metric, levels = metric_levels),
    Value = ifelse(!is.finite(Value), 0, Value),
  )

selected_dgps <- c("DGP1", "DGP2", "DGP3", "DGP4", "DGP9")
selected_dgps <- c("DGP5", "DGP6", "DGP7", "DGP8", "DGP10")
selected_dgps <- c("DGP12", "DGP13", "DGP14", "DGP15", "DGP16")
selected_dgps <- c("DGP11", "DGP17", "DGP18", "DGP19")


create_mcc_plot_by_dgp <- function(selected_dgps, full_data, method_levels, plot_subtitle) {
  
  # Filter data for the specific DGPs and metric
  plot_data <- full_data %>%
    filter(
      Metric == "MCC",
      DGP %in% selected_dgps
    ) %>%
    mutate(
      # Ensure factor levels are correctly ordered for plotting
      Method = factor(Method, levels = method_levels),
      DGP = factor(DGP, levels = selected_dgps)
    )
  
  # Create the plot
  ggplot(plot_data,
         aes(x = DGP, y = Value, fill = Method)) +
    # Create dodged boxplots: one box per Method for each DGP.
    geom_boxplot(outlier.size = 0.5, notch = FALSE,
                 position = position_dodge(width = 0.9)) + 
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = NULL, 
      subtitle = plot_subtitle,
      x = "",
      y = "MCC",
      fill = "Method" 
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      plot.subtitle = element_text(hjust = 0.5, size = 13),
      legend.position = "bottom" 
    )
}

# Define the groups of DGPs for the four plots
dgp_group1 <- c("DGP1", "DGP2", "DGP3", "DGP4", "DGP9")
dgp_group2 <- c("DGP5", "DGP6", "DGP7", "DGP8", "DGP10")
dgp_group3 <- c("DGP12", "DGP13", "DGP14", "DGP15", "DGP16")
dgp_group4 <- c("DGP11", "DGP17", "DGP18", "DGP19")

subtitle1 <- "Magnitude outliers"
subtitle2 <- "Shape outliers"
subtitle3 <- "Complex shape and magnitude outliers"
subtitle4 <- "Complex shape outliers"

# Generate each of the four plots by calling the new function
p1 <- create_mcc_plot_by_dgp(dgp_group1, results_long, method_levels, subtitle1)
p2 <- create_mcc_plot_by_dgp(dgp_group2, results_long, method_levels, subtitle2)
p3 <- create_mcc_plot_by_dgp(dgp_group3, results_long, method_levels, subtitle3)
p4 <- create_mcc_plot_by_dgp(dgp_group4, results_long, method_levels, subtitle4)

# Combine the plots using patchwork, adding a common title and a single shared legend
final_plot <- (p1 + p2) / (p3 + p4) +
  plot_layout(guides = "collect") + # Collects legends from all plots into one
  plot_annotation(
    # title = "MCC Performance Comparison by Method across DGPs",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      legend.position = "bottom" # Position the shared legend at the bottom
    )
  ) & 
  theme(legend.key.size = unit(0.4, 'cm')) 

# Display the final plot
# print(final_plot)

# Save the final combined plot
ggsave("plot_DGPs_MCC_grouped_by_DGP.pdf", 
       plot = final_plot,
       path = "results/plots", width = 14, height = 10, device = "pdf")



## Plot AUC
auc_method_levels <- c("EHyOut", "OG", "MBD", "MDS5LOF")

method_color_map <- setNames(
  brewer.pal(n = length(method_levels), name = "Set2"), 
  method_levels
)

results_long_auc <- results_filtered_renamed %>%
  filter(Method %in% auc_method_levels) %>%
  group_by(DGP, Method) %>%
  mutate(SimulationID = row_number()) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(TPR, FPR, F1, MCC, AC, BA, AUC, Time),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    # Set factor levels for consistent ordering and color mapping
    Method = factor(Method, levels = auc_method_levels),
    DGP = factor(DGP, levels = dgp_names),
    Metric = factor(Metric),
    Value = ifelse(!is.finite(Value), 0, Value)
  )

create_auc_plot_by_dgp <- function(selected_dgps, full_data, method_levels, plot_subtitle) {
  
  # Filter data for the specific DGPs and the AUC metric
  plot_data <- full_data %>%
    filter(
      Metric == "AUC",
      DGP %in% selected_dgps
    ) %>%
    mutate(
      # Ensure factor levels are correctly ordered for plotting
      Method = factor(Method, levels = method_levels),
      DGP = factor(DGP, levels = selected_dgps)
    )
  
  # Create the plot
  ggplot(plot_data, aes(x = DGP, y = Value, fill = Method)) +
    geom_boxplot(outlier.size = 0.5, notch = FALSE, position = position_dodge(width = 0.9)) +
    # Set the y-axis range to be [0, 1] for AUC, which is standard
    coord_cartesian(ylim = c(0, 1)) +
    # scale_fill_brewer(palette = "Set2") +
    scale_fill_manual(values = method_color_map) +
    labs(
      title = NULL,
      subtitle = plot_subtitle,
      x = "",
      y = "AUC", # Changed label to AUC
      fill = "Method" 
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      plot.subtitle = element_text(hjust = 0.5, size = 13),
      legend.position = "bottom" 
    )
}

# Generate each of the four AUC plots by calling the new function
p1_auc <- create_auc_plot_by_dgp(dgp_group1, results_long_auc, auc_method_levels, subtitle1)
p2_auc <- create_auc_plot_by_dgp(dgp_group2, results_long_auc, auc_method_levels, subtitle2)
p3_auc <- create_auc_plot_by_dgp(dgp_group3, results_long_auc, auc_method_levels, subtitle3)
p4_auc <- create_auc_plot_by_dgp(dgp_group4, results_long_auc, auc_method_levels, subtitle4)

# Combine the plots using patchwork
final_auc_plot <- (p1_auc + p2_auc) / (p3_auc + p4_auc) +
  plot_layout(guides = "collect") + # Collect legends into one
  plot_annotation(
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      legend.position = "bottom"
    )
  ) &
  theme(legend.key.size = unit(0.4, 'cm'))

# Display the final plot
print(final_auc_plot)

# Save the final combined plot with a new name
ggsave("plot_DGPs_AUC_grouped_by_DGP.pdf", 
       plot = final_auc_plot,
       path = "results/plots", width = 14, height = 10, device = "pdf")



results_long_factored <- results_long %>%
  mutate(
    Method = factor(Method, levels = method_levels),
    DGP = factor(DGP, levels = dgp_names), # Ensure DGP factor is set for plotting order
    Metric = factor(Metric, levels = metric_levels)
  )

median_mcc_data <- results_long_factored %>%
  filter(Metric == "MCC") %>%
  group_by(DGP, Method) %>% # Group by BOTH DGP and Method
  summarize(
    Median_MCC = median(Value, na.rm = TRUE), # Calculate median MCC for the 100 simulations
    .groups = 'drop' # Drop grouping structure after summarizing
  )

plot_boxplot_median_mcc <- ggplot(median_mcc_data,
                                  # X is Method, Y is the Median_MCC calculated per DGP
                                  aes(x = Method, y = Median_MCC, fill = Method)) +
  # One boxplot per method, summarizing the 19 median values
  geom_boxplot(
    outlier.size = 0.5, 
    notch = FALSE
  ) +
  geom_jitter(width = 0.1, height = 0, size = 1, alpha = 0.4, color = "grey20") +
  scale_fill_brewer(palette = "Set2") + # Or Paired, Dark2, etc.
  labs(
    # title = "Distribution of Median MCC Performance Across DGPs",
    x = "",
    y = "Median MCC (calculated per DGP)",
    fill = "Methodology" # Fill aesthetic matches x-axis
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9), 
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  guides(color = guide_legend(nrow = 1))

ggsave("plot_bp_median_MCC.pdf", 
       plot = plot_boxplot_median_mcc,
       path = "results/plots", width = 7, height = 4.5, device = "pdf")







data_for_plot <- results_long %>%
  filter(Metric %in% c("MCC", "Time")) %>%
  pivot_wider(
    id_cols = c(DGP, Method, SimulationID), # Columns to keep that identify a unique observation set
    names_from = Metric,
    values_from = Value
  )

data_for_plot <- data_for_plot %>%
  mutate(Method = factor(Method, levels = method_levels))


# Group by Method and calculate summary statistics
summary_df <- data_for_plot %>%
  group_by(Method) %>%
  summarise(
    Mean_Time = mean(Time, na.rm = TRUE),
    Mean_MCC = mean(MCC, na.rm = TRUE),
    MCC_Q1 = quantile(MCC, 0.25, na.rm = TRUE), # 25th percentile for MCC
    MCC_Q3 = quantile(MCC, 0.75, na.rm = TRUE), # 75th percentile for MCC
    .groups = 'drop' # drop grouping structure after summarise
  )

print(summary_df)
# xtable(summary_df, digits=3)


p_log_time <- ggplot(summary_df, aes(x = Mean_Time, y = Mean_MCC, color = Method)) +
  geom_point(size = 3.5) +
  geom_linerange(aes(ymin = MCC_Q1, ymax = MCC_Q3), linewidth = 0.8) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x), 
    labels = scales::trans_format("log10", scales::math_format(10^.x)) 

  ) +
  scale_color_brewer(palette = "Set2") +
  labs(
    # title = "Performance Comparison: Mean MCC vs. Mean Time (Log Scale)",
    # subtitle = "Error bars represent Interquartile Range (IQR) of MCC for each Method",
    x = "Mean Time (seconds, log scale)", 
    y = "Mean MCC",
    color = "Method"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )

print(p_log_time)

ggsave("plot_time.pdf", 
       plot = p_log_time,
       path = "results/plots", width = 7, height = 4.5, device = "pdf")
