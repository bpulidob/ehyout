library(dplyr)    # For data manipulation (bind_rows, mutate, filter, group_by)
library(purrr)    # For functional programming (map_dfr)
library(tidyr)    # For reshaping data (pivot_longer)
library(ggplot2)  # For plotting
library(stringr)  # For string manipulation (optional, for cleaning names)
library(patchwork)

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

mcc_subset_data <- results_long %>%
  filter(
    Metric == "MCC",
    DGP %in% selected_dgps
  ) %>%
  mutate(
    # Ensure Method factor uses the predefined order
    Method = factor(Method, levels = method_levels),
    # Ensure DGP factor uses only the selected DGPs and in the specified order
    DGP = factor(DGP, levels = selected_dgps)
  )

plot_mcc_specific_dgps <- ggplot(mcc_subset_data,
                                 # X is Method, Y is MCC Value, Fill distinguishes DGPs
                                 aes(x = Method, y = Value, fill = DGP)) +
  # Create dodged boxplots: one box per DGP for each Method
  geom_boxplot(outlier.size = 0.5, notch = FALSE,
               position = position_dodge(width = 0.85)) + # Adjust width as needed
  # Use a palette suitable for 5 categories
  scale_fill_brewer(palette = "Pastel1") + #  "Set2", "Set3", "Accent", "Pastel1"
  #scale_fill_viridis_d(guide = "none") +
  # scale_fill_viridis_d(option="C") + # Viridis alternative
  labs(
    title = "MCC Performance Comparison by DGP",
    subtitle = paste("Showing DGPs:", paste(selected_dgps, collapse=", ")),
    x = "",
    y = "MCC",
    fill = "DGP" # Legend title for DGP colors
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9), # Rotate method names if needed
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  guides(fill = guide_legend(nrow = 1)) # Arrange legend

p1 <- plot_mcc_specific_dgps + labs(title = NULL)
p2 <- plot_mcc_specific_dgps + labs(title = NULL)
p3 <- plot_mcc_specific_dgps + labs(title = NULL)
p4 <- plot_mcc_specific_dgps + labs(title = NULL)

final_plot <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    # title = "MCC Performance Comparison by DGP",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  )

ggsave("plot_DGPs_MCC.pdf", 
       plot = final_plot,
       path = "results/plots", width = 14, height = 8, device = "pdf")


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
    outlier.size = 0.5, # <--- REDUCE THIS VALUE (e.g., to 0.5, 0.75, etc.)
    # outlier.shape = 16, # Default shape is a solid circle (19 or 16 usually). You can change it.
    # outlier.alpha = 0.8, # You can also make them slightly transparent
    notch = FALSE
  ) +
  # Add individual points (medians from each DGP) with jitter
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
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9), # Rotate if names overlap
    legend.position = "none", # Legend is redundant with x-axis and fill
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

# Check the pivoted data
# print(head(data_for_plot))
# Expected columns: DGP, Method, SimulationID, MCC, Time

# 2. Group by Method and calculate summary statistics
# This step effectively "forgets" DGP and SimulationID by aggregating over them.
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
  scale_x_log10( # Apply log10 transformation to the x-axis
    breaks = scales::trans_breaks("log10", function(x) 10^x), # Nicer breaks
    labels = scales::trans_format("log10", scales::math_format(10^.x)) # Nicer labels (e.g., 10^0, 10^1)
    # Or use scales::label_number() for decimal, 
    # or scales::label_log() for e.g. "1e-2", "1e0"
  ) +
  scale_color_brewer(palette = "Set2") +
  # annotation_logticks(sides = "b") + # Optional: add log tick marks on the bottom axis
  labs(
    # title = "Performance Comparison: Mean MCC vs. Mean Time (Log Scale)",
    # subtitle = "Error bars represent Interquartile Range (IQR) of MCC for each Method",
    x = "Mean Time (seconds, log scale)", # Updated x-axis label
    y = "Mean MCC",
    color = "Method"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1) # Angle x-axis labels if they overlap
  )

print(p_log_time)

ggsave("plot_time.pdf", 
       plot = p_log_time,
       path = "results/plots", width = 7, height = 4.5, device = "pdf")
