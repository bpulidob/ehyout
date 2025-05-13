library(dplyr)    # For data manipulation (bind_rows, mutate, filter, group_by)
library(purrr)    # For functional programming (map_dfr)
library(tidyr)    # For reshaping data (pivot_longer)
library(ggplot2)  # For plotting
library(stringr)  # For string manipulation (optional, for cleaning names)

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
    Metric = factor(Metric, levels = metric_levels)
  )

performance_data <- results_long %>%
  filter(Metric %in% c("TPR", "FPR")) %>%
  mutate(Metric = factor(Metric, levels = intersect(metric_levels, unique(Metric))))


plot_performance <- ggplot(performance_data,
                           aes(x = DGP, y = Value, fill = Method)) +
  geom_boxplot(outlier.size = 0.5,
               notch = FALSE,
               position = position_dodge(width = 0.8)) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 3) + # Adjust ncol (e.g., 3 or 4)
  # Using a specific Brewer palette might be good for 8 distinct colors
  scale_fill_brewer(palette = "Set2") + # Example: "Set2", "Paired", "Dark2"
  # Or keep Viridis: scale_fill_viridis_d(option = "D") +
  labs(
    title = "Comparison of Methodology Performance Across DGPs",
    subtitle = "Distribution over 100 simulations per DGP/Method",
    x = "Data Generation Process (DGP)",
    y = "Metric Value",
    fill = "Methodology"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7), # Vertical labels
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.spacing = unit(0.8, "lines")
  ) +
  guides(fill = guide_legend(nrow = 1)) # Ensure legend is in one row if possible

print(plot_performance)

selected_metric <- "BA"  # Options: "TPR", "FPR", "F1", "MCC", "AC", "BA", "AUC"

# Filter data for the selected metric
single_metric_data <- results_long %>%
  filter(Metric == selected_metric)

plot_single_metric <- ggplot(single_metric_data,
                             aes(x = DGP, y = Value, fill = Method)) +
  # Dodged boxplots: comparison within each DGP
  geom_boxplot(outlier.size = 0.5,
               notch = FALSE,
               position = position_dodge(width = 0.8)) +
  scale_fill_brewer(palette = "Set2") + # Or Set1, Paired, Dark2, Viridis etc.
  labs(
    title = paste("Comparison of Methodology Performance Across DGPs - Metric:", selected_metric),
    subtitle = "Distribution over 100 simulations per DGP/Method",
    x = "Data Generation Process (DGP)",
    y = paste(selected_metric, "Value"), # Label y-axis with the specific metric
    fill = "Methodology"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8), # Rotate x-axis labels
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  guides(fill = guide_legend(nrow = 1)) # Arrange legend in one row

print(plot_single_metric)

plot_single_metric_agg <- ggplot(single_metric_data,
                             aes(x = Method, y = Value)) +
  # Dodged boxplots: comparison within each DGP
  geom_boxplot(outlier.size = 0.5,
               notch = FALSE,
               position = position_dodge(width = 0.8)) +
  # scale_fill_brewer(palette = "Set2") + # Or Set1, Paired, Dark2, Viridis etc.
  labs(
    title = paste("Comparison of Methodology Performance Across DGPs - Metric:", selected_metric),
    subtitle = "Distribution over 100 simulations per DGP/Method",
    x = "Data Generation Process (DGP)",
    y = paste(selected_metric, "Value"), # Label y-axis with the specific metric
    fill = "Methodology"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8), # Rotate x-axis labels
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) 

print(plot_single_metric_agg)

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

# Check the summarized data structure
# head(median_mcc_data)
# glimpse(median_mcc_data) # Should have DGP, Method, Median_MCC columns

# Plot the median MCC values
plot_median_mcc <- ggplot(median_mcc_data,
                          aes(x = DGP, y = Median_MCC, color = Method, group = Method)) +
  # Use group=Method explicitly for lines
  geom_line(linewidth = 0.8) +  # Draw lines connecting medians for each method
  geom_point(size = 2) +       # Add points to mark the median at each DGP
  scale_color_brewer(palette = "Set2") + # Use a distinct color palette
  # scale_color_viridis_d(option = "D") + # Alternative palette
  labs(
    title = "Median MCC Performance Across DGPs",
    subtitle = "Each point represents the median MCC over 100 simulations",
    x = "Data Generation Process (DGP)",
    y = "Median MCC",
    color = "Methodology" # Legend title
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8), # Rotate x-axis labels
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  guides(color = guide_legend(nrow = 1)) # Arrange legend in one row

print(plot_median_mcc)


plot_boxplot_median_mcc <- ggplot(median_mcc_data,
                                  # X is Method, Y is the Median_MCC calculated per DGP
                                  aes(x = Method, y = Median_MCC, fill = Method)) +
  # One boxplot per method, summarizing the 19 median values
  geom_boxplot(outlier.size = 1, notch = FALSE) +
  # Optional: Add individual points (medians from each DGP) with jitter
  geom_jitter(width = 0.1, height = 0, size = 1, alpha = 0.6, color = "grey20") +
  scale_fill_brewer(palette = "Set2") + # Or Paired, Dark2, etc.
  #scale_fill_viridis_d(option="F",guide = "none") +
  labs(
    title = "Distribution of Median MCC Performance Across DGPs",
    subtitle = "Each boxplot summarizes the 19 median MCC values (one per DGP) for a method",
    x = "Methodology",
    y = "Median MCC (calculated per DGP)",
    fill = "Methodology" # Fill aesthetic matches x-axis
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9), # Rotate if names overlap
    legend.position = "none", # Legend is redundant with x-axis and fill
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9)
  )

print(plot_boxplot_median_mcc)

selected_dgps <- c("DGP1", "DGP2", "DGP3", "DGP4", "DGP9")
mcc_subset_data <- results_long %>%
  filter(
    Metric == "MCC",
    DGP %in% selected_dgps
  ) %>%
  mutate(
    Value = ifelse(Metric == "F1" & !is.finite(Value), 0, Value),
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
  scale_fill_brewer(palette = "Set2") + # Or "Set2", "Accent", "Paired"
  #scale_fill_viridis_d(guide = "none") +
  # scale_fill_viridis_d(option="C") + # Viridis alternative
  labs(
    title = "MCC Performance Comparison for Selected DGPs",
    subtitle = paste("Showing DGPs:", paste(selected_dgps, collapse=", ")),
    x = "Methodology",
    y = "MCC Value",
    fill = "DGP" # Legend title for DGP colors
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9), # Rotate method names if needed
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  guides(fill = guide_legend(nrow = 1)) # Arrange legend

print(plot_mcc_specific_dgps)
