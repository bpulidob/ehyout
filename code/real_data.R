source("~/ehyout/code/indices.R")
source("~/ehyout/code/method_fun.R")

library(fdaoutlier)
library(tidyverse)

ehyout <- function(data){
  if(typeof(data) == "list"){
    data <- as.matrix(data)
  }
  data_ind <- indAB(data)
  data_ind_com <- get_outliers_multivariate(data_ind, method = "comedian")
  return(data_ind_com$outliers)
}

## WORLD POPULATION

data(world_population)

set.seed(1221)
world_population_ehyout <- ehyout(world_population)
world_population_subset <- world_population[world_population_ehyout,]
row.names(world_population_subset)
colnames(world_population) <- 1950:2010


df_long_wp <- as.data.frame(world_population) %>%
  mutate(RowID = row_number()) %>%
  pivot_longer(cols = -RowID, names_to = "Variable", values_to = "Value") %>%
  mutate(RowID = factor(RowID),
         Variable = as.integer(Variable))

df_long_wp <- df_long_wp %>%
  mutate(Highlight = ifelse(as.numeric(RowID) %in% world_population_ehyout,
                            "Highlight", "Normal"))

plt_wp <- ggplot(df_long_wp, aes(x = Variable, y = Value, group = RowID, color = Highlight)) +
  geom_line(aes(linetype = Highlight)) +
  theme_minimal() +
  xlab("Time (year)") +
  ylab("Population (millions)")+
  scale_color_manual(values = c("Highlight" = "red", "Normal" = "grey")) +
  scale_linetype_manual(values = c("Highlight" = "dashed", "Normal" = "solid")) +
  theme(legend.position = "none")

ggsave("results/plots/rd_population.pdf", plt_wp, width = 10, height = 6, units = "in")


## SPANISH WEATHER

data("spanish_weather")

sw_temp <- spanish_weather$temperature
sw_temp_ehyout <- ehyout(sw_temp)
sw_temp_subset <- sw_temp[sw_temp_ehyout,]
temp_outliers <- row.names(sw_temp_subset)
sw_temp_tf <- tfd(data = sw_temp, evaluator = tf_approx_spline) |>
  as_tibble() |> t() |> as.data.frame()
colnames(sw_temp_tf) <- 1:365

df_long_swt <- sw_temp_tf %>%
  mutate(RowID = row_number()) %>%
  pivot_longer(cols = -RowID, names_to = "Variable", values_to = "Value") %>%
  mutate(RowID = factor(RowID),
         Variable = as.integer(Variable)
  )

df_long_swt <- df_long_swt %>%
  mutate(Highlight = ifelse(as.numeric(RowID) %in% sw_temp_ehyout,
                            "Highlight", "Normal"))

plt_swt <- ggplot(df_long_swt, aes(x = Variable, y = Value, group = RowID, color = Highlight)) +
  geom_line(aes(linetype = Highlight)) +
  theme_minimal(base_size = 35) +
  scale_color_manual(values = c("Highlight" = "red", "Normal" = "grey")) +
  scale_linetype_manual(values = c("Highlight" = "solid", "Normal" = "solid")) +
  theme(legend.position = "none") +
  xlab("Time (day)") +
  ylab(" ")+
  ggtitle("Temperature")

sw_prec <- spanish_weather$log_precipitation
sw_prec_ehyout <- ehyout(sw_prec)
sw_prec_subset <- sw_prec[sw_prec_ehyout,]
prec_outliers <- row.names(sw_prec_subset)

sw_prec_tf <- tfd(data = sw_prec, evaluator = tf_approx_spline) |>
  as_tibble() |> t() |> as.data.frame()
colnames(sw_prec_tf) <- 1:365

df_long_swt2 <- sw_prec_tf %>%
  mutate(RowID = row_number()) %>%
  pivot_longer(cols = -RowID, names_to = "Variable", values_to = "Value") %>%
  mutate(RowID = factor(RowID),
         Variable = as.integer(Variable)
  )

df_long_swt2 <- df_long_swt2 %>%
  mutate(Highlight = ifelse(as.numeric(RowID) %in% sw_prec_ehyout,
                            "Highlight", "Normal"))

plt_swt2 <- ggplot(df_long_swt2, aes(x = Variable, y = Value, group = RowID, color = Highlight)) +
  geom_line(aes(linetype = Highlight)) +
  theme_minimal(base_size = 35) +
  scale_color_manual(values = c("Highlight" = "red", "Normal" = "grey")) +
  scale_linetype_manual(values = c("Highlight" = "solid", "Normal" = "solid")) +
  theme(legend.position = "none") +
  xlab("Time (day)") +
  ylab(" ")+
  ggtitle("Log Precipitation")

ggsave("results/plots/rd_temp.pdf", plt_swt, width = 10, height = 7, units = "in")
ggsave("results/plots/rd_prec.pdf", plt_swt2, width = 10, height = 7, units = "in")

# sm_tf <- tfb(sw_prec,  k = 20); sm_tf <- tibble::tibble(sm_tf)
# sm_tf <- dplyr::mutate(sm_tf,
#                        out = 1:n() %in% sw_prec_ehyout
# )
# 
# lines <- ggplot(sm_tf) +
#   # x axis btween 0 and 1?
#   geom_spaghetti(aes(y = sm_tf, col = factor(out))) +
#   scale_color_manual(values=c("#999999", "#FF0000")) +
#   xlab("") +
#   ylab(" ")+
#   ggtitle("Log Precipitation")+
#   theme_classic()+
#   theme(legend.position = "none")
# 
# ggsave("results/real_data/prec.pdf", lines, width = 10, height = 6, units = "in")
# 
# 
# sm_tf_temp <- tfb(sw_temp,  k = 20); sm_tf_temp <- tibble::tibble(sm_tf_temp)
# sm_tf_temp <- dplyr::mutate(sm_tf_temp,
#                             out = 1:n() %in% sw_temp_ehyout
# )
# 
# lines_temp <- ggplot(sm_tf_temp) +
#   # x axis btween 0 and 1?
#   geom_spaghetti(aes(y = sm_tf_temp, col = factor(out))) +
#   scale_color_manual(values=c("#999999", "#FF0000")) +
#   xlab("Time (day)") +
#   ylab(" ")+
#   ggtitle("Temperature")+
#   theme_classic()+
#   theme(legend.position = "none")
# 
# 
# ggsave("results/real_data/temp.pdf", lines_temp, width = 10, height = 6, units = "in")
# 




#PLOTS

temp_outliers_s <- gsub("1980-2009", "", temp_outliers)
prec_outliers_s <- gsub("1980-2009", "", prec_outliers)

station_names <- spanish_weather$station_info$name
colors <- vector("character", length(station_names))

# Assign colors based on the conditions
for (i in seq_along(station_names)) {
  if (station_names[i] %in% prec_outliers_s &&
      station_names[i] %in% temp_outliers_s) {
    colors[i] <- "purple"  # Color if in both b and c
  } else if (station_names[i] %in% prec_outliers_s) {
    colors[i] <- "blue"    # Color if only in b
  } else if (station_names[i] %in% temp_outliers_s) {
    colors[i] <- "green"   # Color if only in c
  } else {
    colors[i] <- "gray"    # Color if in neither b nor c
  }
}



# library(maps)
# # library(mclust)
# 
# place <- spanish_weather$station_info$name
# coordinates <- data.frame(spanish_weather$station_info$longitude,
#                           spanish_weather$station_info$latitude)
# rownames(coordinates)<-place
# 
# non_gray_indices <- which(colors != "gray")
# filtered_coordinates <- coordinates[non_gray_indices, ]
# filtered_colors <- colors[non_gray_indices]
# 
# pdf(file = "spanish_map_outliers.pdf", width = 8, height = 6)  # Adjust width and height as needed
# 
# maps::map('world',ylim=c(24,45),xlim=c(-20,7))
# #points(coordinates,col=colors,pch=16, cex=2)
# points(filtered_coordinates,col=filtered_colors,pch=16, cex=2)
# 
# dev.off()




# # Create data frame with coordinates and colors
# coordinates <- data.frame(
#   longitude = spanish_weather$station_info$longitude,
#   latitude = spanish_weather$station_info$latitude,
#   station = station_names,
#   color = colors
# )
# 
# # Filter out gray points (non-outliers)
# filtered_coordinates <- coordinates %>% filter(color != "gray")
# 
# # Get map data for Spain
# world_map <- map_data("world")
# spain_map <- subset(world_map, region == "Spain")
# # Adjust map data to include the Canary Islands
# canary_islands_map <- subset(world_map, subregion %in% c("Canary Islands"))
# 
# # Combine mainland Spain and Canary Islands maps
# combined_map <- rbind(spain_map, canary_islands_map)
# 
# # Plot the map with ggplot
# ggplot() +
#   geom_polygon(data = spain_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "gray") +
#   geom_point(data = filtered_coordinates, aes(x = longitude, y = latitude, color = color), size = 2) +
#   scale_color_manual(values = c("blue" = "blue", "green" = "green", "purple" = "purple")) +
#   theme_minimal() +
#   labs(title = "Spanish Weather Stations and Outliers",
#        color = "Outlier Type") +
#   theme(legend.title = element_text(size = 10),
#         legend.text = element_text(size = 7),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank())



library(leaflet)
# Create leaflet map
m <- leaflet() %>% 
  setView(lat  = 40.416775, lng = -3.703790, zoom = 5) %>%
  addTiles()

# Add colored markers to the map
m <- m %>% 
  addCircleMarkers(data = coordinates, 
                   lng = ~longitude, 
                   lat = ~latitude, 
                   color = ~color, 
                   radius = 5, 
                   fillOpacity = 0.8, 
                   stroke = FALSE,
                   label = ~station)

# Print the map
m



