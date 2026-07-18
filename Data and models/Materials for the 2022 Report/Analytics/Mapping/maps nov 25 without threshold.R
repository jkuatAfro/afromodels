








# Nurses & Midwives density maps with discrete levels
# Load packages
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

# --- Load and prepare data ---
nursmid25 <- read.table("G:/Brazzaville/HRH 24/Analytics/Mapping/nursmid25.txt",
                        header = TRUE, sep = '\t')
world <- ne_countries(scale = "medium", returnclass = "sf")

african_iso3 <- c("DZA","AGO","BEN","BWA","BFA","BDI","CPV","CMR","CAF","TCD","COM",
                  "COD","COG","CIV","DJI","EGY","GNQ","ERI","ETH","GAB","GMB","GHA",
                  "GIN","GNB","KEN","LSO","LBR","LBY","MDG","MWI","MLI","MRT","MUS",
                  "MAR","MOZ","NAM","NER","NGA","RWA","STP","SEN","SYC","SLE","SOM",
                  "ZAF","SSD","SDN","SWZ","TZA","TGO","TUN","UGA","ZMB","ZWE")

africa_sf <- world %>% filter(iso_a3 %in% african_iso3)
africa_nm <- africa_sf %>% left_join(nursmid25, by = "iso_a3")

# Identify island nations
island_nations <- c("CPV","COM","MUS","STP","SYC")

# --- Define discrete breaks ---
breaks <- c(0, 5, 10, 13, 18, 23, 28, 33, Inf)
break_labels <- c("0-5", "5-10", "10-13", "13-18", "18-23", "23-28", "28-33", "33+")

# --- Define color palette (red to green) ---
palette <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee08b", "#a6d96a", "#66bd63", "#1a9850")

# --- Function to build a map for a given year ---
make_map <- function(year, title) {
  
  # Create discrete variable
  africa_nm_discrete <- africa_nm %>%
    mutate(
      value_cat = cut(.data[[paste0("NM", year)]], 
                     breaks = breaks, 
                     labels = break_labels,
                     include.lowest = TRUE, 
                     right = FALSE),
      # Ensure NA values are properly handled
      value_cat = as.character(value_cat),
      value_cat = ifelse(is.na(value_cat), "NOT in AFRO", value_cat)
    )
  
  # Convert to factor with correct order
  all_levels <- c(break_labels, "NOT in AFRO")
  africa_nm_discrete$value_cat <- factor(africa_nm_discrete$value_cat, levels = all_levels)
  
  ggplot() +
    # Main African countries
    geom_sf(data = africa_nm_discrete,
            aes(fill = value_cat),
            color = "white", size = 0.15) +
    
    # Island nations with black border
    geom_sf(data = africa_nm_discrete %>% filter(iso_a3 %in% island_nations),
            aes(fill = value_cat),
            color = "black", size = 0.6) +
    
    # Non-island labels
    geom_sf_text(data = africa_nm_discrete %>% filter(!iso_a3 %in% island_nations),
                 aes(label = iso_a3),
                 size = 2.5, color = "black") +
    
    # Island labels with values
    geom_sf_text(data = africa_nm_discrete %>% filter(iso_a3 %in% island_nations),
                 aes(label = paste0(iso_a3, "\n", round(.data[[paste0("NM", year)]], 1))),
                 size = 3.5, color = "black", fontface = "bold") +
    
    # Discrete color scale
    scale_fill_manual(
      values = c(setNames(palette, break_labels), "NOT in AFRO" = "grey60"),
      name = "Nurses & Midwives\nper 10,000 population",
      drop = FALSE,
      na.value = "grey60"
    ) +
    
    labs(title = title) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.key.size = unit(0.8, "cm")
    )
}

# --- Build maps ---
p2018 <- make_map(2018, "2018")
p2022 <- make_map(2022, "2022") 
p2024 <- make_map(2024, "2024")

# --- Combine maps side by side ---
final_plot <- p2018 + p2022 + p2024 + plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")

print(final_plot)




###############

##seperating the maps




# Nurses & Midwives density maps with discrete levels
# Load packages
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

# --- Load and prepare data ---
nursmid25 <- read.table("G:/Brazzaville/HRH 24/Analytics/Mapping/nursmid25.txt",
                        header = TRUE, sep = '\t')
world <- ne_countries(scale = "medium", returnclass = "sf")

african_iso3 <- c("DZA","AGO","BEN","BWA","BFA","BDI","CPV","CMR","CAF","TCD","COM",
                  "COD","COG","CIV","DJI","EGY","GNQ","ERI","ETH","GAB","GMB","GHA",
                  "GIN","GNB","KEN","LSO","LBR","LBY","MDG","MWI","MLI","MRT","MUS",
                  "MAR","MOZ","NAM","NER","NGA","RWA","STP","SEN","SYC","SLE","SOM",
                  "ZAF","SSD","SDN","SWZ","TZA","TGO","TUN","UGA","ZMB","ZWE")

africa_sf <- world %>% filter(iso_a3 %in% african_iso3)
africa_nm <- africa_sf %>% left_join(nursmid25, by = "iso_a3")

# Identify island nations
island_nations <- c("CPV","COM","MUS","STP","SYC")

# --- Define discrete breaks ---
breaks <- c(0, 5, 10, 13, 18, 23, 28, 33, Inf)
break_labels <- c("0-5", "5-10", "10-13", "13-18", "18-23", "23-28", "28-33", "33+")

# --- Define color palette (red to green) ---
palette <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee08b", "#a6d96a", "#66bd63", "#1a9850")

# --- Function to build a map for a given year ---
make_map <- function(year, title) {
  
  # Create discrete variable
  africa_nm_discrete <- africa_nm %>%
    mutate(
      value_cat = cut(.data[[paste0("NM", year)]], 
                     breaks = breaks, 
                     labels = break_labels,
                     include.lowest = TRUE, 
                     right = FALSE),
      # Ensure NA values are properly handled
      value_cat = as.character(value_cat),
      value_cat = ifelse(is.na(value_cat), "NOT in AFRO", value_cat)
    )
  
  # Convert to factor with correct order
  all_levels <- c(break_labels, "NOT in AFRO")
  africa_nm_discrete$value_cat <- factor(africa_nm_discrete$value_cat, levels = all_levels)
  
  ggplot() +
    # Main African countries
    geom_sf(data = africa_nm_discrete,
            aes(fill = value_cat),
            color = "white", size = 0.15) +
    
    # Island nations with black border
    geom_sf(data = africa_nm_discrete %>% filter(iso_a3 %in% island_nations),
            aes(fill = value_cat),
            color = "black", size = 0.5) +
    
    # Non-island labels
    geom_sf_text(data = africa_nm_discrete %>% filter(!iso_a3 %in% island_nations),
                 aes(label = iso_a3),
                 size = 2.5, color = "black") +
    
    # Island labels with values
    geom_sf_text(data = africa_nm_discrete %>% filter(iso_a3 %in% island_nations),
                 aes(label = paste0(iso_a3, "\n", round(.data[[paste0("NM", year)]], 1))),
                 size = 3.5, color = "black", fontface = "bold") +
    
    # Discrete color scale
    scale_fill_manual(
      values = c(setNames(palette, break_labels), "NOT in AFRO" = "grey60"),
      name = "Nurses & Midwives\nper 10,000 population",
      drop = FALSE,
      na.value = "grey60"
    ) +
    
    labs(title = title) +
    theme_void() +
    theme(
      legend.position = "left",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      legend.key.size = unit(0.4, "cm")
    )
}

# --- Build maps ---
p2018 <- make_map(2018, "2018")
p2022 <- make_map(2022, "2022") 
p2024 <- make_map(2024, "2024")

# --- Combine maps side by side ---
final_plot <- p2018 + p2022 + p2024 + plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "left")

print(final_plot)



# --- Combine maps side by side ---
final_plot11 <- p2018  + plot_layout(ncol = 1, theme(legend.position = "bottom")

print(final_plot1)



















# Nurses & Midwives density maps with discrete levels and island circles
# Load packages
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

# --- Load and prepare data ---
nursmid25 <- read.table("G:/Brazzaville/HRH 24/Analytics/Mapping/nursmid25.txt",
                        header = TRUE, sep = '\t')
world <- ne_countries(scale = "medium", returnclass = "sf")

african_iso3 <- c("DZA","AGO","BEN","BWA","BFA","BDI","CPV","CMR","CAF","TCD","COM",
                  "COD","COG","CIV","DJI","EGY","GNQ","ERI","ETH","GAB","GMB","GHA",
                  "GIN","GNB","KEN","LSO","LBR","LBY","MDG","MWI","MLI","MRT","MUS",
                  "MAR","MOZ","NAM","NER","NGA","RWA","STP","SEN","SYC","SLE","SOM",
                  "ZAF","SSD","SDN","SWZ","TZA","TGO","TUN","UGA","ZMB","ZWE")

africa_sf <- world %>% filter(iso_a3 %in% african_iso3)
africa_nm <- africa_sf %>% left_join(nursmid25, by = "iso_a3")

# Identify island nations
island_nations <- c("CPV","COM","MUS","STP","SYC")

# Get centroids for island nations for the circles
island_centroids <- africa_nm %>%
  filter(iso_a3 %in% island_nations) %>%
  st_centroid()

# --- Define discrete breaks ---
breaks <- c(0, 5, 10, 13, 18, 23, 28, 33, Inf)
break_labels <- c("0-5", "5-10", "10-13", "13-18", "18-23", "23-28", "28-33", "33+")

# --- Define color palette (red to green) ---
palette <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee08b", "#a6d96a", "#66bd63", "#1a9850")

# --- Function to build a map for a given year ---
make_map <- function(year, title) {
  
  # Create discrete variable for main countries
  africa_nm_discrete <- africa_nm %>%
    mutate(
      value_cat = cut(.data[[paste0("NM", year)]], 
                     breaks = breaks, 
                     labels = break_labels,
                     include.lowest = TRUE, 
                     right = FALSE),
      value_cat = as.character(value_cat),
      value_cat = ifelse(is.na(value_cat), "NOT in AFRO", value_cat)
    )
  
  # Create discrete variable for island centroids
  island_data <- island_centroids %>%
    mutate(
      value_cat = cut(.data[[paste0("NM", year)]], 
                     breaks = breaks, 
                     labels = break_labels,
                     include.lowest = TRUE, 
                     right = FALSE),
      value_cat = as.character(value_cat),
      value_cat = ifelse(is.na(value_cat), "NOT in AFRO", value_cat),
      # Get the actual value for labeling
      actual_value = round(.data[[paste0("NM", year)]], 1)
    )
  
  # Convert to factor with correct order
  all_levels <- c(break_labels, "NOT in AFRO")
  africa_nm_discrete$value_cat <- factor(africa_nm_discrete$value_cat, levels = all_levels)
  island_data$value_cat <- factor(island_data$value_cat, levels = all_levels)
  
  ggplot() +
    # Main African countries (excluding islands)
    geom_sf(data = africa_nm_discrete %>% filter(!iso_a3 %in% island_nations),
            aes(fill = value_cat),
            color = "white", size = 0.15) +
    
    # Island circles (filled with performance color)
    geom_sf(data = island_data,
            aes(fill = value_cat),
            color = "black", size = 0.8, shape = 21, stroke = 1.2) +
    
    # Non-island labels
    geom_sf_text(data = africa_nm_discrete %>% filter(!iso_a3 %in% island_nations),
                 aes(label = iso_a3),
                 size = 2.5, color = "black") +
    
    # Island labels with values (positioned above circles)
    geom_sf_text(data = island_data,
                 aes(label = paste0(iso_a3, "\n", actual_value)),
                 size = 3.0, color = "black", fontface = "bold",
                 nudge_y = 1.5) +  # Adjust this value as needed
    
    # Discrete color scale
    scale_fill_manual(
      values = c(setNames(palette, break_labels), "NOT in AFRO" = "grey60"),
      name = "Nurses & Midwives\nper 10,000 population",
      drop = FALSE,
      na.value = "grey60"
    ) +
    
    labs(title = title) +
    theme_void() +
    theme(
      legend.position = "none",  # Remove individual legends
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}

# --- Build maps ---
p2018 <- make_map(2018, "2018")
p2022 <- make_map(2022, "2022") 
p2024 <- make_map(2024, "2024")

# --- Create a separate legend plot ---
create_legend_plot <- function() {
  # Create dummy data for legend
  dummy_data <- data.frame(
    category = factor(c(break_labels, "NOT in AFRO"), 
                     levels = c(break_labels, "NOT in AFRO")),
    value = 1:9
  )
  
  ggplot(dummy_data, aes(x = category, y = value, fill = category)) +
    geom_col() +
    scale_fill_manual(
      values = c(setNames(palette, break_labels), "NOT in AFRO" = "grey60"),
      name = "Nurses & Midwives\nper 10,000 population",
      drop = FALSE
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold", hjust = 0.5),
      legend.text = element_text(size = 10),
      legend.key.size = unit(0.8, "cm"),
      legend.box.margin = margin(0, 0, 0, 0)
    )
}

# Create the legend
legend_plot <- create_legend_plot()
legend <- cowplot::get_legend(legend_plot)

# --- Combine maps with single legend ---
final_plot <- (p2018 + p2022 + p2024) / 
  plot_spacer() / 
  as_ggplot(legend) + 
  plot_layout(ncol = 1, heights = c(8, 0.2, 1))

print(final_plot)



\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\






# Combined Health Workforce density maps with discrete levels

# Load packages
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(cowplot)

# --- Load and prepare data ---
all25 <- read.table("G:/Brazzaville/HRH 24/Analytics/Mapping/all25.txt",
                    header = TRUE, sep = '\t')
world <- ne_countries(scale = "medium", returnclass = "sf")

african_iso3 <- c("DZA","AGO","BEN","BWA","BFA","BDI","CPV","CMR","CAF","TCD","COM",
                  "COD","COG","CIV","DJI","EGY","GNQ","ERI","ETH","GAB","GMB","GHA",
                  "GIN","GNB","KEN","LSO","LBR","LBY","MDG","MWI","MLI","MRT","MUS",
                  "MAR","MOZ","NAM","NER","NGA","RWA","STP","SEN","SYC","SLE","SOM",
                  "ZAF","SSD","SDN","SWZ","TZA","TGO","TUN","UGA","ZMB","ZWE")

africa_sf <- world %>% filter(iso_a3 %in% african_iso3)
africa_all <- africa_sf %>% left_join(all25, by = "iso_a3")

# Identify island nations
island_nations <- c("CPV","COM","MUS","STP","SYC")

# Get centroids for island nations for the circles
island_centroids <- africa_all %>%
  filter(iso_a3 %in% island_nations) %>%
  st_centroid()

# --- Define discrete breaks ---
breaks <- c(0, 10, 25, 40, 55, 70, 85, Inf)
break_labels <- c("0-10", "10-25", "25-40", "40-55", "55-70", "70-85", "85+")

# --- Define color palette (red to green) ---
palette <- c("#a50026", "#d73027", "#f46d43", "#fdae61",
             "#fee08b", "#a6d96a", "#66bd63", "#1a9850")

# --- Function to build a map for a given year ---
make_map <- function(year, title) {
  
  col_name <- paste0("all.", year)
  
  africa_all_discrete <- africa_all %>%
    mutate(
      value_cat = cut(.data[[col_name]], 
                     breaks = breaks, 
                     labels = break_labels,
                     include.lowest = TRUE, 
                     right = FALSE),
      value_cat = as.character(value_cat),
      value_cat = ifelse(is.na(value_cat), "Not in AFRO", value_cat)
    )
  
  island_data <- island_centroids %>%
    mutate(
      value_cat = cut(.data[[col_name]], 
                     breaks = breaks, 
                     labels = break_labels,
                     include.lowest = TRUE, 
                     right = FALSE),
      value_cat = as.character(value_cat),
      value_cat = ifelse(is.na(value_cat), "Not in AFRO", value_cat),
      actual_value = round(.data[[col_name]], 1)
    )
  
  all_levels <- c(break_labels, "Not in AFRO")
  africa_all_discrete$value_cat <- factor(africa_all_discrete$value_cat, levels = all_levels)
  island_data$value_cat <- factor(island_data$value_cat, levels = all_levels)
  
  ggplot() +
    geom_sf(data = africa_all_discrete %>% filter(!iso_a3 %in% island_nations),
            aes(fill = value_cat),
            color = "white", size = 0.15) +
    geom_sf(data = island_data,
            aes(fill = value_cat),
            color = "black", size = 3, shape = 21, stroke = 1.2) +
    geom_sf_text(data = africa_all_discrete %>% filter(!iso_a3 %in% island_nations),
                 aes(label = iso_a3),
                 size = 2.5, color = "black") +
    geom_sf_text(data = island_data,
                 aes(label = paste0(iso_a3, "\n", actual_value)),
                 size = 2.8, color = "black", fontface = "bold",
                 nudge_y = 2.5, vjust = 0) +
    scale_fill_manual(
      values = c(setNames(palette, break_labels), "Not in AFRO" = "grey60"),
      name = "Combined Health Workforce\nper 10,000 population",
      drop = FALSE,
      na.value = "grey60"
    ) +
    labs(title = title) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}

# --- Build maps ---
p2018 <- make_map(2018, "2018")
p2022 <- make_map(2022, "2022") 
p2024 <- make_map(2024, "2024")

# --- Create a separate legend plot ---
create_legend_plot <- function() {
  dummy_data <- data.frame(
    category = factor(c(break_labels, "Not in AFRO"), 
                     levels = c(break_labels, "Not in AFRO")),
    value = 1:8
  )
  
  ggplot(dummy_data, aes(x = category, y = value, fill = category)) +
    geom_col() +
    scale_fill_manual(
      values = c(setNames(palette, break_labels), "Not in AFRO" = "grey60"),
      name = "Combined Health Workforce\nper 10,000 population",
      drop = FALSE
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold", hjust = 0.5),
      legend.text = element_text(size = 10),
      legend.key.size = unit(0.8, "cm"),
      legend.box.margin = margin(0, 0, 0, 0)
    )
}

legend_plot <- create_legend_plot()
legend <- cowplot::get_legend(legend_plot)

# --- Combine maps with single legend ---
final_plot <- (p2018 + p2022 + p2024) /
  plot_spacer() /
  wrap_elements(legend) +
  plot_layout(ncol = 1, heights = c(8, 0.2, 1))

print(final_plot)






\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



# Doctors, Nurses, Midwives density maps with discrete classes
# Classes: 0-7, 7-15, 15-25, 25-38, 38-45, 45+

# Load packages
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(cowplot)

# --- Load and prepare data ---
donumid25 <- read.table("G:/Brazzaville/HRH 24/Analytics/Mapping/donumid25.txt",
                        header = TRUE, sep = '\t')
world <- ne_countries(scale = "medium", returnclass = "sf")

african_iso3 <- c("DZA","AGO","BEN","BWA","BFA","BDI","CPV","CMR","CAF","TCD","COM",
                  "COD","COG","CIV","DJI","EGY","GNQ","ERI","ETH","GAB","GMB","GHA",
                  "GIN","GNB","KEN","LSO","LBR","LBY","MDG","MWI","MLI","MRT","MUS",
                  "MAR","MOZ","NAM","NER","NGA","RWA","STP","SEN","SYC","SLE","SOM",
                  "ZAF","SSD","SDN","SWZ","TZA","TGO","TUN","UGA","ZMB","ZWE")

africa_sf <- world %>% filter(iso_a3 %in% african_iso3)
africa_dnm <- africa_sf %>% left_join(donumid25, by = "iso_a3")

# Identify island nations
island_nations <- c("CPV","COM","MUS","STP","SYC")

# Get centroids for island nations for the circles
island_centroids <- africa_dnm %>%
  filter(iso_a3 %in% island_nations) %>%
  st_centroid()

# --- Define discrete breaks ---
breaks <- c(0, 7, 15, 25, 38, 45, Inf)
break_labels <- c("0-7", "7-15", "15-25", "25-38", "38-45", "45+")

# --- Define color palette (6 classes, red → green) ---
palette <- c("#a50026", "#d73027", "#fdae61", "#fee08b", "#66bd63", "#1a9850")

# --- Function to build a map for a given year ---
make_map <- function(year, title) {
  
  col_name <- paste0("DNM", year)
  
  africa_dnm_discrete <- africa_dnm %>%
    mutate(
      value_cat = cut(.data[[col_name]], 
                     breaks = breaks, 
                     labels = break_labels,
                     include.lowest = TRUE, 
                     right = FALSE),
      value_cat = as.character(value_cat),
      value_cat = ifelse(is.na(value_cat), "Not in AFRO", value_cat),
      actual_value = round(.data[[col_name]], 1)
    )
  
  island_data <- island_centroids %>%
    mutate(
      value_cat = cut(.data[[col_name]], 
                     breaks = breaks, 
                     labels = break_labels,
                     include.lowest = TRUE, 
                     right = FALSE),
      value_cat = as.character(value_cat),
      value_cat = ifelse(is.na(value_cat), "Not in AFRO", value_cat),
      actual_value = round(.data[[col_name]], 1)
    )
  
  all_levels <- c(break_labels, "Not in AFRO")
  africa_dnm_discrete$value_cat <- factor(africa_dnm_discrete$value_cat, levels = all_levels)
  island_data$value_cat <- factor(island_data$value_cat, levels = all_levels)
  
  ggplot() +
    # Mainland countries
    geom_sf(data = africa_dnm_discrete %>% filter(!iso_a3 %in% island_nations),
            aes(fill = value_cat),
            color = "white", size = 0.15) +
    # Island circles
    geom_sf(data = island_data,
            aes(fill = value_cat),
            color = "black", size = 3, shape = 21, stroke = 1.2) +
    # Mainland labels
    geom_sf_text(data = africa_dnm_discrete %>% filter(!iso_a3 %in% island_nations),
                 aes(label = iso_a3),
                 size = 2.5, color = "black") +
    # Island labels above circles
    geom_sf_text(data = island_data,
                 aes(label = paste0(iso_a3, "\n", actual_value)),
                 size = 2.8, color = "black", fontface = "bold",
                 nudge_y = 2.5, vjust = 0) +
    scale_fill_manual(
      values = c(setNames(palette, break_labels), "Not in AFRO" = "grey60"),
      name = "Doctors, Nurses & Midwives\nper 10,000 population",
      drop = FALSE,
      na.value = "grey60"
    ) +
    labs(title = title) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}

# --- Build maps ---
p2018 <- make_map(2018, "2018")
p2022 <- make_map(2022, "2022") 
p2024 <- make_map(2024, "2024")

# --- Create a separate legend plot ---
create_legend_plot <- function() {
  dummy_data <- data.frame(
    category = factor(c(break_labels, "Not in AFRO"), 
                     levels = c(break_labels, "Not in AFRO")),
    value = 1:7
  )
  
  ggplot(dummy_data, aes(x = category, y = value, fill = category)) +
    geom_col() +
    scale_fill_manual(
      values = c(setNames(palette, break_labels), "Not in AFRO" = "grey60"),
      name = "Doctors, Nurses & Midwives\nper 10,000 population",
      drop = FALSE
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold", hjust = 0.5),
      legend.text = element_text(size = 10),
      legend.key.size = unit(0.8, "cm"),
      legend.box.margin = margin(0, 0, 0, 0)
    )
}

legend_plot <- create_legend_plot()
legend <- cowplot::get_legend(legend_plot)

# --- Combine maps with single legend ---
final_plot <- (p2018 + p2022 + p2024) /
  plot_spacer() /
  wrap_elements(legend) +
  plot_layout(ncol = 1, heights = c(8, 0.2, 1))

print(final_plot)




\\\\\\\\\


# Doctors, Nurses, Midwives, Pharmacists, Dentists density maps with discrete classes
# Classes: 0-7, 7-15, 15-25, 25-38, 38-45, 45+

# Load packages
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(cowplot)

# --- Load and prepare data ---
donumidpde25 <- read.table("G:/Brazzaville/HRH 24/Analytics/Mapping/donumidpde25.txt",
                           header = TRUE, sep = '\t')
world <- ne_countries(scale = "medium", returnclass = "sf")

african_iso3 <- c("DZA","AGO","BEN","BWA","BFA","BDI","CPV","CMR","CAF","TCD","COM",
                  "COD","COG","CIV","DJI","EGY","GNQ","ERI","ETH","GAB","GMB","GHA",
                  "GIN","GNB","KEN","LSO","LBR","LBY","MDG","MWI","MLI","MRT","MUS",
                  "MAR","MOZ","NAM","NER","NGA","RWA","STP","SEN","SYC","SLE","SOM",
                  "ZAF","SSD","SDN","SWZ","TZA","TGO","TUN","UGA","ZMB","ZWE")

africa_sf <- world %>% filter(iso_a3 %in% african_iso3)
africa_dnmpd <- africa_sf %>% left_join(donumidpde25, by = "iso_a3")

# Identify island nations
island_nations <- c("CPV","COM","MUS","STP","SYC")

# Get centroids for island nations for the circles
island_centroids <- africa_dnmpd %>%
  filter(iso_a3 %in% island_nations) %>%
  st_centroid()

# --- Define discrete breaks ---
breaks <- c(0, 7, 15, 25, 38, 45, Inf)
break_labels <- c("0-7", "7-15", "15-25", "25-38", "38-45", "45+")

# --- Define color palette (6 classes, red → green) ---
palette <- c("#a50026", "#d73027", "#fdae61", "#fee08b", "#66bd63", "#1a9850")

# --- Function to build a map for a given year ---
make_map <- function(year, title) {
  
  col_name <- paste0("DNMPD", year)
  
  africa_dnmpd_discrete <- africa_dnmpd %>%
    mutate(
      value_cat = cut(.data[[col_name]], 
                     breaks = breaks, 
                     labels = break_labels,
                     include.lowest = TRUE, 
                     right = FALSE),
      value_cat = as.character(value_cat),
      value_cat = ifelse(is.na(value_cat), "Not in AFRO", value_cat),
      actual_value = round(.data[[col_name]], 1)
    )
  
  island_data <- island_centroids %>%
    mutate(
      value_cat = cut(.data[[col_name]], 
                     breaks = breaks, 
                     labels = break_labels,
                     include.lowest = TRUE, 
                     right = FALSE),
      value_cat = as.character(value_cat),
      value_cat = ifelse(is.na(value_cat), "Not in AFRO", value_cat),
      actual_value = round(.data[[col_name]], 1)
    )
  
  all_levels <- c(break_labels, "Not in AFRO")
  africa_dnmpd_discrete$value_cat <- factor(africa_dnmpd_discrete$value_cat, levels = all_levels)
  island_data$value_cat <- factor(island_data$value_cat, levels = all_levels)
  
  ggplot() +
    # Mainland countries
    geom_sf(data = africa_dnmpd_discrete %>% filter(!iso_a3 %in% island_nations),
            aes(fill = value_cat),
            color = "white", size = 0.15) +
    # Island circles
    geom_sf(data = island_data,
            aes(fill = value_cat),
            color = "black", size = 3, shape = 21, stroke = 1.2) +
    # Mainland labels
    geom_sf_text(data = africa_dnmpd_discrete %>% filter(!iso_a3 %in% island_nations),
                 aes(label = iso_a3),
                 size = 2.5, color = "black") +
    # Island labels above circles
    geom_sf_text(data = island_data,
                 aes(label = paste0(iso_a3, "\n", actual_value)),
                 size = 2.8, color = "black", fontface = "bold",
                 nudge_y = 2.5, vjust = 0) +
    scale_fill_manual(
      values = c(setNames(palette, break_labels), "Not in AFRO" = "grey60"),
      name = "Doctors, Nurses, Midwives,\nPharmacists & Dentists per 10,000",
      drop = FALSE,
      na.value = "grey60"
    ) +
    labs(title = title) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}

# --- Build maps ---
p2018 <- make_map(2018, "2018")
p2022 <- make_map(2022, "2022") 
p2024 <- make_map(2024, "2024")

# --- Create a separate legend plot ---
create_legend_plot <- function() {
  dummy_data <- data.frame(
    category = factor(c(break_labels, "Not in AFRO"), 
                     levels = c(break_labels, "Not in AFRO")),
    value = 1:7
  )
  
  ggplot(dummy_data, aes(x = category, y = value, fill = category)) +
    geom_col() +
    scale_fill_manual(
      values = c(setNames(palette, break_labels), "Not in AFRO" = "grey60"),
      name = "Doctors, Nurses, Midwives,\nPharmacists & Dentists per 10,000",
      drop = FALSE
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold", hjust = 0.5),
      legend.text = element_text(size = 10),
      legend.key.size = unit(0.8, "cm"),
      legend.box.margin = margin(0, 0, 0, 0)
    )
}

legend_plot <- create_legend_plot()
legend <- cowplot::get_legend(legend_plot)

# --- Combine maps with single legend ---
final_plot <- (p2018 + p2022 + p2024) /
  plot_spacer() /
  wrap_elements(legend) +
  plot_layout(ncol = 1, heights = c(8, 0.2, 1))

print(final_plot)



\\\\\\\\\\\\\\\\\

# Nurses & Midwives density maps with discrete classes
# Classes: 0-5, 5-10, 10-13, 13-18, 18-23, 23-28, 28+

# Load packages
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(cowplot)

# --- Load and prepare data ---
nursmid25 <- read.table("G:/Brazzaville/HRH 24/Analytics/Mapping/nursmid25.txt",
                        header = TRUE, sep = '\t')
world <- ne_countries(scale = "medium", returnclass = "sf")

african_iso3 <- c("DZA","AGO","BEN","BWA","BFA","BDI","CPV","CMR","CAF","TCD","COM",
                  "COD","COG","CIV","DJI","EGY","GNQ","ERI","ETH","GAB","GMB","GHA",
                  "GIN","GNB","KEN","LSO","LBR","LBY","MDG","MWI","MLI","MRT","MUS",
                  "MAR","MOZ","NAM","NER","NGA","RWA","STP","SEN","SYC","SLE","SOM",
                  "ZAF","SSD","SDN","SWZ","TZA","TGO","TUN","UGA","ZMB","ZWE")

africa_sf <- world %>% filter(iso_a3 %in% african_iso3)
africa_nm <- africa_sf %>% left_join(nursmid25, by = "iso_a3")

# Identify island nations
island_nations <- c("CPV","COM","MUS","STP","SYC")

# Get centroids for island nations for the circles
island_centroids <- africa_nm %>%
  filter(iso_a3 %in% island_nations) %>%
  st_centroid()

# --- Define discrete breaks ---
breaks <- c(0, 5, 10, 13, 18, 23, 28, Inf)
break_labels <- c("0-5", "5-10", "10-13", "13-18", "18-23", "23-28", "28+")

# --- Define color palette (7 classes, red → green) ---
palette <- c("#a50026", "#d73027", "#f46d43", "#fdae61",
             "#fee08b", "#66bd63", "#1a9850")

# --- Function to build a map for a given year ---
make_map <- function(year, title) {
  
  col_name <- paste0("NM", year)
  
  africa_nm_discrete <- africa_nm %>%
    mutate(
      value_cat = cut(.data[[col_name]], 
                     breaks = breaks, 
                     labels = break_labels,
                     include.lowest = TRUE, 
                     right = FALSE),
      value_cat = as.character(value_cat),
      value_cat = ifelse(is.na(value_cat), "Not in AFRO", value_cat),
      actual_value = round(.data[[col_name]], 1)
    )
  
  island_data <- island_centroids %>%
    mutate(
      value_cat = cut(.data[[col_name]], 
                     breaks = breaks, 
                     labels = break_labels,
                     include.lowest = TRUE, 
                     right = FALSE),
      value_cat = as.character(value_cat),
      value_cat = ifelse(is.na(value_cat), "Not in AFRO", value_cat),
      actual_value = round(.data[[col_name]], 1)
    )
  
  all_levels <- c(break_labels, "Not in AFRO")
  africa_nm_discrete$value_cat <- factor(africa_nm_discrete$value_cat, levels = all_levels)
  island_data$value_cat <- factor(island_data$value_cat, levels = all_levels)
  
  ggplot() +
    # Mainland countries
    geom_sf(data = africa_nm_discrete %>% filter(!iso_a3 %in% island_nations),
            aes(fill = value_cat),
            color = "white", size = 0.15) +
    # Island circles
    geom_sf(data = island_data,
            aes(fill = value_cat),
            color = "black", size = 3, shape = 21, stroke = 1.2) +
    # Mainland labels
    geom_sf_text(data = africa_nm_discrete %>% filter(!iso_a3 %in% island_nations),
                 aes(label = iso_a3),
                 size = 2.5, color = "black") +
    # Island labels above circles
    geom_sf_text(data = island_data,
                 aes(label = paste0(iso_a3, "\n", actual_value)),
                 size = 2.8, color = "black", fontface = "bold",
                 nudge_y = 2.5, vjust = 0) +
    scale_fill_manual(
      values = c(setNames(palette, break_labels), "Not in AFRO" = "grey60"),
      name = "Nurses & Midwives\nper 10,000 population",
      drop = FALSE,
      na.value = "grey60"
    ) +
    labs(title = title) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}

# --- Build maps ---
p2018 <- make_map(2018, "2018")
p2022 <- make_map(2022, "2022") 
p2024 <- make_map(2024, "2024")

# --- Create a separate legend plot ---
create_legend_plot <- function() {
  dummy_data <- data.frame(
    category = factor(c(break_labels, "Not in AFRO"), 
                     levels = c(break_labels, "Not in AFRO")),
    value = 1:8
  )
  
  ggplot(dummy_data, aes(x = category, y = value, fill = category)) +
    geom_col() +
    scale_fill_manual(
      values = c(setNames(palette, break_labels), "Not in AFRO" = "grey60"),
      name = "Nurses & Midwives\nper 10,000 population",
      drop = FALSE
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold", hjust = 0.5),
      legend.text = element_text(size = 10),
      legend.key.size = unit(0.8, "cm"),
      legend.box.margin = margin(0, 0, 0, 0)
    )
}

legend_plot <- create_legend_plot()
legend <- cowplot::get_legend(legend_plot)

# --- Combine maps with single legend ---
final_plot <- (p2018 + p2022 + p2024) /
  plot_spacer() /
  wrap_elements(legend) +
  plot_layout(ncol = 1, heights = c(8, 0.2, 1))

print(final_plot)





\\\\\\\\\\\\\\\



# Doctors density maps with discrete classes
# Classes: 0-1, 1-2.5, 2.5-4, 4-5.5, 5.5-8, 8-11, 11-13, 13+

# Load packages
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(cowplot)

# --- Load and prepare data ---
doc25 <- read.table("G:/Brazzaville/HRH 24/Analytics/Mapping/doc25.txt",
                    header = TRUE, sep = '\t')
world <- ne_countries(scale = "medium", returnclass = "sf")

african_iso3 <- c("DZA","AGO","BEN","BWA","BFA","BDI","CPV","CMR","CAF","TCD","COM",
                  "COD","COG","CIV","DJI","EGY","GNQ","ERI","ETH","GAB","GMB","GHA",
                  "GIN","GNB","KEN","LSO","LBR","LBY","MDG","MWI","MLI","MRT","MUS",
                  "MAR","MOZ","NAM","NER","NGA","RWA","STP","SEN","SYC","SLE","SOM",
                  "ZAF","SSD","SDN","SWZ","TZA","TGO","TUN","UGA","ZMB","ZWE")

africa_sf <- world %>% filter(iso_a3 %in% african_iso3)
africa_doc <- africa_sf %>% left_join(doc25, by = "iso_a3")

# Identify island nations
island_nations <- c("CPV","COM","MUS","STP","SYC")

# Get centroids for island nations for the circles
island_centroids <- africa_doc %>%
  filter(iso_a3 %in% island_nations) %>%
  st_centroid()

# --- Define discrete breaks ---
breaks <- c(0, 1, 2.5, 4, 5.5, 8, 11, 13, Inf)
break_labels <- c("0-1", "1-2.5", "2.5-4", "4-5.5", "5.5-8", "8-11", "11-13", "13+")

# --- Define color palette (8 classes, red → green) ---
palette <- c("#a50026", "#d73027", "#f46d43", "#fdae61",
             "#fee08b", "#a6d96a", "#66bd63", "#1a9850")

# --- Function to build a map for a given year ---
make_map <- function(year, title) {
  
  col_name <- paste0("Doc", year)
  
  africa_doc_discrete <- africa_doc %>%
    mutate(
      value_cat = cut(.data[[col_name]], 
                     breaks = breaks, 
                     labels = break_labels,
                     include.lowest = TRUE, 
                     right = FALSE),
      value_cat = as.character(value_cat),
      value_cat = ifelse(is.na(value_cat), "Not in AFRO", value_cat),
      actual_value = round(.data[[col_name]], 1)
    )
  
  island_data <- island_centroids %>%
    mutate(
      value_cat = cut(.data[[col_name]], 
                     breaks = breaks, 
                     labels = break_labels,
                     include.lowest = TRUE, 
                     right = FALSE),
      value_cat = as.character(value_cat),
      value_cat = ifelse(is.na(value_cat), "Not in AFRO", value_cat),
      actual_value = round(.data[[col_name]], 1)
    )
  
  all_levels <- c(break_labels, "Not in AFRO")
  africa_doc_discrete$value_cat <- factor(africa_doc_discrete$value_cat, levels = all_levels)
  island_data$value_cat <- factor(island_data$value_cat, levels = all_levels)
  
  ggplot() +
    # Mainland countries
    geom_sf(data = africa_doc_discrete %>% filter(!iso_a3 %in% island_nations),
            aes(fill = value_cat),
            color = "white", size = 0.15) +
    # Island circles
    geom_sf(data = island_data,
            aes(fill = value_cat),
            color = "black", size = 3, shape = 21, stroke = 1.2) +
    # Mainland labels
    geom_sf_text(data = africa_doc_discrete %>% filter(!iso_a3 %in% island_nations),
                 aes(label = iso_a3),
                 size = 2.5, color = "black") +
    # Island labels above circles
    geom_sf_text(data = island_data,
                 aes(label = paste0(iso_a3, "\n", actual_value)),
                 size = 2.8, color = "black", fontface = "bold",
                 nudge_y = 2.5, vjust = 0) +
    scale_fill_manual(
      values = c(setNames(palette, break_labels), "Not in AFRO" = "grey60"),
      name = "Doctors per 10,000 population",
      drop = FALSE,
      na.value = "grey60"
    ) +
    labs(title = title) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}

# --- Build maps ---
p2018 <- make_map(2018, "2018")
p2022 <- make_map(2022, "2022") 
p2024 <- make_map(2024, "2024")

# --- Create a separate legend plot ---
create_legend_plot <- function() {
  dummy_data <- data.frame(
    category = factor(c(break_labels, "Not in AFRO"), 
                     levels = c(break_labels, "Not in AFRO")),
    value = 1:9
  )
  
  ggplot(dummy_data, aes(x = category, y = value, fill = category)) +
    geom_col() +
    scale_fill_manual(
      values = c(setNames(palette, break_labels), "Not in AFRO" = "grey60"),
      name = "Doctors per 10,000 population",
      drop = FALSE
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold", hjust = 0.5),
      legend.text = element_text(size = 10),
      legend.key.size = unit(0.8, "cm"),
      legend.box.margin = margin(0, 0, 0, 0)
    )
}

legend_plot <- create_legend_plot()
legend <- cowplot::get_legend(legend_plot)

# --- Combine maps with single legend ---
final_plot <- (p2018 + p2022 + p2024) /
  plot_spacer() /
  wrap_elements(legend) +
  plot_layout(ncol = 1, heights = c(8, 0.2, 1))

print(final_plot)




###############

# Doctors density maps with discrete classes
# Classes: 0-1, 1-2.5, 2.5-4, 4-5.5, 5.5-8, 8-11, 11-13, 13+

# Load packages
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

# --- Load and prepare data ---
doc25 <- read.table("G:/Brazzaville/HRH 24/Analytics/Mapping/doc25.txt",
                    header = TRUE, sep = '\t')
world <- ne_countries(scale = "medium", returnclass = "sf")

african_iso3 <- c("DZA","AGO","BEN","BWA","BFA","BDI","CPV","CMR","CAF","TCD","COM",
                  "COD","COG","CIV","DJI","EGY","GNQ","ERI","ETH","GAB","GMB","GHA",
                  "GIN","GNB","KEN","LSO","LBR","LBY","MDG","MWI","MLI","MRT","MUS",
                  "MAR","MOZ","NAM","NER","NGA","RWA","STP","SEN","SYC","SLE","SOM",
                  "ZAF","SSD","SDN","SWZ","TZA","TGO","TUN","UGA","ZMB","ZWE")

africa_sf <- world %>% filter(iso_a3 %in% african_iso3)
africa_doc <- africa_sf %>% left_join(doc25, by = "iso_a3")

# Identify island nations
island_nations <- c("CPV","COM","MUS","STP","SYC")

# Get centroids for island nations for circles
island_centroids <- africa_doc %>%
  filter(iso_a3 %in% island_nations) %>%
  st_centroid()

# --- Define discrete breaks ---
breaks <- c(0, 1, 2.5, 4, 5.5, 8, 11, 13, Inf)
break_labels <- c("0-1", "1-2.5", "2.5-4", "4-5.5", "5.5-8", "8-11", "11-13", "13+")

# --- Define color palette (8 classes, red → green) ---
palette <- c("#a50026", "#d73027", "#f46d43", "#fdae61",
             "#fee08b", "#a6d96a", "#66bd63", "#1a9850")

# --- Function to build a map for a given year ---
make_map <- function(year, title) {
  
  col_name <- paste0("Doc", year)
  
  africa_doc_discrete <- africa_doc %>%
    mutate(
      value_cat = cut(.data[[col_name]], 
                     breaks = breaks, 
                     labels = break_labels,
                     include.lowest = TRUE, 
                     right = FALSE),
      value_cat = as.character(value_cat),
      value_cat = ifelse(is.na(value_cat), "Not in AFRO", value_cat),
      actual_value = round(.data[[col_name]], 1)
    )
  
  island_data <- island_centroids %>%
    mutate(
      value_cat = cut(.data[[col_name]], 
                     breaks = breaks, 
                     labels = break_labels,
                     include.lowest = TRUE, 
                     right = FALSE),
      value_cat = as.character(value_cat),
      value_cat = ifelse(is.na(value_cat), "Not in AFRO", value_cat),
      actual_value = round(.data[[col_name]], 1)
    )
  
  # Force factor levels to include ALL categories
  all_levels <- c(break_labels, "Not in AFRO")
  africa_doc_discrete$value_cat <- factor(africa_doc_discrete$value_cat, levels = all_levels)
  island_data$value_cat <- factor(island_data$value_cat, levels = all_levels)
  
  ggplot() +
    # Mainland countries
    geom_sf(data = africa_doc_discrete %>% filter(!iso_a3 %in% island_nations),
            aes(fill = value_cat),
            color = "white", size = 0.15) +
    # Islands as colored circles (included in legend)
    geom_sf(data = island_data,
            aes(fill = value_cat),
            shape = 21, stroke = 0, size = 3) +
    # Labels for mainland
    geom_sf_text(data = africa_doc_discrete %>% filter(!iso_a3 %in% island_nations),
                 aes(label = iso_a3),
                 size = 2.5, color = "black") +
    # Labels for islands with values
    geom_sf_text(data = island_data,
                 aes(label = paste0(iso_a3, "\n", actual_value)),
                 size = 2.8, color = "black", fontface = "bold",
                 nudge_y = 2.5, vjust = 0) +
    # Discrete color scale with ALL categories forced into legend
    scale_fill_manual(
      values = c(setNames(palette, break_labels), "Not in AFRO" = "grey60"),
      name = "Doctors \nper 10,000 population",
      drop = FALSE,   # ensures unused categories still appear
      na.value = "grey60"
    ) +
    labs(title = title) +
    theme_void() +
    theme(
      legend.position = c(0, 0),              
      legend.justification = c(0, 0),         
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 9),
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      legend.key.size = unit(0.6, "cm")
    )
}

# --- Build maps ---
p2018 <- make_map(2018, "2018")
p2022 <- make_map(2022, "2022") 
p2024 <- make_map(2024, "2024")

# --- Combine maps ---
final_plot <- p2018 + p2022 + p2024

print(final_plot)






