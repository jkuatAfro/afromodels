

######setting thresholds

10 doctors per 10,000 population

# Load packages
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

# Load and prepare data
doctors1 <- read.table("G:/Brazzaville/HRH 24/Analytics/Mapping/doctors1.txt",
                       header = TRUE, sep = '\t')
world <- ne_countries(scale = "medium", returnclass = "sf")

african_iso3 <- c("DZA","AGO","BEN","BWA","BFA","BDI","CPV","CMR","CAF","TCD","COM",
                  "COD","COG","CIV","DJI","EGY","GNQ","ERI","ETH","GAB","GMB","GHA",
                  "GIN","GNB","KEN","LSO","LBR","LBY","MDG","MWI","MLI","MRT","MUS",
                  "MAR","MOZ","NAM","NER","NGA","RWA","STP","SEN","SYC","SLE","SOM",
                  "ZAF","SSD","SDN","SWZ","TZA","TGO","TUN","UGA","ZMB","ZWE")

africa_sf <- world %>% filter(iso_a3 %in% african_iso3)
africa_doctors <- africa_sf %>% left_join(doctors1, by = "iso_a3")

# Identify island nations
island_nations <- c("CPV","COM","MUS","STP","SYC")

# --- Compute global min/max across all years ---
global_min <- min(c(africa_doctors$Medical.Doctors.2013,
                    africa_doctors$Medical.Doctors.2018,
                    africa_doctors$Medical.Doctors.2022), na.rm = TRUE)

global_max <- max(c(africa_doctors$Medical.Doctors.2013,
                    africa_doctors$Medical.Doctors.2018,
                    africa_doctors$Medical.Doctors.2022), na.rm = TRUE)

# --- Define fixed threshold at density = 10 ---
threshold <- 10

# --- Define palette (red → yellow → green) ---
palette <- c("#d73027", "#f46d43", "#fdae61", "#fee08b", "#1a9850")

# --- Function to build a map for a given year ---
make_map <- function(year, title) {
  ggplot() +
    geom_sf(data = africa_doctors,
            aes(fill = .data[[paste0("Medical.Doctors.", year)]]),
            color = "white", size = 0.15) +
    geom_sf(data = africa_doctors %>% filter(iso_a3 %in% island_nations),
            aes(fill = .data[[paste0("Medical.Doctors.", year)]]),
            color = "black", size = 0.6) +
    # Non-island labels
    geom_sf_text(data = africa_doctors %>% filter(!iso_a3 %in% island_nations),
                 aes(label = iso_a3),
                 size = 2.5, color = "black") +
    # Island labels with values
    geom_sf_text(data = africa_doctors %>% filter(iso_a3 %in% island_nations),
                 aes(label = paste0(iso_a3, "\n", round(.data[[paste0("Medical.Doctors.", year)]],1))),
                 size = 3.5, color = "black", fontface = "bold") +
    scale_fill_gradientn(
      colours = palette,
      limits = c(global_min, global_max),
      values = scales::rescale(c(global_min, threshold, global_max)),
      na.value = "grey60",
      name = "Doctors per\n10,000"
    ) +
    labs(title = title) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}

# --- Build maps ---
p2013 <- make_map(2013, "2013")
p2018 <- make_map(2018, "2018")
p2022 <- make_map(2022, "2022")

# --- Combine maps side by side ---
final_plot <- p2013 + p2018 + p2022 + plot_layout(ncol = 3)

print(final_plot)



#############################################################


####

#

######setting thresholds
All 
44.5 all employeees per 10,000 population
# 10 doctors per 10,000 population

# Load packages
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

# --- Load and prepare data ---
doctors1 <- read.table("G:/Brazzaville/HRH 24/Analytics/Mapping/all25.txt",
                       header = TRUE, sep = '\t')
world <- ne_countries(scale = "medium", returnclass = "sf")

african_iso3 <- c("DZA","AGO","BEN","BWA","BFA","BDI","CPV","CMR","CAF","TCD","COM",
                  "COD","COG","CIV","DJI","EGY","GNQ","ERI","ETH","GAB","GMB","GHA",
                  "GIN","GNB","KEN","LSO","LBR","LBY","MDG","MWI","MLI","MRT","MUS",
                  "MAR","MOZ","NAM","NER","NGA","RWA","STP","SEN","SYC","SLE","SOM",
                  "ZAF","SSD","SDN","SWZ","TZA","TGO","TUN","UGA","ZMB","ZWE")

africa_sf <- world %>% filter(iso_a3 %in% african_iso3)
africa_doctors <- africa_sf %>% left_join(doctors1, by = "iso_a3")

# Identify island nations
island_nations <- c("CPV","COM","MUS","STP","SYC")

# --- Compute global min/max across all years ---
global_min <- min(c(africa_doctors$all.2018,
                    africa_doctors$all.2022,
                    africa_doctors$all.2024), na.rm = TRUE)

global_max <- max(c(africa_doctors$all.2018,
                    africa_doctors$all.2022,
                    africa_doctors$all.2024), na.rm = TRUE)

# --- Define fixed threshold at density = 44.5 ---
threshold <- 44.5
green_start <- threshold * 1.6   # greens start at 60% above threshold

# --- Define palette (red → yellow → green) ---
palette <- c("#d73027", "#fdae61", "#1a9850", "#006837")

# --- Function to build a map for a given year ---
make_map <- function(year, title) {
  ggplot() +
    geom_sf(data = africa_doctors,
            aes(fill = .data[[paste0("all.", year)]]),
            color = "white", size = 0.15) +
    geom_sf(data = africa_doctors %>% filter(iso_a3 %in% island_nations),
            aes(fill = .data[[paste0("all.", year)]]),
            color = "black", size = 0.6) +
    # Non-island labels
    geom_sf_text(data = africa_doctors %>% filter(!iso_a3 %in% island_nations),
                 aes(label = iso_a3),
                 size = 2.5, color = "black") +
    # Island labels with values
    geom_sf_text(data = africa_doctors %>% filter(iso_a3 %in% island_nations),
                 aes(label = paste0(iso_a3, "\n", round(.data[[paste0("all.", year)]],1))),
                 size = 3.5, color = "black", fontface = "bold") +
    scale_fill_gradientn(
      colours = palette,
      limits = c(global_min, global_max),
      values = scales::rescale(c(global_min, threshold, green_start, global_max)),
      na.value = "grey60",
      name = "All cadres per 10,000 pop",
      breaks = seq(global_min, global_max, length.out = 6),  # spread legend ticks
      guide = guide_colorbar(
        reverse = FALSE,
        barwidth = 20,
        barheight = 1.0,
        title.position = "top",   # <-- legend title above the bar
        title.hjust = 0.5         # center the title
      )
    ) +
    labs(title = title) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}

# --- Build maps ---
p2018 <- make_map(2018, "2018")
p2022 <- make_map(2022, "2022")
p2024 <- make_map(2024, "2024")

# --- Combine maps side by side ---
final_plot <- p2018 + p2022 + p2024 + plot_layout(ncol = 3)

print(final_plot)


\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# Doctors, Nurses, Midwives density maps
# Threshold: 38 per 10,000 population

# Load packages
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

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

# --- Compute global min/max across all years ---
global_min <- min(c(africa_dnm$DNM2018,
                    africa_dnm$DNM2022,
                    africa_dnm$DNM2024), na.rm = TRUE)

global_max <- max(c(africa_dnm$DNM2018,
                    africa_dnm$DNM2022,
                    africa_dnm$DNM2024), na.rm = TRUE)

# --- Define fixed threshold at density = 38 ---
threshold <- 38

# --- Define palette (graduated reds/yellows → greens) ---
palette <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee08b", "#ffffbf", "#1a9850", "#006837")

# --- Function to build a map for a given year ---
make_map <- function(year, title) {
  ggplot() +
    geom_sf(data = africa_dnm,
            aes(fill = .data[[paste0("DNM", year)]]),
            color = "white", size = 0.15) +
    geom_sf(data = africa_dnm %>% filter(iso_a3 %in% island_nations),
            aes(fill = .data[[paste0("DNM", year)]]),
            color = "black", size = 0.6) +
    # Non-island labels
    geom_sf_text(data = africa_dnm %>% filter(!iso_a3 %in% island_nations),
                 aes(label = iso_a3),
                 size = 2.5, color = "black") +
    # Island labels with values
    geom_sf_text(data = africa_dnm %>% filter(iso_a3 %in% island_nations),
                 aes(label = paste0(iso_a3, "\n", round(.data[[paste0("DNM", year)]],1))),
                 size = 3.5, color = "black", fontface = "bold") +
    scale_fill_gradientn(
      colours = palette,
      limits = c(global_min, global_max),
      # Fixed: Ensure green starts exactly at 38
      values = scales::rescale(
        c(global_min, 
          global_min + (threshold - global_min) * 0.2,
          global_min + (threshold - global_min) * 0.4,
          global_min + (threshold - global_min) * 0.6,
          global_min + (threshold - global_min) * 0.8,
          threshold - 0.001,  # Last point before threshold
          threshold,          # First green point
          global_max)
      ),
      na.value = "grey60",
      name = "Doctors, Nurses & Midwives per 10,000 pop",
      breaks = seq(global_min, global_max, length.out = 6),
      guide = guide_colorbar(
        reverse = FALSE,
        barwidth = 20,
        barheight = 1.0,
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    labs(title = title) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}

# --- Build maps ---
p2018 <- make_map(2018, "2018")
p2022 <- make_map(2022, "2022")
p2024 <- make_map(2024, "2024")

# --- Combine maps side by side ---
final_plot <- p2018 + p2022 + p2024 + plot_layout(ncol = 3)

print(final_plot)






























#Doctors, nurses and midwives


# Doctors, Nurses, Midwives density maps
# Threshold: 38 per 10,000 population

# Load packages
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

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

# --- Compute global min/max across all years ---
global_min <- min(c(africa_dnm$DNM2018,
                    africa_dnm$DNM2022,
                    africa_dnm$DNM2024), na.rm = TRUE)

global_max <- max(c(africa_dnm$DNM2018,
                    africa_dnm$DNM2022,
                    africa_dnm$DNM2024), na.rm = TRUE)

# --- Define fixed threshold at density = 38 ---
threshold <- 35
green_start <- threshold * 1.00   # greens start at 01% above threshold

# --- Define palette (graduated reds/yellows → greens) ---
palette <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee08b", "#ffffbf", "#1a9850", "#006837")

# --- Function to build a map for a given year ---
make_map <- function(year, title) {
  ggplot() +
    geom_sf(data = africa_dnm,
            aes(fill = .data[[paste0("DNM", year)]]),
            color = "white", size = 0.15) +
    geom_sf(data = africa_dnm %>% filter(iso_a3 %in% island_nations),
            aes(fill = .data[[paste0("DNM", year)]]),
            color = "black", size = 0.6) +
    # Non-island labels
    geom_sf_text(data = africa_dnm %>% filter(!iso_a3 %in% island_nations),
                 aes(label = iso_a3),
                 size = 2.5, color = "black") +
    # Island labels with values
    geom_sf_text(data = africa_dnm %>% filter(iso_a3 %in% island_nations),
                 aes(label = paste0(iso_a3, "\n", round(.data[[paste0("DNM", year)]],1))),
                 size = 3.5, color = "black", fontface = "bold") +
    scale_fill_gradientn(
      colours = palette,
      limits = c(global_min, global_max),
      values = scales::rescale(c(global_min, threshold, green_start, global_max)),
      na.value = "grey60",
      name = "Doctors, Nurses & Midwives per 10,000 pop",   # legend title
      breaks = seq(global_min, global_max, length.out = 6),
      guide = guide_colorbar(
        reverse = FALSE,
        barwidth = 20,
        barheight = 1.0,
        title.position = "top",   # legend title above the bar
        title.hjust = 0.5         # center the title
      )
    ) +
    labs(title = title) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}

# --- Build maps ---
p2018 <- make_map(2018, "2018")
p2022 <- make_map(2022, "2022")
p2024 <- make_map(2024, "2024")

# --- Combine maps side by side ---
final_plot <- p2018 + p2022 + p2024 + plot_layout(ncol = 3)

print(final_plot)








\\\\\



# Doctors, Nurses, Midwives, Pharmacists, Dentists density maps
# Threshold: 44.5 per 10,000 population

# Doctors, Nurses, Midwives, Pharmacists, Dentists density maps
# Threshold: 44.5 per 10,000 population

# Load packages
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

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

# --- Compute global min/max across all years ---
global_min <- min(c(africa_dnmpd$DNMPD2018,
                    africa_dnmpd$DNMPD2022,
                    africa_dnmpd$DNMPD2024), na.rm = TRUE)

global_max <- max(c(africa_dnmpd$DNMPD2018,
                    africa_dnmpd$DNMPD2022,
                    africa_dnmpd$DNMPD2024), na.rm = TRUE)

# --- Define fixed threshold at density = 44.5 ---
threshold <- 44.5

# --- Define palette (graduated reds/yellows → greens) ---
palette <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee08b", "#ffffbf", "#1a9850", "#006837")

# --- Function to build a map for a given year ---
make_map <- function(year, title) {
  ggplot() +
    geom_sf(data = africa_dnmpd,
            aes(fill = .data[[paste0("DNMPD", year)]]),
            color = "white", size = 0.15) +
    geom_sf(data = africa_dnmpd %>% filter(iso_a3 %in% island_nations),
            aes(fill = .data[[paste0("DNMPD", year)]]),
            color = "black", size = 0.6) +
    # Non-island labels
    geom_sf_text(data = africa_dnmpd %>% filter(!iso_a3 %in% island_nations),
                 aes(label = iso_a3),
                 size = 2.5, color = "black") +
    # Island labels with values
    geom_sf_text(data = africa_dnmpd %>% filter(iso_a3 %in% island_nations),
                 aes(label = paste0(iso_a3, "\n", round(.data[[paste0("DNMPD", year)]],1))),
                 size = 3.5, color = "black", fontface = "bold") +
    scale_fill_gradientn(
      colours = palette,
      limits = c(global_min, global_max),
      # Fixed: Create proper breakpoints for the 8-color palette
      values = scales::rescale(
        c(global_min, 
          global_min + (threshold - global_min) * 0.2,
          global_min + (threshold - global_min) * 0.4,
          global_min + (threshold - global_min) * 0.6,
          global_min + (threshold - global_min) * 0.8,
          threshold - 0.001,  # Just before threshold
          threshold,           # Exactly at threshold
          global_max)
      ),
      na.value = "grey60",
      name = "Doctors, Nurses, Midwives,\nPharmacists & Dentists per 10,000 pop",
      breaks = seq(global_min, global_max, length.out = 6),
      guide = guide_colorbar(
        reverse = FALSE,
        barwidth = 20,
        barheight = 1.0,
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    labs(title = title) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}

# --- Build maps ---
p2018 <- make_map(2018, "2018")
p2022 <- make_map(2022, "2022")
p2024 <- make_map(2024, "2024")

# --- Combine maps side by side ---
final_plot <- p2018 + p2022 + p2024 + plot_layout(ncol = 3)

print(final_plot)


































# Load packages
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

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

# --- Compute global min/max across all years ---
global_min <- min(c(africa_dnmpd$DNMPD2018,
                    africa_dnmpd$DNMPD2022,
                    africa_dnmpd$DNMPD2024), na.rm = TRUE)

global_max <- max(c(africa_dnmpd$DNMPD2018,
                    africa_dnmpd$DNMPD2022,
                    africa_dnmpd$DNMPD2024), na.rm = TRUE)

# --- Define fixed threshold at density = 44.5 ---
threshold <- 44.5

# --- Define palette (graduated reds/yellows → greens) ---
palette <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee08b", "#ffffbf", "#1a9850", "#006837")

# --- Function to build a map for a given year ---
make_map <- function(year, title) {
  ggplot() +
    geom_sf(data = africa_dnmpd,
            aes(fill = .data[[paste0("DNMPD", year)]]),
            color = "white", size = 0.15) +
    geom_sf(data = africa_dnmpd %>% filter(iso_a3 %in% island_nations),
            aes(fill = .data[[paste0("DNMPD", year)]]),
            color = "black", size = 0.6) +
    # Non-island labels
    geom_sf_text(data = africa_dnmpd %>% filter(!iso_a3 %in% island_nations),
                 aes(label = iso_a3),
                 size = 2.5, color = "black") +
    # Island labels with values
    geom_sf_text(data = africa_dnmpd %>% filter(iso_a3 %in% island_nations),
                 aes(label = paste0(iso_a3, "\n", round(.data[[paste0("DNMPD", year)]],1))),
                 size = 3.5, color = "black", fontface = "bold") +
    scale_fill_gradientn(
      colours = palette,
      limits = c(global_min, global_max),
      # Corrected: green starts exactly at threshold
      values = scales::rescale(c(global_min, threshold - 0.001, threshold, global_max)),
      na.value = "grey60",
      name = "Doctors, Nurses, Midwives,\nPharmacists & Dentists per 10,000 pop",   # legend title
      breaks = seq(global_min, global_max, length.out = 6),
      guide = guide_colorbar(
        reverse = FALSE,
        barwidth = 20,
        barheight = 1.0,
        title.position = "top",   # legend title above the bar
        title.hjust = 0.5         # center the title
      )
    ) +
    labs(title = title) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}

# --- Build maps ---
p2018 <- make_map(2018, "2018")
p2022 <- make_map(2022, "2022")
p2024 <- make_map(2024, "2024")

# --- Combine maps side by side ---
final_plot <- p2018 + p2022 + p2024 + plot_layout(ncol = 3)

print(final_plot)


\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\||||||


# Doctors, Nurses, Midwives, Pharmacists, Dentists density maps
# Threshold: 44.5 per 10,000 population

# Load packages
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

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

# --- Compute global min/max across all years ---
global_min <- min(c(africa_dnmpd$DNMPD2018,
                    africa_dnmpd$DNMPD2022,
                    africa_dnmpd$DNMPD2024), na.rm = TRUE)

global_max <- max(c(africa_dnmpd$DNMPD2018,
                    africa_dnmpd$DNMPD2022,
                    africa_dnmpd$DNMPD2024), na.rm = TRUE)

# --- Define fixed threshold at density = 44.5 ---
threshold <- 44.5
green_start <- threshold * 1.0   # greens start at 0% above threshold

# --- Define palette (graduated reds/yellows → greens) ---
palette <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee08b", "#ffffbf", "#1a9850", "#006837")

# --- Function to build a map for a given year ---
make_map <- function(year, title) {
  ggplot() +
    geom_sf(data = africa_dnmpd,
            aes(fill = .data[[paste0("DNMPD", year)]]),
            color = "white", size = 0.15) +
    geom_sf(data = africa_dnmpd %>% filter(iso_a3 %in% island_nations),
            aes(fill = .data[[paste0("DNMPD", year)]]),
            color = "black", size = 0.6) +
    # Non-island labels
    geom_sf_text(data = africa_dnmpd %>% filter(!iso_a3 %in% island_nations),
                 aes(label = iso_a3),
                 size = 2.5, color = "black") +
    # Island labels with values
    geom_sf_text(data = africa_dnmpd %>% filter(iso_a3 %in% island_nations),
                 aes(label = paste0(iso_a3, "\n", round(.data[[paste0("DNMPD", year)]],1))),
                 size = 3.5, color = "black", fontface = "bold") +
    scale_fill_gradientn(
      colours = palette,
      limits = c(global_min, global_max),
      values = scales::rescale(c(global_min, threshold, green_start, global_max)),
      na.value = "grey60",
      name = "Doctors, Nurses, Midwives,\nPharmacists & Dentists per 10,000 pop",   # legend title
      breaks = seq(global_min, global_max, length.out = 6),
      guide = guide_colorbar(
        reverse = FALSE,
        barwidth = 20,
        barheight = 1.0,
        title.position = "top",   # legend title above the bar
        title.hjust = 0.5         # center the title
      )
    ) +
    labs(title = title) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}

# --- Build maps ---
p2018 <- make_map(2018, "2018")
p2022 <- make_map(2022, "2022")
p2024 <- make_map(2024, "2024")

# --- Combine maps side by side ---
final_plot <- p2018 + p2022 + p2024 + plot_layout(ncol = 3)

print(final_plot)







\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

 
> 
> # Nurses & Midwives density maps
> # Threshold: 23 per 10,000 population
> 
> # Load packages
> library(ggplot2)
> library(dplyr)
> library(sf)
> library(rnaturalearth)
> library(rnaturalearthdata)
> library(patchwork)
> 
> # --- Load and prepare data ---
> nursmid25 <- read.table("G:/Brazzaville/HRH 24/Analytics/Mapping/nursmid25.txt",
+                         header = TRUE, sep = '\t')
> world <- ne_countries(scale = "medium", returnclass = "sf")
> 
> african_iso3 <- c("DZA","AGO","BEN","BWA","BFA","BDI","CPV","CMR","CAF","TCD","COM",
+                   "COD","COG","CIV","DJI","EGY","GNQ","ERI","ETH","GAB","GMB","GHA",
+                   "GIN","GNB","KEN","LSO","LBR","LBY","MDG","MWI","MLI","MRT","MUS",
+                   "MAR","MOZ","NAM","NER","NGA","RWA","STP","SEN","SYC","SLE","SOM",
+                   "ZAF","SSD","SDN","SWZ","TZA","TGO","TUN","UGA","ZMB","ZWE")
> 
> africa_sf <- world %>% filter(iso_a3 %in% african_iso3)
> africa_nm <- africa_sf %>% left_join(nursmid25, by = "iso_a3")
> 
> # Identify island nations
> island_nations <- c("CPV","COM","MUS","STP","SYC")
> 
> # --- Compute global min/max across all years ---
> global_min <- min(c(africa_nm$NM2018,
+                     africa_nm$NM2022,
+                     africa_nm$NM2024), na.rm = TRUE)
> 
> global_max <- max(c(africa_nm$NM2018,
+                     africa_nm$NM2022,
+                     africa_nm$NM2024), na.rm = TRUE)
> 
> # --- Define fixed threshold at density = 23 ---
> threshold <- 23
> 
> # --- Define palette (red → yellow → green) ---
> palette <- c("#d73027", "#fdae61", "#1a9850")
> 
> # --- Function to build a map for a given year ---
> make_map <- function(year, title) {
+   ggplot() +
+     geom_sf(data = africa_nm,
+             aes(fill = .data[[paste0("NM", year)]]),
+             color = "white", size = 0.15) +
+     geom_sf(data = africa_nm %>% filter(iso_a3 %in% island_nations),
+             aes(fill = .data[[paste0("NM", year)]]),
+             color = "black", size = 0.6) +
+     # Non-island labels
+     geom_sf_text(data = africa_nm %>% filter(!iso_a3 %in% island_nations),
+                  aes(label = iso_a3),
+                  size = 2.5, color = "black") +
+     # Island labels with values
+     geom_sf_text(data = africa_nm %>% filter(iso_a3 %in% island_nations),
+                  aes(label = paste0(iso_a3, "\n", round(.data[[paste0("NM", year)]],1))),
+                  size = 3.5, color = "black", fontface = "bold") +
+     scale_fill_gradientn(
+       colours = palette,
+       limits = c(global_min, global_max),
+       # Corrected: green starts exactly at threshold
+       values = scales::rescale(c(global_min, threshold - 0.001, threshold, global_max)),
+       na.value = "grey60",
+       name = "Nurses & Midwives per 10,000 population",
+       breaks = seq(global_min, global_max, length.out = 6),
+       guide = guide_colorbar(
+         reverse = FALSE,
+         barwidth = 20,
+         barheight = 1.0,
+         title.position = "top",
+         title.hjust = 0.5
+       )
+     ) +
+     labs(title = title) +
+     theme_void() +
+     theme(
+       legend.position = "bottom",
+       legend.title = element_text(size = 12, face = "bold"),
+       legend.text = element_text(size = 10),
+       plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
+     )
+ }
> 
> # --- Build maps ---
> p2018 <- make_map(2018, "2018")
> p2022 <- make_map(2022, "2022")
> p2024 <- make_map(2024, "2024")
> 
> # --- Combine maps side by side ---
> final_plot <- p2018 + p2022 + p2024 + plot_layout(ncol = 3)
> 
> print(final_plot)
























# Nurses & Midwives density maps
# Threshold: 23 per 10,000 population

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

# --- Compute global min/max across all years ---
global_min <- min(c(africa_nm$NM2018,
                    africa_nm$NM2022,
                    africa_nm$NM2024), na.rm = TRUE)

global_max <- max(c(africa_nm$NM2018,
                    africa_nm$NM2022,
                    africa_nm$NM2024), na.rm = TRUE)

# --- Define fixed threshold at density = 23 ---
threshold <- 23

# --- Define palette (graduated reds/yellows → greens) ---
palette <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee08b", "#ffffbf", "#1a9850", "#006837")

# --- Function to build a map for a given year ---
make_map <- function(year, title) {
  ggplot() +
    geom_sf(data = africa_nm,
            aes(fill = .data[[paste0("NM", year)]]),
            color = "white", size = 0.15) +
    geom_sf(data = africa_nm %>% filter(iso_a3 %in% island_nations),
            aes(fill = .data[[paste0("NM", year)]]),
            color = "black", size = 0.6) +
    # Non-island labels
    geom_sf_text(data = africa_nm %>% filter(!iso_a3 %in% island_nations),
                 aes(label = iso_a3),
                 size = 2.5, color = "black") +
    # Island labels with values
    geom_sf_text(data = africa_nm %>% filter(iso_a3 %in% island_nations),
                 aes(label = paste0(iso_a3, "\n", round(.data[[paste0("NM", year)]],1))),
                 size = 3.5, color = "black", fontface = "bold") +
    scale_fill_gradientn(
      colours = palette,
      limits = c(global_min, global_max),
      values = scales::rescale(c(global_min, threshold, global_max)),  # threshold only
      na.value = "grey60",
      name = "Nurses & Midwives per 10,000 population",   # legend title
      breaks = seq(global_min, global_max, length.out = 6),
      guide = guide_colorbar(
        reverse = FALSE,
        barwidth = 20,
        barheight = 1.0,
        title.position = "top",   # legend title above the bar
        title.hjust = 0.5         # center the title
      )
    ) +
    labs(title = title) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}

# --- Build maps ---
p2018 <- make_map(2018, "2018")
p2022 <- make_map(2022, "2022")
p2024 <- make_map(2024, "2024")

# --- Combine maps side by side ---
final_plot <- p2018 + p2022 + p2024 + plot_layout(ncol = 3)

print(final_plot)




\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


# Doctors density maps
# Threshold: 10 per 10,000 population

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

# --- Compute global min/max across all years ---
global_min <- min(c(africa_doc$Doc2018,
                    africa_doc$Doc2022,
                    africa_doc$Doc2024), na.rm = TRUE)

global_max <- max(c(africa_doc$Doc2018,
                    africa_doc$Doc2022,
                    africa_doc$Doc2024), na.rm = TRUE)

# --- Define fixed threshold at density = 10 ---
threshold <- 10

# --- Define palette (graduated reds/yellows → greens) ---
palette <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee08b", "#ffffbf", "#1a9850", "#006837")

# --- Function to build a map for a given year ---
make_map <- function(year, title) {
  ggplot() +
    geom_sf(data = africa_doc,
            aes(fill = .data[[paste0("Doc", year)]]),
            color = "white", size = 0.15) +
    geom_sf(data = africa_doc %>% filter(iso_a3 %in% island_nations),
            aes(fill = .data[[paste0("Doc", year)]]),
            color = "black", size = 0.6) +
    # Non-island labels
    geom_sf_text(data = africa_doc %>% filter(!iso_a3 %in% island_nations),
                 aes(label = iso_a3),
                 size = 2.5, color = "black") +
    # Island labels with values
    geom_sf_text(data = africa_doc %>% filter(iso_a3 %in% island_nations),
                 aes(label = paste0(iso_a3, "\n", round(.data[[paste0("Doc", year)]],1))),
                 size = 3.5, color = "black", fontface = "bold") +
    scale_fill_gradientn(
      colours = palette,
      limits = c(global_min, global_max),
      # Ensure green starts exactly at 10
      values = scales::rescale(
        c(global_min, 
          global_min + (threshold - global_min) * 0.2,
          global_min + (threshold - global_min) * 0.4,
          global_min + (threshold - global_min) * 0.6,
          global_min + (threshold - global_min) * 0.8,
          threshold - 0.001,  # Last point before threshold
          threshold,          # First green point
          global_max)
      ),
      na.value = "grey60",
      name = "Doctors per 10,000 population",
      breaks = seq(global_min, global_max, length.out = 6),
      guide = guide_colorbar(
        reverse = FALSE,
        barwidth = 20,
        barheight = 1.0,
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    labs(title = title) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}

# --- Build maps ---
p2018 <- make_map(2018, "2018")
p2022 <- make_map(2022, "2022")
p2024 <- make_map(2024, "2024")

# --- Combine maps side by side ---
final_plot <- p2018 + p2022 + p2024 + plot_layout(ncol = 3)

print(final_plot)

