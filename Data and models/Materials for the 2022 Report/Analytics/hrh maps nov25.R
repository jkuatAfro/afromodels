




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

# --- Define fixed threshold at density = 10 --- for med doctors
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
