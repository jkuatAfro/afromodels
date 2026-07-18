# install.packages("ggplot2")
# install.packages("sf")
library(ggplot2)
library(sf)

# Import a geojson or shapefile
map <- read_sf("https://raw.githubusercontent.com/R-CoderDotCom/data/main/shapefile_spain/spain.geojson")

ggplot(map) +
  geom_sf(color = "white", aes(fill = name)) +
  theme(legend.position = "none")




# Import a geojson or shapefile
map <- read_sf(word1)

ggplot(wrld_simpl) +
  geom_sf(color = "white", aes(fill = name)) +
  theme(legend.position = "none")


destfile="D:/DATA/world_shape_file.zip"

# Download the shapefile. (note that I store it in a folder called DATA. You have to change that if needed.)
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile)
# You now have it in your current working directory, have a look!



library(rgdal)
library(ggplot2)
library(maptools)
library(rgeos)
library(RColorBrewer)


world.map <- readOGR(dsn="/Users/bob/Desktop/TM_WORLD_BORDERS_SIMPL-0.3/", layer="TM_WORLD_BORDERS_SIMPL-0.3")

# Get centroids of countries
theCents <- coordinates(world.map)






 require(maptools)
  world.map.simplified <- readShapeSpatial("~/TM_WORLD_BORDERS_SIMPL-0.3/TM_WORLD_BORDERS_SIMPL-0.3.shp")
