 ###remotes::install_github("afrimapr/afrilearndata")


###Require Rtools
###install.packages("sf")
##install.packages("raster")
##install.packages("tmap")
##install.packages("mapview")
##install.packages("afrilearndata")
##install.packages("tmap")



library(sf)     # for vector data handling
library(raster) # for raster data handling
library(tmap) # static & interactive mapping
library(mapview) #interactive mapping
library(afrilearndata) # example spatial data for Africa
library(rgdal) #seems to be needed for mapview raster

#you may need :
#remotes::install_github("afrimapr/afrilearnr")

#temporary fix for shinyapps & may help for users with an older version of rgdal
sf::st_crs(africapitals) <- 4326
sf::st_crs(afrihighway) <- 4326
sf::st_crs(africountries) <- 4326
sf::st_crs(africontinent) <- 4326 

####interactive ploting

tmap::tm_shape(afripop2020) +
    tm_raster(palette = rev(viridisLite::magma(5)), breaks=c(0,2,20,200,2000,25000)) +
tm_shape(africountries) +
    tm_borders("white", lwd = .5) +
    #tm_text("iso_a3", size = "AREA") +
tm_shape(afrihighway) +
    tm_lines(col = "red") + 
tm_shape(africapitals) +
    tm_symbols(col = "blue", alpha=0.4, scale = .6 )+
tm_legend(show = FALSE)


#################

tm_shape(africountries) +
    tm_polygons("pop_est")    #try commenting out above line and uncommenting that below
    #tm_polygons("income_grp")



####################

library(ggplot2)
library(ggrepel)

ggplot(africountries) +
    geom_sf(aes(fill = pop_est)) +
    scale_fill_viridis_c() +
    theme_void() +
    geom_text_repel(aes(label=name_sw, geometry=geometry),
                    stat="sf_coordinates",
                    point.padding = NA, #allows points to overlap centroid
                    colour='darkgrey', size=3
                   ) +
    labs(title = "Population by country 2000", fill = "Population Estimate")

#####################



library(ggplot2)
library(sf)

# Import a geojson or shapefile
map <- read_sf("https://raw.githubusercontent.com/R-CoderDotCom/data/main/shapefile_spain/spain.geojson")

ggplot(map) +
  geom_sf(color = "white", aes(fill = unemp_rate)) +
  geom_text_repel(aes(label = name, geometry = geometry),
                  stat = "sf_coordinates", size = 3) +
  theme(legend.position = "none")


##################################


setwd("D:/DATA")

mapA <- read_sf("custom.geo.json")
ggplot(mapA)





## Download the shape files to working directory ##
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="TM_WORLD_BORDERS_SIMPL-0.3.zip")
## Unzip them ##
unzip("TM_WORLD_BORDERS_SIMPL-0.3.zip")



###################################################


# Download the shapefile. (note that I store it in a folder called DATA. You have to change that if needed.)
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="DATA/world_shape_file.zip")
# You now have it in your current working directory, have a look!

# Unzip this file. You can do it with R (as below), or clicking on the object you downloaded.
system("unzip DATAMaps/world_shape_file.zip")
#  -- > You now have 4 files. One of these files is a .shp file! (TM_WORLD_BORDERS_SIMPL-0.3.shp)


word1=data(wrld_simpl)
plot(wrld_simpl)

download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip")





library(rgl)
library(rgdal)
library(raster)

shp <- readOGR(dsn="[myprojectfolder]/GADM/gadm36.shp", layer="gadm36")
countryweb <-  "https://pkgstore.datahub.io/JohnSnowLabs/country-and-continent-codes-list/country-and-continent-codes-list-csv_csv/data/b7876b7f496677669644f3d1069d3121/country-and-continent-codes-list-csv_csv.csv"
country.csv <- read.csv(countryweb)
names(country.csv)[5] <- "GID_0"
africaCountries <- subset(country.csv, Continent_Code=="AF")
africa_shp <- subset(shp, GID_0 %in% africaCountries$GID_0 )
## Store the Africa shapefile so that you don't have to import the whole world next time:
writeOGR(africa_shp,".", "africa-rgdal", driver="ESRI Shapefile")



