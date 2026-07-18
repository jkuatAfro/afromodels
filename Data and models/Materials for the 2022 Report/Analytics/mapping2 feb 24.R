library(ggplot2)
library(dplyr)
require(maps)
require(viridis)


library(sf)     # for vector data handling
library(raster) # for raster data handling
library(tmap) # static & interactive mapping
library(mapview) #interactive mapping
library(afrilearndata) # example spatial data for Africa
library(rgdal) #seems to be needed for mapview raster



theme_set(theme_void())

#theme_set(theme_bw())

world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")

head(world_map)

table(world_map$region)


Africa=c("Algeria", "Angola", "Benin",
"Botswana",
"Burkina Faso",
"Burundi", "Cameroon",
"Cape Verde", "Central African Republic",
"Chad", 
"Comoros",
"Democratic Republic of the Congo",
"Djibouti",
"Egypt", "Equatorial Guinea",
"Eritrea", "Ethiopia", 
"Gabon",
"Gambia", "Ghana",
"Guinea", "Guinea-Bissau", 
"Ivory Coast", 
"Kenya", 
"Lesotho", "Liberia",
"Libya", "Madagascar", "Malawi","Mali", "Mauritania",
"Mauritius", 
"Morocco", "Mozambique", "Namibia",
"Niger", "Nigeria",
"Republic of Congo", 
"Rwanda", "Saint Helena",
"Sao Tome and Principe","Senegal",
"Seychelles",
"Sierra Leone", 
"Somalia", "South Africa", 
"South Sudan", 
"Sudan", "Swaziland",
"Tanzania","Togo", 
"Tunisia","Western Sahara",
"Uganda",
"Zambia", "Zimbabwe")




# Retrievethe map data
africa.maps <- map_data("world", region = Africa)

SYafrica.maps <- map_data("world", region = Seychelles)




head(africa.maps)


#####creating multipoygons

poly <- data.frame(
    lon = c(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 2, 2, 0.8, 1, 1, 2, 2, 1, 1),
    lat = c(0, 0, 1, 1.5, 0, 1, 1, 2, 2, 1, 1, 1, 2, 2, 1, 0, 0, 1, 1, 0),
    var = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3 ,3 ,3 ,3 ,3, 4 ,4 ,4, 4, 4)
  ) %>%
  st_as_sf(coords = c("lon", "lat"), dim = "XY") %>% st_set_crs(4326) %>% 
group_by(var) %>%
  summarise(geometry = st_union(geometry), do_union = F) %>% st_cast("POLYGON")



# Recode factor levels by name
africa.maps$region <- recode(africa.maps$region, Swaziland = "Eswatini")


#####Creating a multipolygon for the africa.maps data


africa.maps
 %>%
  st_as_sf(coords = c("long", "lat"), dim = "XY") %>% st_set_crs(4326) %>% 
group_by(region) %>%
  summarise(geometry = st_union(geometry), do_union = F) %>% st_cast("POLYGON")


africa_coord <- st_as_sf(africa.maps, coords = c(1:2),dim = "XY")%>% st_set_crs(4326) %>% 
group_by(region) %>%
  summarise(geometry = st_union(geometry), do_union = F) %>% st_cast("POLYGON")

africa_coord
table(africa_coord$region)




# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- africa.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

ggplot(africa.maps, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = region))+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")


#######loading data

###medical doctors

doctors=read.table("H:/Brazzaville/HRH 24/Analytics/Mapping/doctors.txt",header=T,sep='\t')


doctors1=read.table("H:/Brazzaville/HRH 24/Analytics/Mapping/doctors1.txt",header=T,sep='\t')


DNMs=read.table("H:/Brazzaville/HRH 24/Analytics/Mapping/DocNursMidwives.txt",header=T,sep='\t')

AllCadres=read.table("H:/Brazzaville/HRH 24/Analytics/Mapping/AllCadres.txt",header=T,sep='\t')


Nurses=read.table("H:/Brazzaville/HRH 24/Analytics/Mapping/Nurses.txt",header=T,sep='\t')


Midwives=read.table("H:/Brazzaville/HRH 24/Analytics/Mapping/Midwives.txt",header=T,sep='\t')


Dentists=read.table("H:/Brazzaville/HRH 24/Analytics/Mapping/Dentists.txt",header=T,sep='\t')


Pharmacists=read.table("H:/Brazzaville/HRH 24/Analytics/Mapping/Pharmacists.txt",header=T,sep='\t')

Nurses_Midwives=read.table("H:/Brazzaville/HRH 24/Analytics/Mapping/Nurses_Midwives.txt",header=T,sep='\t')

dataA=read.table("H:/Brazzaville/HRH 24/Analytics/Mapping/data_avail.txt",header=T,sep='\t')




###merge the data

doctors.exp.map <- left_join(africa_coord,doctors, by = "region")

africa_coord_doctors1<- left_join(africountries,doctors1, by = "iso_a3")

DNMs_coord<- left_join(africountries,DNMs, by = "iso_a3")

Allcadres_coord<- left_join(africountries,AllCadres, by = "iso_a3")

Nurses_coord<- left_join(africountries,Nurses, by = "iso_a3")


Midwives_coord<- left_join(africountries,Midwives, by = "iso_a3")


Dentists_coord<- left_join(africountries,Dentists, by = "iso_a3")


Pharmacists_coord<- left_join(africountries,Pharmacists, by = "iso_a3")


Nurses_Midwives_coord<- left_join(africountries,Nurses_Midwives, by = "iso_a3")

dataA_coord<- left_join(africountries,dataA, by = "iso_a3")


################

table(africountries$name_long)

table(africa_coord_doctors1$name)
####


library(ggplot2)
library(ggrepel)

ggplot(africa_coord_doctors1) +
    geom_sf(aes(fill = Medical.Doctors.2018)) +
    scale_fill_viridis_c() +
    theme_void() +
    geom_text_repel(aes(label=name_long, geometry=geometry),
                    stat="sf_coordinates",
                    point.padding = NA, #allows points to overlap centroid
                    colour='blue', size=3
                   ) +
    labs(title = "Medical doctors 2018", fill = "Density per 10,000 pop")

#####################
require(maps)
map(database = "world", regions = c('comoros'))
map(database = "world", regions = c('seychelles'))
map(database = "world", regions = c('Sao Tome and Principe'))

#####to change polygon to raster using terra
require(raster)
require(fasterize)
library(terra)

 Mauritius. Latitude: -20.2067 Longitude: 57.6755
Comoros. Latitude: -11.6520 Longitude: 43.3726
São Tomé is located at latitude 0.33654 and longitude 6.72732



syc1=c(-4.679574, 55.491977)

syc_dat <- data.frame(lat=-4.679574, lon=55.491977)
cpv_dat<-data.frame(lat=15.1201, lon=-23.6052)
mus_dat<-data.frame(lat= -20.2067, lon=57.6755)
stp_dat<-data.frame(lat=0.33654, lon=6.72732)
com_dat<-data.frame(lat=-11.6520, lon=43.3726)


syc_dat$region <- "SYC"
cpv_dat$region <- "CPV"
mus_dat$region <- "MUS"
stp_dat$region <- "STP"
com_dat$region <- "COM"
dat1 <- rbind(syc_dat, cpv_dat,mus_dat,stp_dat,com_dat)


library(sf)
library(dplyr)

hulls <- dat1 %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  group_by(region) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_convex_hull()
plot(hulls)


library(sf)
library(dplyr)
library(tmap)
library(tmaptools)

sf_use_s2(TRUE)

dat_sf <- st_as_sf(dat1, coords = c("lon", "lat"), crs = 4326)

conv_syc <- st_simplify(st_buffer(st_convex_hull(st_union(st_geometry(filter(dat_sf, region == "SYC")))), dist = 15000), dTolerance = 5000)
conv_cpv <- st_simplify(st_buffer(st_convex_hull(st_union(st_geometry(filter(dat_sf, region == "CPV")))), dist = 15000), dTolerance = 5000)
conv_mus <- st_simplify(st_buffer(st_convex_hull(st_union(st_geometry(filter(dat_sf, region == "MUS")))), dist = 15000), dTolerance = 5000)
conv_stp <- st_simplify(st_buffer(st_convex_hull(st_union(st_geometry(filter(dat_sf, region == "STP")))), dist = 15000), dTolerance = 5000)
conv_com <- st_simplify(st_buffer(st_convex_hull(st_union(st_geometry(filter(dat_sf, region == "COM")))), dist = 15000), dTolerance = 5000)


#sf_use_s2(T)



#tmap_mode("view")

#tmap_mode("plot")

#map_style("white")
tm_shape(africa_coord_doctors1, bbox = sf::st_bbox(c(xmin = -25, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Medical.Doctors.2018", palette = "RdYlGn", n=7,title = "Doctors density per 10,000 (2018)", textNA = "Not in AFRO", border.alpha = 0.5)+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "forestgreen", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "goldenrod1", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "forestgreen", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkolivegreen2", lwd = 12)+
tm_shape(conv_cpv)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')


ggplot2::ggplot(data = africa_coord_doctors1, aes(fill = Medical.Doctors.2018)) +
  ggplot2::geom_sf() +
  ggplot2::scale_fill_viridis_c(alpha = 0.75) +
  ggplot2::theme_bw()+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+
  tm_borders(col = "forestgreen", lwd = 15)+
  tm_shape(conv_cpv)+
  tm_borders(col = "goldenrod1", lwd = 15)+
tm_shape(conv_mus)+
  tm_borders(col = "forestgreen", lwd = 15)+
tm_shape(conv_stp)+
  tm_borders(col = "darkolivegreen2", lwd = 15)+
tm_shape(conv_cpv)+
  tm_borders(col = "brown", lwd = 15)+
lims(x = c(-60, 160), y= c(-125, 160))




geom_text(data= africa_coord_doctors1,aes(x=X, y=Y, label=iso_a3),
    color = "darkblue", fontface = "bold", check_overlap = FALSE)



ggplot2::ggplot(data = africa_coord_doctors1, aes(fill = Medical.Doctors.2018)) +
  ggplot2::geom_sf() +
  ggplot2::scale_fill_viridis_c(alpha = 0.75) +
  ggplot2::xlab("Longitude") +
  ggplot2::ylab("Latitude") +
  ggplot2::labs(
    title = "Medical Doctors density in 2018",
    caption = "Source: WHO 2024",
    fill = "Medical.Doctors.2018"
  ) + 
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_line(
      color = gray(0.5), linetype = "dashed", size = 0.5
    ),
    panel.background = ggplot2::element_rect(fill = gray(0.75))
  )

tm_borders(alpha = 0.5) 

+
 


tmap_style()

###############################################

##### Medical doctors

###################################################


breaks = c(0,1, 2.5, 4, 5.5,8,11,13,15)
tm_shape(africa_coord_doctors1,bbox = sf::st_bbox(c(xmin = -25, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Medical.Doctors.2022", palette = "RdYlGn", breaks = breaks,title = "Doctors density per 10,000 (2022)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "darkolivegreen", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "goldenrod1", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "forestgreen", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkolivegreen2", lwd = 12)+
tm_shape(conv_cpv)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')




breaks = c(0,1, 2.5, 4, 5.5,8,11,13,15)
tm_shape(africa_coord_doctors1,bbox = sf::st_bbox(c(xmin = -25, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Medical.Doctors.2018", palette = "RdYlGn", breaks = breaks,title = "Doctors density per 10,000 (2018)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "forestgreen", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "goldenrod1", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "forestgreen", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkolivegreen2", lwd = 12)+
tm_shape(conv_cpv)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')



breaks = c(0,1, 2.5, 4, 5.5,8,11,13,15)
tm_shape(africa_coord_doctors1,bbox = sf::st_bbox(c(xmin = -25, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Medical.Doctors.2013", palette = "RdYlGn", breaks = breaks,title = "Doctors density per 10,000 (2013)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "forestgreen", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "goldenrod1", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "forestgreen", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkolivegreen2", lwd = 12)+
tm_shape(conv_cpv)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')



#####
##Doctors nurses and midwives


breaks = c(0,7, 15, 25, 38,42,45,50)
tm_shape(DNMs_coord,bbox = sf::st_bbox(c(xmin = -27, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Density.2013", palette = "RdYlGn", breaks = breaks,title = "Doctors, Nurses and Midwives (2013)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "forestgreen", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "goldenrod1", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "green1", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')


breaks = c(0,7, 15, 25, 38,42,45,50)
tm_shape(DNMs_coord,bbox = sf::st_bbox(c(xmin = -27, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Density.2018", palette = "RdYlGn", breaks = breaks,title = "Doctors, Nurses and Midwives (2018)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "forestgreen", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "goldenrod1", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "forestgreen", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkolivegreen2", lwd = 12)+
tm_shape(conv_cpv)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')



breaks = c(0,7, 15, 25, 38,42,45,50)
tm_shape(DNMs_coord,bbox = sf::st_bbox(c(xmin = -27, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Density.2022", palette = "RdYlGn", breaks = breaks,title = "Doctors, Nurses and Midwives (2022)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "forestgreen", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "goldenrod1", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "forestgreen", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkolivegreen2", lwd = 12)+
tm_shape(conv_cpv)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')

####################################

breaks = c(0,7, 15, 25, 38,42,45,50)
tm_shape(Allcadres_coord) + tm_polygons(col = "Core.HWF.Density.2013", palette = "RdYlGn", breaks = breaks,title = "Core HWF Density (2013)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "forestgreen", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "goldenrod1", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "forestgreen", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkolivegreen2", lwd = 12)+
tm_shape(conv_cpv)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')



breaks = c(0,7, 15, 25, 38,42,45,50)
tm_shape(Allcadres_coord) + tm_polygons(col = "Core.HWF.Density.2018", palette = "RdYlGn", breaks = breaks,title = "Core HWF Density (2018)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "forestgreen", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "goldenrod1", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "forestgreen", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkolivegreen2", lwd = 12)+
tm_shape(conv_cpv)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')



breaks = c(0,7, 15, 25, 38,42,45,50)
tm_shape(Allcadres_coord) + tm_polygons(col = "Core.HWF.Density.2022", palette = "RdYlGn", breaks = breaks,title = "Core HWF Density (2022)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "forestgreen", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "goldenrod1", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "forestgreen", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkolivegreen2", lwd = 12)+
tm_shape(conv_cpv)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')




#################



breaks = c(0,10, 25, 40, 55,70,85,100)
tm_shape(Allcadres_coord) + tm_polygons(col = "Total.workforce.Density.2013", palette = "RdYlGn", breaks = breaks,title = " Total workforce Density (2013)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )




breaks = c(0,10, 25, 40, 55,70,85,100)
tm_shape(Allcadres_coord) + tm_polygons(col = "Total.workforce.Density.2018", palette = "RdYlGn", breaks = breaks,title = " Total workforce Density (2018)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )



breaks = c(0,7, 15, 25, 38,42,45,50)
tm_shape(Allcadres_coord) + tm_polygons(col = "Other.worforce.Density.2018", palette = "RdYlGn", breaks = breaks,title = "Other workforce Density (2018)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )




breaks = c(0,7, 15, 25, 38,42,45,50)
tm_shape(Allcadres_coord) + tm_polygons(col = "Other.worforce.Density.2022", palette = "RdYlGn", breaks = breaks,title = "Other workforce Density (2022)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )



###########################

breaks = c(0,7, 15, 25, 38,42,45,50)
tm_shape(Allcadres_coord) + tm_polygons(col = "Other.worforce.Density.2013", palette = "RdYlGn", breaks = breaks,title = "Other workforce Density (2013)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )




breaks = c(0,10, 25, 40, 55,70,85,100)
tm_shape(Allcadres_coord) + tm_polygons(col = "Total.workforce.Density.2013", palette = "RdYlGn", breaks = breaks,title = " Total workforce Density (2013)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )




breaks = c(0,10, 25, 40, 55,70,85,100)
tm_shape(Allcadres_coord) + tm_polygons(col = "Total.workforce.Density.2018", palette = "RdYlGn", breaks = breaks,title = " Total workforce Density (2018)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )


breaks = c(0,10, 25, 40, 55,70,85,100)
tm_shape(Allcadres_coord) + tm_polygons(col = "Total.workforce.Density.2022", palette = "RdYlGn", breaks = breaks,title = " Total workforce Density (2022)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )






###########################

breaks = c(0,4, 7, 10, 15,20,25,30)
tm_shape(Nurses_coord) + tm_polygons(col = "Nurses.Density.2013", palette = "RdYlGn", breaks = breaks,title = "Nurses Density (2013)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )



breaks = c(0,4, 7, 10, 15,20,25,30)
tm_shape(Nurses_coord) + tm_polygons(col = "Nurses.Density.2018", palette = "RdYlGn", breaks = breaks,title = "Nurses Density (2018)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )


breaks = c(0,4, 7, 10, 15,20,25,30)
tm_shape(Nurses_coord) + tm_polygons(col = "Nurses.Density.2022", palette = "RdYlGn", breaks = breaks,title = "Nurses Density (2022)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )




###########################

breaks = c(0,0.4,0.7, 1,1.5,2,2.5,3)
tm_shape(Midwives_coord) + tm_polygons(col = "Midwives.Density.2013", palette = "RdYlGn", breaks = breaks,title = "Midwives Density (2013)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )


breaks = c(0,0.4,0.7, 1,1.5,2,2.5,3)
tm_shape(Midwives_coord) + tm_polygons(col = "Midwives.Density.2018", palette = "RdYlGn", breaks = breaks,title = "Midwives Density (2018)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )



breaks = c(0,0.4,0.7, 1,1.5,2,2.5,3)
tm_shape(Midwives_coord) + tm_polygons(col = "Midwives.Density.2022", palette = "RdYlGn", breaks = breaks,title = "Midwives Density (2022)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )






###########################

breaks = c(0,0.4,0.7, 1,1.5,2,2.5,3)
tm_shape(Dentists_coord) + tm_polygons(col = "Dentists.density.2013", palette = "RdYlGn", breaks = breaks,title = "Dentists Density (2013)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )



breaks = c(0,0.4,0.7, 1,1.5,2,2.5,3)
tm_shape(Dentists_coord) + tm_polygons(col = "Dentists.density.2018", palette = "RdYlGn", breaks = breaks,title = "Dentists Density (2018)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )


breaks = c(0,0.4,0.7, 1,1.5,2,2.5,3)
tm_shape(Dentists_coord) + tm_polygons(col = "Dentists.density.2022", palette = "RdYlGn", breaks = breaks,title = "Dentists Density (2022)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )



###########################

breaks = c(0,0.4,0.7, 1,1.5,2,2.5,3)
tm_shape(Pharmacists_coord) + tm_polygons(col = "Pharmacists.density.2013", palette = "RdYlGn", breaks = breaks,title = "Pharmacists Density (2013)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )



breaks = c(0,0.4,0.7, 1,1.5,2,2.5,3)
tm_shape(Pharmacists_coord) + tm_polygons(col = "Pharmacists.density.2018", palette = "RdYlGn", breaks = breaks,title = "Pharmacists Density (2018)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )



breaks = c(0,0.4,0.7, 1,1.5,2,2.5,3)
tm_shape(Pharmacists_coord) + tm_polygons(col = "Pharmacists.density.2022", palette = "RdYlGn", breaks = breaks,title = "Pharmacists Density (2022)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )





###########################

breaks = c(0,5, 10, 13, 18,23,28,33)
tm_shape(Nurses_Midwives_coord) + tm_polygons(col = "Nurses.and.midwives.density.2013", palette = "RdYlGn", breaks = breaks,title = "Nurses and Midwives Density (2013)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )


breaks = c(0,5, 10, 13, 18,23,28,33)
tm_shape(Nurses_Midwives_coord) + tm_polygons(col = "Nurses.and.midwives.density.2018", palette = "RdYlGn", breaks = breaks,title = "Nurses and Midwives Density (2018)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )


breaks = c(0,5, 10, 13, 18,23,28,33)
tm_shape(Nurses_Midwives_coord) + tm_polygons(col = "Nurses.and.midwives.density.2022", palette = "RdYlGn", breaks = breaks,title = "Nurses and Midwives Density (2022)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )





###########################

breaks = c(0,5, 10, 16, 22,27,31,33)
tm_shape(dataA_coord) + tm_polygons(col = "Number.of.cadres.reported.in.2013", palette = "RdYlGn", breaks = breaks,title = "Number of reported occupations (2013)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )



breaks = c(0,5, 10, 16, 22,27,31,33)
tm_shape(dataA_coord) + tm_polygons(col = "Number.of.cadres.reported.in.2018", palette = "RdYlGn", breaks = breaks,title = "Number of reported occupations (2018)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )



breaks = c(0,5, 10, 16, 22,27,31,33)
tm_shape(dataA_coord) + tm_polygons(col = "Number.of.cadres.reported.in.2022", palette = "RdYlGn", breaks = breaks,title = "Number of reported occupations (2022)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )



###########################

breaks = c(0,10,20,30,40,50,60,70,80,90,100)
tm_shape(dataA_coord) + tm_polygons(col = "Percentage.of.occupations.2013", palette = "RdYlGn", breaks = breaks,title = "% of reported occupations (2013)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )



breaks = c(0,10,20,30,40,50,60,70,80,90,100)
tm_shape(dataA_coord) + tm_polygons(col = "Percentage.of.occupations.2018", palette = "RdYlGn", breaks = breaks,title = "% of reported occupations (2018)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )


breaks = c(0,10,20,30,40,50,60,70,80,90,100)
tm_shape(dataA_coord) + tm_polygons(col = "Percentage.of.occupations.2022", palette = "RdYlGn", breaks = breaks,title = "% of reported occupations (2022)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_shape(Seychelles)+ tm_markers()


























 geom_polygon(data = vietnam, aes(x = long, y = lat, group = group), fill = "lightblue")


# Import the "raster" package
library(raster)

# Get map data for Vietnam from the online database called GADM
HN <- getData(name = "GADM", country = "Vietnam", level = 0)

# Plot the map of Vietnam
par(mar = rep(2, 4))
plot(HN, border = "black", axes = TRUE)

vietnam <- getData("GADM", country = "Vietnam", level = 2)
china <- getData("GADM", country = "China", level = 0)
laos <- getData("GADM", country = "Laos", level = 0)
cambodia <- getData("GADM", country = "Cambodia", level = 0)



# Create a custom map with multiple layers
ggplot() +
  geom_polygon(data = vietnam, aes(x = long, y = lat, group = group), fill = "lightblue") +
  geom_polygon(data = china, aes(x = long, y = lat, group = group), fill = "lightgreen") +
  geom_polygon(data = laos, aes(x = long, y = lat, group = group), fill = "lightyellow") +
  geom_polygon(data = cambodia, aes(x = long, y = lat, group = group), fill = "lightpink") +
  theme_void()



Seychelles
Mauritius
Cabo Verde
Comoros
São Tomé and Príncipe

STP=getData("GADM", country = "STP", level = 0)
Seychelles=getData("GADM", country = "Seychelles", level = 0)
Mauritius=getData("GADM", country = "Mauritius", level = 0)
Cabo Verde=getData("GADM", country = "Cabo Verde", level = 1)
Comoros=getData("GADM", country = "Comoros", level = 0)



plot(STP, border = "black", axes = TRUE, size=3)
plot(Seychelles, border = "black", axes = TRUE)
plot(Cabo Verde, border = "black", axes = TRUE)
plot(Mauritius, border = "black", axes = TRUE)
plot(Comoros, border = "black", axes = TRUE)




















































ggplot(doctors.exp.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Medical.Doctors.2013), color = "white")+
  scale_fill_viridis_c(option = "C")


ggplot(doctors.exp.map, aes(map_id = region, fill = Medical.Doctors.2013))+
  geom_map(map = doctors.exp.map,  color = "white")+
  expand_limits(x = doctors.exp.map$long, y = doctors.exp.map$lat)+
  scale_fill_viridis_c(option = "C")

+
geom_text_repel(aes(label = region, geometry = geometry),
                  stat = "sf_coordinates", size = 3) 








ggplot(doctors.exp.map, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = Medical.Doctors.2013))+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "right")+
geom_text_repel(aes(label = region, geometry = geometry),
                  stat = "sf_coordinates", size = 3) 



plot(wrld_simpl)
head(wrld_simpl)

plot(wrld_simpl, REGION==2)
















####converting the data to sf object


head(doctors.exp.map)

doctors_coord <- st_as_sf(doctors.exp.map, coords = c(1:2))

head(doctors_coord)

########

###mapping

########


######################
install.packages(spData)

library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)


library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package

tm_shape(doctors_coord) +
  tm_fill()

map_nz = tm_shape(doctors_coord) + tm_polygons()
class(map_nz)
ma1 = tm_shape(doctors_coord) + tm_fill(col = "red")
ma1












ggplot(africa_coord) +
  geom_sf(color = "white", aes(fill = "Medical.Doctors.2018")) +
  geom_text_repel(aes(label = region, geometry = geometry),
                  stat = "sf_coordinates", size = 3) +
  theme(legend.position = "none")