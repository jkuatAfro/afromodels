library(ggplot2)
library(dplyr)
require(maps)
require(viridis)
library(ggrepel)


library(sf)     # for vector data handling
library(raster) # for raster data handling
library(tmap) # static & interactive mapping
library(mapview) #interactive mapping
library(afrilearndata) # example spatial data for Africa
library(rgdal) #seems to be needed for mapview raster


#install.packages(c("sf", "terra", "stars"))
library(sf)
library(terra)
library(stars)


theme_set(theme_void())


#######loading data

###medical doctors
doctors1=read.table("G:/Brazzaville/HRH 24/Analytics/Mapping/doctors1.txt",header=T,sep='\t')
DNMs=read.table("G:/Brazzaville/HRH 24/Analytics/Mapping/DocNursMidwives.txt",header=T,sep='\t')
AllCadres=read.table("G:/Brazzaville/HRH 24/Analytics/Mapping/AllCadres.txt",header=T,sep='\t')
Nurses=read.table("G:/Brazzaville/HRH 24/Analytics/Mapping/Nurses.txt",header=T,sep='\t')
Midwives=read.table("G:/Brazzaville/HRH 24/Analytics/Mapping/Midwives.txt",header=T,sep='\t')
Dentists=read.table("H:/Brazzaville/HRH 24/Analytics/Mapping/Dentists.txt",header=T,sep='\t')
Pharmacists=read.table("H:/Brazzaville/HRH 24/Analytics/Mapping/Pharmacists.txt",header=T,sep='\t')
Nurses_Midwives=read.table("H:/Brazzaville/HRH 24/Analytics/Mapping/Nurses_Midwives.txt",header=T,sep='\t')
dataA=read.table("H:/Brazzaville/HRH 24/Analytics/Mapping/data_avail.txt",header=T,sep='\t')
migration=read.table("H:/Brazzaville/HRH 24/Analytics/Mapping/migration.txt",header=T,sep='\t')

##
needs=read.table("H:/Brazzaville/HRH 24/Analytics/Mapping/needs.txt",header=T,sep='\t')


head(doctors1)

###merge the data

#doctors.exp.map <- left_join(africa_coord,doctors, by = "region")
africa_coord_doctors1<- left_join(africountries,doctors1, by = "iso_a3")
DNMs_coord<- left_join(africountries,DNMs, by = "iso_a3")
Allcadres_coord<- left_join(africountries,AllCadres, by = "iso_a3")
Nurses_coord<- left_join(africountries,Nurses, by = "iso_a3")
Midwives_coord<- left_join(africountries,Midwives, by = "iso_a3")
Dentists_coord<- left_join(africountries,Dentists, by = "iso_a3")
Pharmacists_coord<- left_join(africountries,Pharmacists, by = "iso_a3")
Nurses_Midwives_coord<- left_join(africountries,Nurses_Midwives, by = "iso_a3")
dataA_coord<- left_join(africountries,dataA, by = "iso_a3")
migration_coord<- left_join(africountries,migration, by = "iso_a3")

needs_coord<- left_join(africountries,needs, by = "iso_a3")





################

#####to change polygon to raster using terra
require(raster)
require(fasterize)
library(terra)

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



###############################################

##### Medical doctors

###################################################


breaks = c(0,1, 2.5, 4, 5.5,8,11,13,15)
tm_shape(africa_coord_doctors1,bbox = sf::st_bbox(c(xmin = -27, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Medical.Doctors.2022", palette = "RdYlGn", breaks = breaks,title = "Doctors density per 10,000 (2022)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 21, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "green4", lwd = 12)+
  tm_shape(conv_cpv)+ 
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')
##BrBG



breaks = c(0,1, 2.5, 4, 5.5,8,11,13,15)
tm_shape(africa_coord_doctors1,bbox = sf::st_bbox(c(xmin = -25, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Medical.Doctors.2018", palette = "RdYlGn", breaks = breaks,title = "Doctors density per 10,000 (2018)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 21, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "green4", lwd = 12)+
  tm_shape(conv_cpv)+ 
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkgoldenrod2", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod2", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')




breaks = c(0,1, 2.5, 4, 5.5,8,11,13,15)
tm_shape(africa_coord_doctors1,bbox = sf::st_bbox(c(xmin = -25, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Medical.Doctors.2013", palette = "RdYlGn", breaks = breaks,title = "Doctors density per 10,000 (2013)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 21, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "green3", lwd = 12)+
  tm_shape(conv_cpv)+ 
  tm_borders(col = "green2", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkgoldenrod3", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod3", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')



#####################################

####Nurses

##########################




###########################

breaks = c(0,4, 7, 10, 15,20,25,30)
tm_shape(Nurses_coord,bbox = sf::st_bbox(c(xmin = -25, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Nurses.Density.2013", palette = "RdYlGn", breaks = breaks,title = "Nurses Density (2013)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 21, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "green4", lwd = 12)+
  tm_shape(conv_cpv)+ 
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')



breaks = c(0,4, 7, 10, 15,20,25,30)
tm_shape(Nurses_coord,bbox = sf::st_bbox(c(xmin = -25, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Nurses.Density.2018", palette = "RdYlGn", breaks = breaks,title = "Nurses Density (2018)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 21, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "green4", lwd = 12)+
  tm_shape(conv_cpv)+ 
  tm_borders(col = "yellow", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "green2", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')


breaks = c(0,4, 7, 10, 15,20,25,30)
tm_shape(Nurses_coord,bbox = sf::st_bbox(c(xmin = -26, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Nurses.Density.2022", palette = "RdYlGn", breaks = breaks,title = "Nurses Density (2022)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 21, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "green4", lwd = 12)+
  tm_shape(conv_cpv)+ 
  tm_borders(col = "yellowgreen", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')




###########################


#####Dentists

###########################

breaks = c(0,0.4,0.7, 1,1.5,2,2.5,3)
tm_shape(Dentists_coord,bbox = sf::st_bbox(c(xmin = -26, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Dentists.density.2013", palette = "RdYlGn", breaks = breaks,title = "Dentists Density (2013)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 21, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "green2", lwd = 12)+
  tm_shape(conv_cpv)+ 
  tm_borders(col = "brown", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod3", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')




breaks = c(0,0.4,0.7, 1,1.5,2,2.5,3)
tm_shape(Dentists_coord,bbox = sf::st_bbox(c(xmin = -26, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Dentists.density.2018", palette = "RdYlGn", breaks = breaks,title = "Dentists Density (2018)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 21, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "green4", lwd = 12)+
  tm_shape(conv_cpv)+ 
  tm_borders(col = "green2", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkgoldenrod2", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')



breaks = c(0,0.4,0.7, 1,1.5,2,2.5,3)
tm_shape(Dentists_coord,bbox = sf::st_bbox(c(xmin = -26, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Dentists.density.2022", palette = "RdYlGn", breaks = breaks,title = "Dentists Density (2022)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 21, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "green4", lwd = 12)+
  tm_shape(conv_cpv)+ 
  tm_borders(col = "green2", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkgoldenrod2", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')




###########################



###########################

breaks = c(0,0.4,0.7, 1,1.5,2,2.5,3)
tm_shape(Pharmacists_coord,bbox = sf::st_bbox(c(xmin = -26, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Pharmacists.density.2013", palette = "RdYlGn", breaks = breaks,title = "Pharmacists Density (2013)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 21, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
  tm_shape(conv_cpv)+ 
  tm_borders(col = "brown", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod3", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')




breaks = c(0,0.4,0.7, 1,1.5,2,2.5,3)
tm_shape(Pharmacists_coord,bbox = sf::st_bbox(c(xmin = -26, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Pharmacists.density.2018", palette = "RdYlGn", breaks = breaks,title = "Pharmacists Density (2018)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 21, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "green4", lwd = 12)+
  tm_shape(conv_cpv)+ 
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')




breaks = c(0,0.4,0.7, 1,1.5,2,2.5,3)
tm_shape(Pharmacists_coord,bbox = sf::st_bbox(c(xmin = -26, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Pharmacists.density.2022", palette = "RdYlGn", breaks = breaks,title = "Pharmacists Density (2022)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 21, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "green4", lwd = 12)+
  tm_shape(conv_cpv)+ 
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod2", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')




###########################

breaks = c(0,5, 10, 13, 18,23,28,33)
tm_shape(Nurses_Midwives_coord,bbox = sf::st_bbox(c(xmin = -27, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Nurses.and.midwives.density.2013", palette = "RdYlGn", breaks = breaks,title = "Nurses and Midwives Density (2013)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 21, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "green4", lwd = 12)+
  tm_shape(conv_cpv)+ 
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod2", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')



breaks = c(0,5, 10, 13, 18,23,28,33)
tm_shape(Nurses_Midwives_coord,bbox = sf::st_bbox(c(xmin = -26, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Nurses.and.midwives.density.2018", palette = "RdYlGn", breaks = breaks,title = "Nurses and Midwives Density (2018)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 21, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "green4", lwd = 12)+
  tm_shape(conv_cpv)+ 
  tm_borders(col = "yellowgreen", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "yellowgreen", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')



breaks = c(0,5, 10, 13, 18,23,28,33)
tm_shape(Nurses_Midwives_coord, bbox = sf::st_bbox(c(xmin = -26, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Nurses.and.midwives.density.2022", palette = "RdYlGn", breaks = breaks,title = "Nurses and Midwives Density (2022)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 21, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "green4", lwd = 12)+
  tm_shape(conv_cpv)+ 
  tm_borders(col = "yellowgreen", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "yellowgreen", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')





###########################


####
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
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')



breaks = c(0,7, 15, 25, 38,42,45,50)
tm_shape(DNMs_coord,bbox = sf::st_bbox(c(xmin = -27, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Density.2022", palette = "RdYlGn", breaks = breaks,title = "Doctors, Nurses and Midwives (2022)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "forestgreen", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "forestgreen", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')

####################################


################




################


###SDG 3 occupations


breaks = c(0,7, 15, 25, 38,42,45,50)
tm_shape(Allcadres_coord,bbox = sf::st_bbox(c(xmin = -27, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Core.HWF.Density.2013", palette = "RdYlGn", breaks = breaks,title = "SDG 3.c occupations Density, 2013", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "forestgreen", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "forestgreen", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "brown", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')




breaks = c(0,7, 15, 25, 38,42,45,50)
tm_shape(Allcadres_coord,bbox = sf::st_bbox(c(xmin = -27, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Core.HWF.Density.2018", palette = "RdYlGn", breaks = breaks,title = "SDG 3.c occupations Density, 2018", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "forestgreen", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "forestgreen", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')




breaks = c(0,7, 15, 25, 38,42,45,50)
tm_shape(Allcadres_coord,bbox = sf::st_bbox(c(xmin = -27, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Core.HWF.Density.2022", palette = "RdYlGn", breaks = breaks,title = "SDG 3.c occupations Density, 2022", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "forestgreen", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "forestgreen", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')










#####All workforce

#########################################



breaks = c(0,10, 25, 40, 55,70,85,100)
tm_shape(Allcadres_coord, bbox = sf::st_bbox(c(xmin = -27, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Total.workforce.Density.2013", palette = "RdYlGn", breaks = breaks,title = " Total workforce Density (2013)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "yellowgreen", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "yellowgreen", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod3", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')






breaks = c(0,10, 25, 40, 55,70,85,100)
tm_shape(Allcadres_coord, bbox = sf::st_bbox(c(xmin = -27, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Total.workforce.Density.2018", palette = "RdYlGn", breaks = breaks,title = " Total workforce Density (2018)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "green4", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "yellowgreen", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod2", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')




breaks = c(0,10, 25, 40, 55,70,85,100)
tm_shape(Allcadres_coord, bbox = sf::st_bbox(c(xmin = -27, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Total.workforce.Density.2022", palette = "RdYlGn", breaks = breaks,title = " Total workforce Density (2022)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "green4", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "yellowgreen", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod2", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')




###########################

breaks = c(0,5, 10, 16, 22,27,31,33)
tm_shape(dataA_coord, bbox = sf::st_bbox(c(xmin = -27, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Number.of.cadres.reported.in.2013", palette = "RdYlGn", breaks = breaks,title = "Number of reported occupations (2013)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "indianred", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "indianred", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "indianred", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkgoldenrod2", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod2", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')



breaks = c(0,5, 10, 16, 22,27,31,33)
tm_shape(dataA_coord, bbox = sf::st_bbox(c(xmin = -27, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Number.of.cadres.reported.in.2018", palette = "RdYlGn", breaks = breaks,title = "Number of reported occupations (2018)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "yellowgreen", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkgoldenrod2", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod2", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')



breaks = c(0,5, 10, 16, 22,27,31,33)
tm_shape(dataA_coord, bbox = sf::st_bbox(c(xmin = -27, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Number.of.cadres.reported.in.2022", palette = "RdYlGn", breaks = breaks,title = "Number of reported occupations (2022)", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "green4", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkgoldenrod2", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod2", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')



###########################














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


#####mmigration


breaks = c(0,0.01,0.03, 0.04)
tm_shape(migration_coord, bbox = sf::st_bbox(c(xmin = -27, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Doctors.Migration.to.Stock.ratio", palette = "RdYlGn", breaks = breaks,title = "Doctors stock to migrated")", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "forestgreen", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "forestgreen", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "green3", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')



################################################

breaks = c(0,0.01,0.02,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
tm_shape(migration_coord,bbox = sf::st_bbox(c(xmin = -27, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Doctors.Migration.to.Stock.ratio", palette = "YlOrRd", breaks = breaks,title = "Doctors stock to migrated ratio", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "khaki1", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = " lightsalmon", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "khaki1", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')





################################################

breaks = c(0,0.01,0.02,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
tm_shape(migration_coord,bbox = sf::st_bbox(c(xmin = -27, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Nurses.Migration.to.Stock.ratio", palette = "YlOrRd", breaks = breaks,title = "Nurses stock to migrated ratio", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 1, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "khaki1", lwd = 12)+
  tm_shape(conv_cpv)+
  tm_borders(col = "lightsalmon4", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "lightsalmon4", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "lightyellow1", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')





#######################################



breaks = c(0,20,40,60,80,100)
tm_shape(needs_coord,bbox = sf::st_bbox(c(xmin = -27, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "HWF.Need.Availability.Ratio", palette = "RdYlGn", breaks = breaks,title = " HWF Need Availability Ratio", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 21, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "green4", lwd = 12)+
  tm_shape(conv_cpv)+ 
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "yellow", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "yellow", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "darkgoldenrod1", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')



Needed.Density.2022




breaks = c(0,25,50,75,100)

tm_shape(needs_coord,bbox = sf::st_bbox(c(xmin = -27, xmax = 60, ymin = -40, ymax = 40))) + tm_polygons(col = "Needed.Density.2022", palette = "Greens", breaks = breaks,title = "Needed Density", textNA = "Not in AFRO")+
 tm_text("iso_a3", auto.placement=TRUE, remove.overlap=FALSE, just='centre', col='black', size=0.7 )+
tm_dots(size = 0.01, col = "black", shape = 21, alpha = 0.03,title = "region",id="region")+
  tm_shape(conv_syc)+ 
  tm_borders(col = "green4", lwd = 12)+
  tm_shape(conv_cpv)+ 
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_mus)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_stp)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(conv_com)+
  tm_borders(col = "green4", lwd = 12)+
tm_shape(dat_sf) +
    tm_text("region", size =0.7, root = 2,  just='right')

