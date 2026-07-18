library(tidyr)
recruitment_info <- data.frame(
  Centre = c("CentreA", "CentreB", "CentreC"),
  Lat = c(51.51770, 52.48947, 51.45451),
  Long = c(-0.100400, -1.898575, -2.587910),
  GroupA = c(907, 1910, 4419),
  GroupB = c(47, 116, 277),
  stringsAsFactors = FALSE
)

recruitment_info <- recruitment_info %>%
  gather(Group, values, -Centre, -Lat, -Long)


library(tmap)
library(raster)
jnk <- getData("GADM",country="IND",level=1)
tm_shape(jnk) + tm_polygons("NAME_1", legend.show = F) +
  tm_text("NAME_1", size = 1/2)



AFRO<-getData("GADM",country="Africa",level=1)


AFRO=c(BFA,
CAF
,
STP
,
COD
,
ETH
,
NGA
,
GAB
,
MRT
,
SWZ
,
LSO
,
DZA
,
AGO
,
BEN
,
BWA
,
BDI
,
CIV
,
CPV
,
CMR
,
TCD
,
GNQ
,
GMB
,
GHA
,
GIN
,
GNB
,
KEN
,
LBR
,
MDG
,
MWI
,
MLI
,
MUS
,
MOZ
,
NAM
,
RWA
,
SEN
,
SYC
,
SLE
,
ZAF
,
SSD
,
COG
,
NER
,
UGA
,
ZMB
,
ZWE
,
ERI
,
TGO
,
COM
,
TZA)

