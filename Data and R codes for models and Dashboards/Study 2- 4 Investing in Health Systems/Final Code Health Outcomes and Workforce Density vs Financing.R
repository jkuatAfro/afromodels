# Load the necessary packages
library(pacman)
pacman::p_load(shiny, leaflet, plotly, DT, rnaturalearth, 
               rnaturalearthdata, sf, shinythemes, shinydashboard,stats, 
               tidyverse, nloptr, plm, segmented,foreign,semPlot,
               MASS,lme4, sandwich,lmtest,dLagM, mgcv,  gratia, pracma)


# Reading in the Data into R
HF_HWF_Data<-read.csv("D:\\Research Collaborations\\WHO AFRO\\Data Assimilation\\Health Workforce Modelling\\Data synthesis_WHO_AFRO.csv",header=TRUE)
str(HF_HWF_Data)

CHE<-HF_HWF_Data$CHE.per.capita
GGHED<-HF_HWF_Data$GGHED.per.capita
GDP<-HF_HWF_Data$GDP.per.capita
Country<-HF_HWF_Data$Country
Year<-HF_HWF_Data$Year

GGHED_GDP<-HF_HWF_Data$GGHED.to.GDP.Ratio
GGHED_CHE<-HF_HWF_Data$GGHED.to.CHE.Ratio

PVTD<-HF_HWF_Data$PVTD.per.capita
PVTD_CHE<-HF_HWF_Data$PVTD.to.CHE.Ratio

EXT<-HF_HWF_Data$EXT.per.capita
EXT_CHE<-HF_HWF_Data$EXT.to.CHE.Ratio

OOP<-HF_HWF_Data$OOP.per.capita
OOP_CHE<-HF_HWF_Data$OOP.to.CHE.Ratio

Doctors<-HF_HWF_Data$Doctors
NursingMidwifery<-HF_HWF_Data$Nursing_Midwifery
Pharmacists<-HF_HWF_Data$Pharmacists
Dentists<-HF_HWF_Data$Dentists                

HF_HWF_Data$HWF<-Doctors+NursingMidwifery+Pharmacists+Dentists
HWF<-HF_HWF_Data$HWF
IMR<-HF_HWF_Data$IMR
MMR<-HF_HWF_Data$MMR
LE<-HF_HWF_Data$LE
UHC_SCI<-HF_HWF_Data$UHC_SCI/100
PopDensity<-HF_HWF_Data$Population.Density 


# Correlations between CHE and Key Health Outcomes
cor(na.omit(data.frame(CHE,UHC_SCI)))
cor(na.omit(data.frame(CHE,LE)))
cor(na.omit(data.frame(CHE,MMR)))
cor(na.omit(data.frame(CHE,IMR)))
cor(na.omit(data.frame(CHE,GDP)))

cor(na.omit(data.frame(CHE,HWF)))

HF<-data.frame(CHE,EXT,GGHED,PVTD,LE,MMR,IMR,UHC_SCI,Country,Year)
panel_data<-pdata.frame(HF, index=c("Country","Year"))





#######################   MODELLING NON-LINEAR RELATIONSHIP BETWEEN CHE AND HEALTH OUTCOMES ###########

# Visual Statistic
par(mfrow=c(2,2))
plot(HF$CHE, HF$LE, xlab="Current Health Expenditure (CHE)",ylab="Life Expectancy(LE)", main="CHE vs LE",col="red")
plot(HF$CHE, HF$UHC_SCI, xlab="Current Health Expenditure (CHE)",ylab="Universal Service Coverage (UHC)", main="CHE vs UHC_SCI",col="blue")
plot(HF$CHE, HF$MMR, xlab="Current Health Expenditure (CHE)",ylab="Maternal Mortality Ratio (MMR)", main="CHE vs MMR",col="red")
plot(HF$CHE, HF$IMR, xlab="Current Health Expenditure (CHE)",ylab="Infant Mortality Ratio (IMR)", main="CHE vs IMR",col="blue")



# Fitting nonlinear models (GAM)
gam_le <- gam(LE ~ s(EXT) + s(GGHED) + s(PVTD), data = HF)
summary(gam_le)

gam_mmr <- gam(MMR ~ s(EXT) + s(GGHED) + s(PVTD), data = HF)
summary(gam_mmr)

gam_imr <- gam(IMR ~ s(EXT) + s(GGHED) + s(PVTD), data = HF)
summary(gam_imr)

gam_uhc <- gam(UHC_SCI ~ s(EXT) + s(GGHED) + s(PVTD), data = HF)
summary(gam_uhc)




# Setting the constraints
m.CHE <- mean(na.omit(HF_HWF_Data$CHE.per.capita))

# Initial guess: equal shares
init <- c(1/3, 1/3, 1/3)

# Defining the optimization function

# 1. Life Expectancy (Objective: Maximize LE)
eval_le <- function(x) {
  EXT <- x[1]*m.CHE
  GGHED <- x[2]*m.CHE
  PVTD <- x[3]*m.CHE
  pred <- predict(gam_le, newdata = data.frame(EXT=EXT, GGHED=GGHED, PVTD=PVTD))
  return(-pred) # maximize
}

eval_le_eq <- function(x) sum(x) - 1

result_le <- nloptr(x0 = init,
                 eval_f = eval_le,
                 eval_g_eq = eval_le_eq,
                 lb = c(0,0,0),
                 ub = c(1,1,1),
                 opts = list(algorithm="NLOPT_LN_COBYLA", maxeval=1000))

optimal_shares_le <- result_le$solution
names(optimal_shares_le) <- c("EXT_share", "GGHED_share", "PVTD_share")
optimal_shares_le*100


# 2. UHC_SCI (Objective: Maximize UHC_SCI)
eval_uhc <- function(x) {
  EXT <- x[1]*m.CHE
  GGHED <- x[2]*m.CHE
  PVTD <- x[3]*m.CHE
  pred <- predict(gam_uhc, newdata = data.frame(EXT=EXT, GGHED=GGHED, PVTD=PVTD))
  return(-pred) # maximize
}

eval_uhc_eq <- function(x) sum(x) - 1

result_uhc <- nloptr(x0 = init,
                    eval_f = eval_uhc,
                    eval_g_eq = eval_uhc_eq,
                    lb = c(0,0,0),
                    ub = c(1,1,1),
                    opts = list(algorithm="NLOPT_LN_COBYLA", maxeval=1000))

optimal_shares_uhc <- result_uhc$solution
names(optimal_shares_uhc) <- c("EXT_share", "GGHED_share", "PVTD_share")
optimal_shares_uhc*100


# 3.  (Objective: Minimize MMR)
eval_mmr <- function(x) {
  EXT <- x[1]*m.CHE
  GGHED <- x[2]*m.CHE
  PVTD <- x[3]*m.CHE
  pred <- predict(gam_mmr, newdata = data.frame(EXT=EXT, GGHED=GGHED, PVTD=PVTD))
  return(pred) # maximize
}

eval_mmr_eq <- function(x) sum(x) - 1

result_mmr <- nloptr(x0 = init,
                     eval_f = eval_mmr,
                     eval_g_eq = eval_mmr_eq,
                     lb = c(0,0,0),
                     ub = c(1,1,1),
                     opts = list(algorithm="NLOPT_LN_COBYLA", maxeval=1000))

optimal_shares_mmr <- result_mmr$solution
names(optimal_shares_mmr) <- c("EXT_share", "GGHED_share", "PVTD_share")
optimal_shares_mmr*100


# 4.  (Objective: Minimize IMR)
eval_imr <- function(x) {
  EXT <- x[1]*m.CHE
  GGHED <- x[2]*m.CHE
  PVTD <- x[3]*m.CHE
  pred <- predict(gam_imr, newdata = data.frame(EXT=EXT, GGHED=GGHED, PVTD=PVTD))
  return(pred) # maximize
}

eval_imr_eq <- function(x) sum(x) - 1

result_imr <- nloptr(x0 = init,
                     eval_f = eval_imr,
                     eval_g_eq = eval_imr_eq,
                     lb = c(0,0,0),
                     ub = c(1,1,1),
                     opts = list(algorithm="NLOPT_LN_COBYLA", maxeval=1000))

optimal_shares_imr <- result_imr$solution
names(optimal_shares_imr) <- c("EXT_share", "GGHED_share", "PVTD_share")
optimal_shares_imr*100



# Optimal Allocations

optimal_EXT<-100*0.25*round((optimal_shares_imr[1]+optimal_shares_mmr[1]+optimal_shares_uhc[1]+optimal_shares_le[1]),2)
optimal_GGHED<-100*0.25*round((optimal_shares_imr[2]+optimal_shares_mmr[2]+optimal_shares_uhc[2]+optimal_shares_le[2]),2)
optimal_PVTD<-100*0.25*round((optimal_shares_imr[3]+optimal_shares_mmr[3]+optimal_shares_uhc[3]+optimal_shares_le[3]),2)


# Effect of GGHED>67%

library(dplyr)

HF_GGHED<-na.omit(data.frame(EXT_CHE,GGHED_CHE,PVTD_CHE,LE,MMR,IMR,UHC_SCI))


HF_GGHED %>%
  mutate(GGHED_group = if_else(HF_GGHED$GGHED_CHE>= 67, "GGHED >= 67%", "GGHED < 67%")) %>%
  group_by(GGHED_group) %>%
  summarise(mean_UHC = mean(UHC_SCI), mean_LE = mean(LE),mean_MMR=mean(MMR),mean_IMR=mean(IMR))



















#######################   MODELLING NON-LINEAR RELATIONSHIP BETWEEN WORKFORCE DENSITIES AND HEALTH OUTCOMES ###########


# Loading the necessary packages.

library(pacman)
pacman::p_load(plm,segmented,foreign,semPlot,MASS,dplyr,lme4, 
               sandwich,lmtest,dLagM, mgcv, nloptr, gratia)


#Extracting the data

Country<-HF_HWF_Data$Country
Year<-HF_HWF_Data$Year
Doctors<-HF_HWF_Data$Doctors
NursingMidwifery<-HF_HWF_Data$Nursing_Midwifery
Pharmacists<-HF_HWF_Data$Pharmacists
Dentists<-HF_HWF_Data$Dentists
CHE<-HF_HWF_Data$CHE.per.capita
GGHED<-HF_HWF_Data$GGHED.per.capita
GGHED_GDP<-HF_HWF_Data$GGHED.to.GDP.Ratio
GGHED_CHE<-HF_HWF_Data$GGHED.to.CHE.Ratio

PVTD<-HF_HWF_Data$PVTD.per.capita
PVTD_CHE<-HF_HWF_Data$PVTD.to.CHE.Ratio

EXT<-HF_HWF_Data$EXT.per.capita
EXT_CHE<-HF_HWF_Data$EXT.to.CHE.Ratio

OOP<-HF_HWF_Data$OOP.per.capita
OOP_CHE<-HF_HWF_Data$OOP.to.CHE.Ratio

HF_HWF_Data$HWF<-Doctors+NursingMidwifery+Pharmacists+Dentists
HWF<-HF_HWF_Data$HWF
IMR<-HF_HWF_Data$IMR
MMR<-HF_HWF_Data$MMR
LE<-HF_HWF_Data$LE
UHC_SCI<-HF_HWF_Data$UHC_SCI
PopDensity<-HF_HWF_Data$Population.Density


attach(HF_HWF_Data)

# Correlations
cor(na.omit(data.frame(HWF,UHC_SCI)))
cor(na.omit(data.frame(HWF,LE)))
cor(na.omit(data.frame(HWF,MMR)))
cor(na.omit(data.frame(HWF,IMR)))


################## Effect of HWF on Key Health Outcomes #################

library(dplyr)

HF_UHC<-na.omit(data.frame(HWF,LE,MMR,IMR,UHC_SCI,Doctors,NursingMidwifery, Dentists, Pharmacists ))

# Descriptive  Analysis

HF_UHC %>%
  mutate(HWF_group = if_else(HF_UHC$HWF>=44.61, "HWF >= 44.61 workers", "HWF< 44.61 workers")) %>%
  group_by(HWF_group) %>%
  summarise(mean_UHC = mean(UHC_SCI), mean_LE = mean(LE),mean_MMR=mean(MMR),mean_IMR=mean(IMR))




# Optimization Problem

HWF_Optim<-na.omit(data.frame(Doctors,NursingMidwifery, Dentists, Pharmacists, LE, MMR, IMR, UHC_SCI))


library(mgcv)

######################## OPTIMAL NUMBER OF DOCTORS ########################

# 1. Life Expectancy (LE)

gam_le_doc <- gam(LE ~ s(Doctors), data = HF_HWF_Data)
doctor_seq <- seq(min(na.omit(HF_HWF_Data$Doctors)), max(na.omit(HF_HWF_Data$Doctors)), length.out = 200)
pred <- predict(gam_le_doc, newdata = data.frame(Doctors = doctor_seq), type = "response")
slope <- diff(pred) / diff(doctor_seq)
opt_index <- which.min(abs(slope))
opt_doctors.le <- doctor_seq[opt_index]
opt_doctors.le


# 2. Maternal Mortality (MMR)
gam_mmr_doc <- gam(MMR ~ s(Doctors), data = HF_HWF_Data)
pred <- predict(gam_mmr_doc, newdata = data.frame(Doctors = doctor_seq), type = "response")
slope <- diff(pred) / diff(doctor_seq)
opt_index <- which.min(abs(slope))
opt_doctors.mmr <- doctor_seq[opt_index]
opt_doctors.mmr


# 3. Infant Mortality (IMR)
gam_imr_doc <- gam(IMR ~ s(Doctors), data = HF_HWF_Data)
pred <- predict(gam_imr_doc, newdata = data.frame(Doctors = doctor_seq), type = "response")
slope <- diff(pred) / diff(doctor_seq)
opt_index <- which.min(abs(slope))
opt_doctors.imr <- doctor_seq[opt_index]
opt_doctors.imr

# 4. Universal Health Coverage (UHC)
gam_uhc_doc <- gam(UHC_SCI ~ s(Doctors), data = HF_HWF_Data)
pred <- predict(gam_uhc_doc, newdata = data.frame(Doctors = doctor_seq), type = "response")
slope <- diff(pred) / diff(doctor_seq)
opt_index <- which.min(abs(slope))
opt_doctors.uhc <- doctor_seq[opt_index]
opt_doctors.uhc

opt_doctors<-0.25*(opt_doctors.imr+opt_doctors.le+opt_doctors.mmr+opt_doctors.uhc)
opt_doctors



######################## OPTIMAL NUMBER OF NURSES & MIDWIVES  ########################

# 1. Life Expectancy (LE)

gam_le_nurse <- gam(LE ~ s(Nursing_Midwifery), data = HF_HWF_Data)
nurse_seq <- seq(min(na.omit(HF_HWF_Data$Nursing_Midwifery)), max(na.omit(HF_HWF_Data$Nursing_Midwifery)), length.out = 200)
pred <- predict(gam_le_nurse, newdata = data.frame(Nursing_Midwifery = nurse_seq), type = "response")
slope <- diff(pred) / diff(nurse_seq)
opt_index <- which.min(abs(slope))
opt_nurses.le <- nurse_seq[opt_index]
opt_nurses.le


# 2. Maternal Mortality (MMR)
gam_mmr_nurse <- gam(MMR ~ s(Nursing_Midwifery), data = HF_HWF_Data)
pred <- predict(gam_mmr_nurse, newdata = data.frame(Nursing_Midwifery = nurse_seq), type = "response")
slope <- diff(pred) / diff(nurse_seq)
opt_index <- which.min(abs(slope))
opt_nurses.mmr <- nurse_seq[opt_index]
opt_nurses.mmr


# 3. Infant Mortality (IMR)
gam_imr_nurse <- gam(IMR ~ s(Nursing_Midwifery), data = HF_HWF_Data)
pred <- predict(gam_imr_nurse, newdata = data.frame(Nursing_Midwifery = nurse_seq), type = "response")
slope <- diff(pred) / diff(nurse_seq)
opt_index <- which.min(abs(slope))
opt_nurses.imr <- nurse_seq[opt_index]
opt_nurses.imr


# 4. Universal Health Coverage (UHC)
gam_uhc_nurse <- gam(UHC_SCI ~ s(Nursing_Midwifery), data = HF_HWF_Data)
pred <- predict(gam_uhc_nurse, newdata = data.frame(Nursing_Midwifery = nurse_seq), type = "response")
slope <- diff(pred) / diff(nurse_seq)
opt_index <- which.min(abs(slope))
opt_nurses.uhc <- nurse_seq[opt_index]
opt_nurses.uhc

opt_nurses<-0.25*(opt_nurses.imr+opt_nurses.le+opt_nurses.mmr+opt_nurses.uhc)
opt_nurses


######################## OPTIMAL NUMBER OF DENTISTS ########################

# 1. Life Expectancy (LE)

gam_le_den <- gam(LE ~ s(Dentists), data = HF_HWF_Data)
dentists_seq <- seq(min(na.omit(HF_HWF_Data$Dentists)), max(na.omit(HF_HWF_Data$Dentists)), length.out = 200)
pred <- predict(gam_le_den, newdata = data.frame(Dentists = dentists_seq), type = "response")
slope <- diff(pred) / diff(dentists_seq)
opt_index <- which.min(abs(slope))
opt_dentists.le <- dentists_seq[opt_index]
opt_dentists.le


# 2. Maternal Mortality (MMR)
gam_mmr_den <- gam(MMR ~ s(Dentists), data = HF_HWF_Data)
pred <- predict(gam_mmr_den, newdata = data.frame(Dentists = dentists_seq), type = "response")
slope <- diff(pred) / diff(dentists_seq)
opt_index <- which.min(abs(slope))
opt_dentists.mmr <- dentists_seq[opt_index]
opt_dentists.mmr


# 3. Infant Mortality (IMR)
gam_imr_den <- gam(IMR ~ s(Dentists), data = HF_HWF_Data)
pred <- predict(gam_imr_den, newdata = data.frame(Dentists = dentists_seq), type = "response")
slope <- diff(pred) / diff(dentists_seq)
opt_index <- which.min(abs(slope))
opt_dentists.imr <- dentists_seq[opt_index]
opt_dentists.imr


# 4. Universal Health Coverage (UHC)
gam_uhc_den <- gam(UHC_SCI ~ s(Dentists), data = HF_HWF_Data)
pred <- predict(gam_uhc_den, newdata = data.frame(Dentists = dentists_seq), type = "response")
slope <- diff(pred) / diff(dentists_seq)
opt_index <- which.min(abs(slope))
opt_dentists.uhc <- dentists_seq[opt_index]
opt_dentists.uhc

opt_dentists<-0.25*(opt_dentists.imr+opt_dentists.le+opt_dentists.mmr+opt_dentists.uhc)
opt_dentists

######################## OPTIMAL NUMBER OF PHARMACISTS ########################

# 1. Life Expectancy (LE)

gam_le_pharma <- gam(LE ~ s(Pharmacists), data = HF_HWF_Data)
pharmacists_seq <- seq(min(na.omit(HF_HWF_Data$Pharmacists)), max(na.omit(HF_HWF_Data$Pharmacists)), length.out = 200)
pred <- predict(gam_le_pharma, newdata = data.frame(Pharmacists = pharmacists_seq), type = "response")
slope <- diff(pred) / diff(pharmacists_seq)
opt_index <- which.min(abs(slope))
opt_pharmacists.le <- pharmacists_seq[opt_index]
opt_pharmacists.le


# 2. Maternal Mortality (MMR)
gam_mmr_pharma <- gam(MMR ~ s(Pharmacists), data = HF_HWF_Data)
pred <- predict(gam_mmr_pharma, newdata = data.frame(Pharmacists = pharmacists_seq), type = "response")
slope <- diff(pred) / diff(pharmacists_seq)
opt_index <- which.min(abs(slope))
opt_pharmacists.mmr <- pharmacists_seq[opt_index]
opt_pharmacists.mmr


# 3. Infant Mortality (IMR)
gam_imr_pharma <- gam(IMR ~ s(Pharmacists), data = HF_HWF_Data)
pred <- predict(gam_imr_pharma, newdata = data.frame(Pharmacists = pharmacists_seq), type = "response")
slope <- diff(pred) / diff(pharmacists_seq)
opt_index <- which.min(abs(slope))
opt_pharmacists.imr <- pharmacists_seq[opt_index]
opt_pharmacists.imr


# 4. Universal Health Coverage (UHC)
gam_uhc_pharma <- gam(UHC_SCI ~ s(Pharmacists), data = HF_HWF_Data)
pred <- predict(gam_uhc_pharma, newdata = data.frame(Pharmacists = pharmacists_seq), type = "response")
slope <- diff(pred) / diff(pharmacists_seq)
opt_index <- which.min(abs(slope))
opt_pharmacists.uhc <- pharmacists_seq[opt_index]
opt_pharmacists.uhc

opt_pharmacists<-0.25*(opt_pharmacists.imr+opt_pharmacists.le+opt_pharmacists.mmr+opt_pharmacists.uhc)
opt_pharmacists


# SENSITIVITY ANALYSIS OF HWF ON KEY HEALTH OUTCOMES

# Doctors <0.74, 0.74-3.09, 3.09-11.96, 11.96-21.7, 21.7-30, >30
HF_UHC %>%
  mutate(HWF_group = if_else(HF_UHC$Doctors<0.74, "Doctors <0.74", 
                             if_else(HF_UHC$Doctors>=0.74&HF_UHC$Doctors<3.09,"0.74<=Doctors<3.09",
                                     if_else(HF_UHC$Doctors>=3.09&HF_UHC$Doctors<11.96,"3.09<=Doctors<11.96",
                                             if_else(HF_UHC$Doctors>=11.96&HF_UHC$Doctors<=21.7,"11.96<=Doctors<21.7", 
                                                     if_else(HF_UHC$Doctors>21.7,"Doctors>=31.61", "NA")))))) %>%
  group_by(HWF_group) %>%
  summarise(mean_UHC = mean(UHC_SCI), mean_LE = mean(LE),mean_MMR=mean(MMR),mean_IMR=mean(IMR))



# Dentists<0.04, 0.04-0.11, 0.11-0.26, 0.26-2.17, 2.17-3.16, >3.16
HF_UHC %>%
  mutate(HWF_group = if_else(HF_UHC$Dentists<0.04, "Dentists <0.04", 
                             if_else(HF_UHC$Dentists>=0.04&HF_UHC$Dentists<0.11,"0.04<=Dentists<0.11",
                                     if_else(HF_UHC$Dentists>=0.11&HF_UHC$Dentists<0.26,"0.11<=Dentists<0.26",
                                             if_else(HF_UHC$Dentists>=0.26&HF_UHC$Dentists<=2.17,"0.26<=Dentists<2.17", 
                                                     if_else(HF_UHC$Dentists>2.17&HF_UHC$Dentists<=2.69 ,"2.17<=Dentists<=2.69", 
                                                             if_else(HF_UHC$Dentists>2.69,"Dentists>=2.69", "NA"))))))) %>%
  group_by(HWF_group) %>%
  summarise(mean_UHC = mean(UHC_SCI), mean_LE = mean(LE),mean_MMR=mean(MMR),mean_IMR=mean(IMR))




# Nurses/Midwives<4.44, 4.44-8.35, 8.35-16.22, 16.22-29.25, 29.25-45.99, 45.99-60.91, >60.91
HF_UHC %>%
  mutate(HWF_group = if_else(HF_UHC$NursingMidwifery<4.44, "Nurses <4.44", 
                             if_else(HF_UHC$NursingMidwifery>=4.44&HF_UHC$NursingMidwifery<8.35,"4.44<=NursingMidwifery<8.35",
                                     if_else(HF_UHC$NursingMidwifery>=8.35&HF_UHC$NursingMidwifery<16.22,"8.35<=NursingMidwifery<16.22",
                                             if_else(HF_UHC$NursingMidwifery>=16.22&HF_UHC$NursingMidwifery<29.25,"16.22<=NursingMidwifery<29.25", 
                                                     if_else(HF_UHC$NursingMidwifery>=29.25&HF_UHC$NursingMidwifery<=45.99 ,"29.25<=NursingMidwifery<=45.99", 
                                                             if_else(HF_UHC$NursingMidwifery>45.99,"NursingMidwifery>45.99", "NA"))))))) %>%
  group_by(HWF_group) %>%
  summarise(mean_UHC = mean(UHC_SCI), mean_LE = mean(LE),mean_MMR=mean(MMR),mean_IMR=mean(IMR))






# Pharmacists <0.07, 0.07-0.21, 0.21-0.60, 0.60-5.15, 5.15-7.77, 7.77-7.96, 7.96-10.08, >10.08
HF_UHC %>%
  mutate(HWF_group = if_else(HF_UHC$Pharmacists<0.07, "Pharmacists <0.07", 
                             if_else(HF_UHC$Pharmacists>=0.07&HF_UHC$Pharmacists<0.21,"0.07<=Pharmacists<0.21",
                                     if_else(HF_UHC$Pharmacists>=0.21&HF_UHC$Pharmacists<0.60,"0.21<=Pharmacists<0.60",
                                             if_else(HF_UHC$Pharmacists>=0.60&HF_UHC$Pharmacists<5.15,"0.60<=Pharmacists<5.15", 
                                                     if_else(HF_UHC$Pharmacists>5.15,"Pharmacists>=5.15", "NA")))))) %>%
  group_by(HWF_group) %>%
  summarise(mean_UHC = mean(UHC_SCI), mean_LE = mean(LE),mean_MMR=mean(MMR),mean_IMR=mean(IMR))














##########################    MODELLING RELATIONSHIP OF GGHED AND HWF       ############################################   


data_healthfinancing=data.frame(Country,Year, GDP, PopDensity, CHE, GGHED, HWF)

str(data_healthfinancing)


# Creating the Lagged Variables Model


library(dplyr)

data_healthfinancing <- data_healthfinancing %>%
  arrange(Country, Year) %>%   # order by panel structure
  group_by(Country) %>%        # ensure lags are within each county
  mutate(
    GGHED_Lag0 = GGHED,
    GGHED_Lag1 = dplyr::lag(GGHED, 1),
    GGHED_Lag2 = dplyr::lag(GGHED, 2),
    GGHED_Lag3 = dplyr::lag(GGHED, 3),
    GGHED_Lag4 = dplyr::lag(GGHED, 4),
    GGHED_Lag5 = dplyr::lag(GGHED, 5)
  )


# Model 1: Panel Data Analysis of GGHED and Health Workforce

panel_data<-pdata.frame(data_healthfinancing, index=c("Country","Year"))


# Fixed Effects Mode
fe_model <- plm(HWF ~ GGHED + GGHED_Lag1+GGHED_Lag2+GGHED_Lag3+GGHED_Lag4+GGHED_Lag5, data = panel_data, model = "within")
summary(fe_model)




# Model 2: Generalized Additive Model (Lagged)

# Full Model
HWF.GAM_Lag <- gam(HWF~s(GGHED)+s(GGHED_Lag1)+s(GGHED_Lag2)+s(GGHED_Lag3)+
                     s(GGHED_Lag4)+s(GGHED_Lag5), data = data_healthfinancing) 
summary(HWF.GAM_Lag)


# Partial Model with Lag0
HWF.GAM0 <- gam(HWF~s(GGHED), data = data_healthfinancing) 
summary(HWF.GAM0)

# Partial Model with Lag5
HWF.GAM5 <- gam(HWF~s(GGHED_Lag5), data = data_healthfinancing) 
summary(HWF.GAM5)


# Partial Model with Lag 0 and 5
HWF.GAM05 <- gam(HWF~s(GGHED)+s(GGHED_Lag5), data = data_healthfinancing) 
summary(HWF.GAM05)


# Most signigicant lag is lag 5 (Effect is seen after 5 years)

############ THRESHOLD LAGGED MODEL #####

smooths(HWF.GAM_Lag)

# Smoothing the current expenditure
est_current <- smooth_estimates(HWF.GAM_Lag, smooth = "s(GGHED)", n = 200)

# Smoothing the 5-year lagged expenditure
est_lag5 <- smooth_estimates(HWF.GAM_Lag, smooth = "s(GGHED_Lag5)", n = 200)

# Current expenditure slope
est_current$derivative <- c(NA, diff(est_current$.estimate) / diff(est_current$GGHED))

# 5-year lagged slope (note the backticks!)
est_lag5$derivative<- c(NA,diff(est_lag5$.estimate) / diff(est_lag5$GGHED_Lag5))

thresholds_current<- est_current[which(diff(sign(est_current$derivative)) != 0), ]
thresholds_lag5<- est_lag5[which(diff(sign(est_lag5$derivative)) != 0), ]

par(mfrow=c(1,2))
library(ggplot2)

ggplot(est_current, aes(x = GGHED, y = derivative)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Derivative of Current Expenditure Smooth")

ggplot(est_lag5, aes(x = `GGHED_Lag5`, y = derivative)) +
  geom_line(color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Derivative of 5-year Lagged Expenditure Smooth")



######################   Elasticity  ###############################

# Elasticity for current expenditure
est_current$elasticity <- est_current$derivative * (est_current$GGHED / est_current$.estimate)

# Elasticity for 5-year lagged expenditure
# est_lag5$elasticity <- est_lag5$derivative * (est_lag5$`GGHED_Lag[, 5]` / est_lag5$.estimate)

# Plot elasticity instead of raw derivative
ggplot(est_current, aes(x = GGHED, y = elasticity)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Elasticity of Current Expenditure")

# ggplot(est_lag5, aes(x = `GGHED_Lag[, 5]`, y = elasticity)) +
#  geom_line(color = "red") +
#  geom_hline(yintercept = 0, linetype = "dashed") +
#  labs(title = "Elasticity of 5-year Lagged Expenditure")

# Thresholds for lagged elasticity
#thresholds_lag5 <- est_lag5[which(diff(sign(est_lag5$elasticity)) != 0), ]

# Thresholds for current elasticity
thresholds_current <- est_current[which(diff(sign(est_current$elasticity)) != 0), ]



#################   Elasticity of GGHED to HWF ################################


# Non-Linear Relationship

gghed_seq <- seq(min(na.omit(data_healthfinancing$GGHED)),
                 max(na.omit(data_healthfinancing$GGHED)),
                 length.out = 10000)

newdata <- data.frame(GGHED = gghed_seq)
HWF.GAM <- gam(HWF~s(GGHED), data = data_healthfinancing) 
summary(HWF.GAM)
pred <- predict(HWF.GAM, newdata = newdata, type = "response")

deriv <- derivatives(HWF.GAM, term = "s(GGHED)", data = newdata)

elasticity <- deriv$.derivative*(gghed_seq / pred)

elasticity_df <- data.frame(GGHED = gghed_seq,
                            HWF = pred,
                            Elasticity = elasticity)
opt_gghed <- which.max(elasticity_df$Elasticity)
opt_hwf_gghed <- gghed_seq[opt_gghed]
opt_hwf_gghed
head(elasticity_df)

par(mfrow=c(1,2))
plot(elasticity_df$GGHED, elasticity_df$Elasticity, type="l",
     xlab="Government Health Expenditure",
     ylab="Elasticity",
     main="HWF Elasticity against Government Financing")


plot(elasticity_df$GGHED, elasticity_df$HWF, type="l",
     xlab="Government Health Expenditure",
     ylab="Health Workforce Density",
     main="HWF against Government Financing")


############################   Threshold of GGHED vs HWF       #####################

# Finding Peaks of GGHED Values per Capita
hwf_peaks <- findpeaks(elasticity_df$HWF)
hwf_peak_indices <- hwf_peaks[,1]
hwf_peak_indices 
gghed_peak_indices <-elasticity_df$GGHED[elasticity_df$HWF %in% hwf_peak_indices]
gghed_peak_indices

GGHED_HWF_Peaks<-data.frame(GGHED_peak=gghed_peak_indices,HWF_peak=hwf_peak_indices)
GGHED_HWF_Peaks

n.hwf.peaks<-length(hwf_peak_indices)
n.gghed.peaks<-length(gghed_peak_indices)


# Sensitivity Analysis of GGHED and HWF

HF_GGHED_KHO<-na.omit(data.frame(HWF,LE,MMR,IMR,UHC_SCI,Doctors,NursingMidwifery, Dentists, Pharmacists, GGHED))

HF_GGHED_KHO %>%
mutate(GGHED_group = if_else(HF_GGHED_KHO$GGHED<40, "GGHED<40", 
                     if_else(HF_GGHED_KHO$GGHED>=40&HF_GGHED_KHO$GGHED<200,"40<=GGHED<200",
                     if_else(HF_GGHED_KHO$GGHED>=200&HF_GGHED_KHO$GGHED<400,"200<=GGHED<400",
                     if_else(HF_GGHED_KHO$GGHED>400,"GGHED>400", "NA"))))) %>%
  group_by(GGHED_group) %>%
  summarise(mean_hwf = mean(HWF), mean_docs =mean(Doctors), mean_nur =mean(NursingMidwifery), mean_dent =mean(Dentists), mean_pharma =mean(Pharmacists), mean_UHC = mean(UHC_SCI), mean_LE = mean(LE),mean_MMR=mean(MMR),mean_IMR=mean(IMR))


## GGHED vs Key Health Outcomes
GGHED_KHO<-na.omit(data.frame(LE,MMR,IMR,UHC_SCI,GGHED))
GGHED_KHO %>%
  mutate(GGHED_group = if_else(GGHED_KHO$GGHED<100, "GGHED<100", 
                        if_else(GGHED_KHO$GGHED>=100&GGHED_KHO$GGHED<=300,"100<=GGHED<300",
                                       if_else(GGHED_KHO$GGHED>300,"GGHED>300", "NA")))) %>%
  group_by(GGHED_group) %>%
  summarise(mean_UHC = mean(UHC_SCI), mean_LE = mean(LE),mean_MMR=mean(MMR),mean_IMR=mean(IMR))


## GGHED vs HWF

GGHED_HWF<-na.omit(data.frame(HWF,Doctors,NursingMidwifery, Dentists, Pharmacists, GGHED))

GGHED_HWF %>%
  mutate(GGHED_group = if_else(GGHED_HWF$GGHED<100, "GGHED<100", 
                               if_else(GGHED_HWF$GGHED>=100&GGHED_HWF$GGHED<=300,"100<=GGHED<300",
                                       if_else(GGHED_HWF$GGHED>300,"GGHED>300", "NA")))) %>%
  group_by(GGHED_group) %>%
  summarise(mean_hwf = mean(HWF), mean_docs =mean(Doctors), mean_nur =mean(NursingMidwifery), mean_dent =mean(Dentists), mean_pharma =mean(Pharmacists))





############################     Other Results ############################

# Optimizing CHE for HWF
library(mgcv)

# (i) Fitting the GAM Model

gam_hwf <- gam(HWF ~ s(CHE), data = data_healthfinancing)

summary(gam_hwf)
plot(gam_hwf, shade=TRUE)

# (ii) Predicting across a grid of CHE values
che_grid <- data.frame(CHE = seq(min(data_healthfinancing$CHE, na.rm=TRUE),
                                 max(data_healthfinancing$CHE, na.rm=TRUE),
                                 length.out = 200))

pred_hwf <- predict(gam_hwf, newdata = che_grid)

# Find CHE that maximizes predicted HWF
optimal_index <- which.max(pred_hwf)
optimal_CHE <- che_grid$CHE[optimal_index]
optimal_CHE

# Effect of optimal CHE

library(dplyr)

# Suppose you already calculated optimal_CHE (a single numeric value)
data_healthfinancing<-na.omit(data_healthfinancing)
data_healthfinancing %>%
  mutate(CHE_group = if_else(CHE < optimal_CHE, 
                             "Below optimal CHE", "Above optimal CHE")) %>%
  group_by(CHE_group) %>%
  summarise(mean_HWF = mean(HWF, na.rm = TRUE))






# WHO Dataset Data

library(dplyr)
Data_WHO<-read.csv("Data synthesis_WHO_AFRO.csv",header=TRUE)
View(Data_WHO)
str(Data_WHO)

# Data Cleaning
Data_WHO$Labor.Force<-as.numeric(Data_WHO$Labor.Force) 
Data_WHO$GDP.Deflator<-as.numeric(Data_WHO$GDP.Deflator) 
Data_WHO$GNI <-as.numeric(Data_WHO$GNI) 
Data_WHO$GNI.per.capita <-as.numeric(Data_WHO$GNI.per.capita) 
Data_WHO$GNI.per.capita..PPP <-as.numeric(Data_WHO$GNI.per.capita..PPP) 


# Extracting Data for a particular Country

Data_WHO_Country<-Data_WHO%>%filter(Country=="Kenya")

#  Converting Data to TS Objects
Population_Country<- ts(Data_WHO_Country$Population, start = 2000, frequency = 1) # Yearly data
TFR_Country<- ts(Data_WHO_Country$TFR, start = 2000, frequency = 1) # Yearly data
Unemployment.Rate_Country<- ts(Data_WHO_Country$Unemployment.Rate, start = 2000, frequency = 1) # Yearly data
GEE.to.GDP.Ratio_Country<- ts(Data_WHO_Country$GEE.to.GDP.Ratio, start = 2000, frequency = 1) # Yearly data
GDP_Country<- ts(Data_WHO_Country$GDP, start = 2000, frequency = 1) # Yearly data
GDP.per.capita_Country<- ts(Data_WHO_Country$GDP.per.capita, start = 2000, frequency = 1) # Yearly data
GDP.Deflator_Country<- ts(Data_WHO_Country$GDP.Deflator, start = 2000, frequency = 1) # Yearly data
CHE.per.capita_Country<- ts(Data_WHO_Country$CHE.per.capita, start = 2000, frequency = 1) # Yearly data
CHE.to.GDP.Ratio_Country<- ts(Data_WHO_Country$CHE.to.GDP.Ratio, start = 2000, frequency = 1) # Yearly data
GGHED.to.CHE.Ratio_Country<- ts(Data_WHO_Country$GGHED.to.CHE.Ratio, start = 2000, frequency = 1) # Yearly data
GGHED.per.capita_Country<- ts(Data_WHO_Country$GGHED.per.capita, start = 2000, frequency = 1) # Yearly data
PVTD.to.CHE.Ratio_Country<- ts(Data_WHO_Country$PVTD.to.CHE.Ratio, start = 2000, frequency = 1) # Yearly data
PVTD.per.capita_Country<- ts(Data_WHO_Country$PVTD.per.capita, start = 2000, frequency = 1) # Yearly data
EXT.to.CHE.Ratio_Country<- ts(Data_WHO_Country$EXT.to.CHE.Ratio, start = 2000, frequency = 1) # Yearly data
EXT.per.capita_Country<- ts(Data_WHO_Country$EXT.per.capita, start = 2000, frequency = 1) # Yearly data
MMR_Country<- ts(Data_WHO_Country$MMR, start = 2000, frequency = 1) # Yearly data
IMR_Country<- ts(Data_WHO_Country$IMR, start = 2000, frequency = 1) # Yearly data
LE_Country<- ts(Data_WHO_Country$LE, start = 2000, frequency = 1) # Yearly data
UHC_Country<- ts(Data_WHO_Country$UHC_SCI, start = 2000, frequency = 1) # Yearly data
Doctors_Country<- ts(Data_WHO_Country$Doctors, start = 2000, frequency = 1) # Yearly data
Nursing_Midwifery_Country<- ts(Data_WHO_Country$Nursing_Midwifery, start = 2000, frequency = 1) # Yearly data
Dentists_Country<- ts(Data_WHO_Country$Dentists, start = 2000, frequency = 1) # Yearly data
Pharmacists_Country<- ts(Data_WHO_Country$Pharmacists, start = 2000, frequency = 1) # Yearly data
Debt.to.GDP.Ratio_Country<- ts(Data_WHO_Country$Debt.to.GDP.Ratio, start = 2000, frequency = 1) # Yearly data
Net.migration_Country<- ts(Data_WHO_Country$Net.migration, start = 2000, frequency = 1) # Yearly data
Labor.Force_Country<- ts(Data_WHO_Country$Labor.Force, start = 2000, frequency = 1) # Yearly data


# Population Visualizing the Time Series Data

plot(Population_Country, main = " Yearly Population Time Series",
     ylab = "Number", xlab = " Year", 
     col = "red", lwd = 2)

# Add seasonal subseries plot to see annual pattern
library(forecast)
library(ggplot2)
ggseasonplot(Population_Country, year.labels =TRUE, 
             year.labels.left=TRUE) +
  ggtitle ("Seasonal Plot : Country Population by Year") +
  ylab ("Number") + theme_minimal()


# TFR Model

HoltLinear.TFR<- ets(TFR_Country , model = "AAN")
HoltLinear.TFR

# Population Forecasts
TFR2025_2035<-forecast(HoltLinear.TFR, h = 11)
TFR2025_2035

plot(TFR2025_2035,
     main ="11 Year TFR Forecast",
     ylab ="Number", xlab = "Time",
     col = "red", fcol = "darkred")


# Population Model

HoltLinear.Population <- ets(Population_Country , model = "AAN")
HoltLinear.Population

# Population Forecasts
Population2025_2035<-forecast(HoltLinear.Population, h = 11)
Population2025_2035

plot(Population2025_2035,
     main ="11 Year Population Forecast",
     ylab ="Cases", xlab = "Time",
     col = "red", fcol = "darkred")


# Unemployment Rate Model

HoltLinear.Uemployment<- ets(Unemployment.Rate_Country , model = "AAN")
HoltLinear.Uemployment

# Population Forecasts
Uemployment2025_2035<-forecast(HoltLinear.Uemployment, h = 11)
Uemployment2025_2035

plot(Uemployment2025_2035,
     main ="11 Year Uemployment Rate Forecast",
     ylab ="Number", xlab = "Time",
     col = "red", fcol = "darkred")



# GDP Rate per Capita Model

HoltLinear.GDP.per.capita<- ets(GDP.per.capita_Country , model = "AAN")
HoltLinear.GDP.per.capita

# GDP per Capita Forecasts
GDP.per.capita2025_2035<-forecast(HoltLinear.GDP.per.capita, h = 11)
GDP.per.capita2025_2035

plot(GDP.per.capita2025_2035,
     main ="11 Year GDP.per.capita Rate Forecast",
     ylab ="Number", xlab = "Time",
     col = "red", fcol = "darkred")



# CHE per Capita Model

HoltLinear.CHE.per.capita<- ets(CHE.per.capita_Country , model = "AAN")
HoltLinear.CHE.per.capita

# CHE per Capita Forecasts
CHE.per.capita2025_2035<-forecast(HoltLinear.CHE.per.capita, h = 11)
CHE.per.capita2025_2035

plot(CHE.per.capita2025_2035,
     main ="11 Year CHE.per.capita Rate Forecast",
     ylab ="Number", xlab = "Time",
     col = "red", fcol = "darkred")



# GGHED per Capita Model

HoltLinear.GGHED.per.capita<- ets(GGHED.per.capita_Country , model = "AAN")
HoltLinear.GGHED.per.capita

# GGHED per Capita Forecasts
GGHED.per.capita2025_2035<-forecast(HoltLinear.GGHED.per.capita, h = 11)
GGHED.per.capita2025_2035

plot(GGHED.per.capita2025_2035,
     main ="11 Year GGHED.per.capita Rate Forecast",
     ylab ="Number", xlab = "Time",
     col = "red", fcol = "darkred")

GGHEDratio2025_2035<-GGHED.per.capita2025_2035$mean/CHE.per.capita2025_2035$mean


# PVTD per Capita Model

HoltLinear.PVTD.per.capita<- ets(PVTD.per.capita_Country , model = "AAN")
HoltLinear.PVTD.per.capita

# PVTD per Capita Forecasts
PVTD.per.capita2025_2035<-forecast(HoltLinear.PVTD.per.capita, h = 11)
PVTD.per.capita2025_2035

plot(PVTD.per.capita2025_2035,
     main ="11 Year PVTD.per.capita Rate Forecast",
     ylab ="Number", xlab = "Time",
     col = "red", fcol = "darkred")

PVTDratio2025_2035<-PVTD.per.capita2025_2035$mean/CHE.per.capita2025_2035$mean

# EXT per Capita Model

HoltLinear.EXT.per.capita<- ets(EXT.per.capita_Country , model = "AAN")
HoltLinear.EXT.per.capita

# EXT per Capita Forecasts
EXT.per.capita2025_2035<-forecast(HoltLinear.EXT.per.capita, h = 11)
EXT.per.capita2025_2035

plot(EXT.per.capita2025_2035,
     main ="11 Year EXT.per.capita Rate Forecast",
     ylab ="Number", xlab = "Time",
     col = "red", fcol = "darkred")


EXTratio2025_2035<-EXT.per.capita2025_2035$mean/CHE.per.capita2025_2035$mean


# Doctors per 10,000 Model

HoltLinear.Doctors<- ets(Doctors_Country , model = "AAN")
HoltLinear.Doctors

# Doctors Forecasts
Doctors.2025_2035<-forecast(HoltLinear.Doctors, h = 11)
Doctors.2025_2035

plot(Doctors2025_2035,
     main ="11 Year Doctors per 10,000 population Forecast",
     ylab ="Number", xlab = "Time",
     col = "red", fcol = "darkred")




# Nurses or Midwifery per 10,000 Model

HoltLinear.Nurses<- ets(Nursing_Midwifery_Country , model = "AAN")
HoltLinear.Nurses

# Nurses Forecasts
Nurses.2025_2035<-forecast(HoltLinear.Nurses, h = 11)
Nurses.2025_2035

plot(Nurses.2025_2035,
     main ="11 Year Nurses per 10,000 population Forecast",
     ylab ="Number", xlab = "Time",
     col = "red", fcol = "darkred")




# Dentists per 10,000 Model

HoltLinear.Dentists<- ets(Dentists_Country , model = "AAN")
HoltLinear.Dentists

# Dentists Forecasts
Dentists.2025_2035<-forecast(HoltLinear.Dentists, h = 11)
Dentists.2025_2035

plot(Dentists.2025_2035,
     main ="11 Year Dentists per 10,000 population Forecast",
     ylab ="Number", xlab = "Time",
     col = "red", fcol = "darkred")



# Pharmacy per 10,000 Model

HoltLinear.Pharmacists<- ets(Pharmacists_Country , model = "AAN")
HoltLinear.Pharmacists

# Pharmacists Forecasts
Pharmacists.2025_2035<-forecast(HoltLinear.Pharmacists, h = 11)
Pharmacists.2025_2035

plot(Pharmacists.2025_2035,
     main ="11 Year Pharmacists per 10,000 population Forecast",
     ylab ="Number", xlab = "Time",
     col = "red", fcol = "darkred")



# Predicted Parameters
gam.doc<-gam(Doctors ~ s(EXT) + s(GGHED) + s(PVTD)+s(Year), data = HF)
summary(gam.doc)
predict(gam.doc, newdata = data.frame(EXT=EXTratio2025_2035, GGHED=GGHEDratio2025_2035, PVTD=PVTDratio2025_2035, Year=c(2025:2035)), type = "response")

predict(gam.doc, newdata = data.frame(EXT=0.15, GGHED=0.67, PVTD=0.18, Year=c(2025:2035)), type = "response")











##############################        SENSITIVITY ANALYSIS              ###############################




####               Health Workforce Cadres        #############

New<-HF_HWF_Data%>%filter(Country=="Kenya",Year==2008)
Optimal.GGHED=round(New$CHE.per.capita*0.67,0)
Optimal.EXT=round(New$CHE.per.capita*0.15,0)
Optimal.PVTD=round(New$CHE.per.capita*0.18,0)

doctor_model<- gam(Doctors_Number ~ s(EXT.per.capita)+s(GGHED.per.capita)+s(PVTD.per.capita)+Country+Year, data = HF_HWF_Data)
x_doctor<-data.frame(EXT.per.capita   = Optimal.EXT, GGHED.per.capita = Optimal.GGHED, PVTD.per.capita  = Optimal.PVTD, Country   = "Kenya", Year= 2008)
pred_doctor <- round(predict(doctor_model, newdata = x_doctor),0)
pred_doctor


dentist_model<- gam(Dentists_Number ~ s(EXT.per.capita)+s(GGHED.per.capita)+s(PVTD.per.capita)+Country+Year, data = HF_HWF_Data)
x_dentist<-data.frame(EXT.per.capita   = Optimal.EXT, GGHED.per.capita = Optimal.GGHED, PVTD.per.capita  = Optimal.PVTD, Country   = "Kenya", Year= 2008)
pred_dentist <- round(predict(dentist_model, newdata = x_dentist),0)
pred_dentist


pharmacist_model<- gam(Pharmacists_Number ~ s(EXT.per.capita)+s(GGHED.per.capita)+s(PVTD.per.capita)+Country+Year, data = HF_HWF_Data)
x_pharmacist<-data.frame(EXT.per.capita   = Optimal.EXT, GGHED.per.capita = Optimal.GGHED, PVTD.per.capita  = Optimal.PVTD, Country   = "Kenya", Year= 2008)
pred_pharmacist <- round(predict(pharmacist_model, newdata = x_pharmacist),0)
pred_pharmacist


nurse_model<- gam(Nursing_Midwifery_Number ~ s(EXT.per.capita)+s(GGHED.per.capita)+s(PVTD.per.capita)+Country+Year, data = HF_HWF_Data)
x_nurse<-data.frame(EXT.per.capita   = Optimal.EXT, GGHED.per.capita = Optimal.GGHED, PVTD.per.capita  = Optimal.PVTD, Country   = "Kenya", Year= 2008)
pred_nurse <- round(predict(nurse_model, newdata = x_nurse),0)
pred_nurse




####               Health Outcomes        #############

Optimal.Doctors=round(10000*pred_doctor/New$Population,2)
Optimal.Nurses=round(10000*pred_nurse/New$Population,2)
Optimal.Pharmacists=round(10000*pred_pharmacist/New$Population,2)
Optimal.Dentists=round(10000*pred_dentist/New$Population,2)




le_model<- gam(LE ~ s(EXT.per.capita)+s(GGHED.per.capita)+s(PVTD.per.capita)+s(Doctors)+s(Nursing_Midwifery)+ s(Pharmacists)+ s(Dentists) + Country+Year, data = HF_HWF_Data)
x_le<-data.frame(EXT.per.capita   = Optimal.EXT, GGHED.per.capita = Optimal.GGHED, PVTD.per.capita  = Optimal.PVTD, 
                 Doctors=Optimal.Doctors, Nursing_Midwifery=Optimal.Nurses,  Pharmacists=Optimal.Pharmacists, Dentists=Optimal.Dentists, 
                 Country   = "Kenya", Year= 2008)
pred_le <- round(predict(le_model, newdata = x_le),2)
pred_le





uhc_model<- gam(UHC_SCI ~ s(EXT.per.capita)+s(GGHED.per.capita)+s(PVTD.per.capita)+s(Doctors)+s(Nursing_Midwifery)+ s(Pharmacists)+ s(Dentists) + Country+Year, data = HF_HWF_Data)
x_uhc<-data.frame(EXT.per.capita   = Optimal.EXT, GGHED.per.capita = Optimal.GGHED, PVTD.per.capita  = Optimal.PVTD, 
                 Doctors=Optimal.Doctors, Nursing_Midwifery=Optimal.Nurses,  Pharmacists=Optimal.Pharmacists, Dentists=Optimal.Dentists, 
                 Country   = "Kenya", Year= 2008)
pred_uhc <- round(predict(uhc_model, newdata = x_uhc),0)
pred_uhc





imr_model<- gam(IMR ~ s(EXT.per.capita)+s(GGHED.per.capita)+s(PVTD.per.capita)+s(Doctors)+s(Nursing_Midwifery)+ s(Pharmacists)+ s(Dentists) + Country+Year, data = HF_HWF_Data)
x_imr<-data.frame(EXT.per.capita   = Optimal.EXT, GGHED.per.capita = Optimal.GGHED, PVTD.per.capita  = Optimal.PVTD, 
                  Doctors=Optimal.Doctors, Nursing_Midwifery=Optimal.Nurses,  Pharmacists=Optimal.Pharmacists, Dentists=Optimal.Dentists, 
                  Country   = "Kenya", Year= 2008)
pred_imr <- round(predict(imr_model, newdata = x_imr),2)
pred_imr




mmr_model<- gam(MMR ~ s(EXT.per.capita)+s(GGHED.per.capita)+s(PVTD.per.capita)+s(Doctors)+s(Nursing_Midwifery)+ s(Pharmacists)+ s(Dentists) + Country+Year, data = HF_HWF_Data)
x_mmr<-data.frame(EXT.per.capita   = Optimal.EXT, GGHED.per.capita = Optimal.GGHED, PVTD.per.capita  = Optimal.PVTD, 
                  Doctors=Optimal.Doctors, Nursing_Midwifery=Optimal.Nurses,  Pharmacists=Optimal.Pharmacists, Dentists=Optimal.Dentists, 
                  Country   = "Kenya", Year= 2008)
pred_mmr <- round(predict(mmr_model, newdata = x_mmr),2)
pred_mmr
