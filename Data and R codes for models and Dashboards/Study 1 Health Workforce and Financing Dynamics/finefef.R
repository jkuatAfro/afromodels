####Required packages
library(tidyverse)
library(dplyr)
library(plm)
library(readxl)

###load data
library(readxl)
Data_synthesis_WHO_AFRO <- read_excel("~/Documents/WHO-AFRO/Models/Data synthesis_WHO_AFRO.xlsx")
View(Data_synthesis_WHO_AFRO)
dat<-Data_synthesis_WHO_AFRO
names(dat)


###Create total health workforce
dat$Total_Workforce <- dat$Doctors_Number +
  dat$Nursing_Midwifery_Number +
  dat$Dentists_Number +
  dat$Pharmacists_Number

# Create workforce density per 10,000 population
dat$Workforce_Density <- (dat$Total_Workforce / dat$Population) * 10000


plot(dat$Workforce_Density, dat$UHC_SCI)
summary(dat$Workforce_Density)


plot(dat$Workforce_Density, dat$LE)

summary(dat$Workforce_Density)
summary(dat$UHC_SCI)
summary(dat$LE)
summary(dat$HALE)
summary(dat$NMR)

sum(is.na(dat$Workforce_Density))
sum(is.na(dat$UHC_SCI))

sum(is.infinite(dat$Workforce_Density))
sum(is.infinite(dat$UHC_SCI))

clean_dat <- dat[is.finite(dat$Workforce_Density) & 
                   is.finite(dat$UHC_SCI), ]

datr<- dat %>%
  rename(`Migrant_Stock` = `Migrant Stock`,
         'Population_Density'= 'Population Density',
         'GGHED_per_capita'='GGHED per capita',
         'GDP_per_capita'='GDP per capita',
         'GGHED_to_GDP_Ratio'='GGHED to GDP Ratio',)


###create variable for the core 
datr$WD_core <- datr$Doctors + datr$Nursing_Midwifery
datr$log_WD_core <- log1p(datr$WD_core)
####Life Expectancy
fe_le <- plm(
 LE~ Doctors + Nursing_Midwifery +  GGHED_to_GDP_Ratio+Population_Density+
    factor(Year),
  data = datr,
  index = c("Country", "Year"),
  model = "within"
)
summary(fe_le)


#####specific UHC - infectious 
fe_UHCID <- plm(
  SCI_ID ~ Doctors + Nursing_Midwifery +  GGHED_to_GDP_Ratio+Population_Density+
    factor(Year),
  data = datr,
  index = c("Country", "Year"),
  model = "within"
)
summary(fe_UHCID)
fe_UHCIDR <- plm(
  SCI_ID ~ Doctors + Nursing_Midwifery +  GGHED_to_GDP_Ratio+Population_Density+
    factor(Year),
  data = datr,
  index = c("Country", "Year"),
  model = "random"
)
summary(fe_UHCIDR)

phtest(fe_UHCID, fe_UHCIDR)

##SCI_NCD

fe_SCI_NCD <- plm(
  SCI_NCD ~ Doctors + Nursing_Midwifery +  GGHED_to_GDP_Ratio+Population_Density+
    factor(Year),
  data = datr,
  index = c("Country", "Year"),
  model = "within"
)
summary(fe_SCI_NCD)

fe_SCI_NCDR <- plm(
  SCI_NCD ~ Doctors + Nursing_Midwifery +  GGHED_to_GDP_Ratio+Population_Density+
    factor(Year),
  data = datr,
  index = c("Country", "Year"),
  model ="random"
)
summary(fe_SCI_NCDR)

phtest(fe_SCI_NCD, fe_SCI_NCDR)

library(lmtest)
library(sandwich)

coeftest(
  fe_SCI_NCDR,
  vcov = vcovHC(
    fe_SCI_NCDR,
    method = "arellano",
    type = "HC1",
    cluster = "group"
  )
)
####SCI_RMNC
fe_RMNC <- plm(
  SCI_RMNC ~ Doctors + Nursing_Midwifery +  GGHED_to_GDP_Ratio+Population_Density+
    factor(Year),
  data = datr,
  index = c("Country", "Year"),
  model = "within"
)
summary(fe_RMNC)

fe_RMNCR <- plm(
  SCI_RMNC ~ Doctors + Nursing_Midwifery +  GGHED_to_GDP_Ratio+Population_Density+
    factor(Year),
  data = datr,
  index = c("Country", "Year"),
  model = "random"
)
summary(fe_RMNCR)
phtest(fe_RMNC, fe_RMNCR)


###UHC_SCI
fe_UHC <- plm(
  UHC_SCI ~ Doctors + Nursing_Midwifery +  GGHED_to_GDP_Ratio+Population_Density+
    factor(Year),
  data = datr,
  index = c("Country", "Year"),
  model = "within"
)
summary(fe_UHC)
fe_UHCW <- plm(
  UHC_SCI ~ Workforce_Density+  GGHED_to_GDP_Ratio+Population_Density+
    factor(Year),
  data = datr,
  index = c("Country", "Year"),
  model = "within"
)
summary(fe_UHCW)

AIC_plm <- function(model){
  
  n <- nobs(model)
  rss <- sum(residuals(model)^2)
  
  k <- length(coef(model))
  
  AIC <- n*log(rss/n) + 2*k
  
  return(AIC)
}

BIC_plm <- function(model){
  
  n <- nobs(model)
  rss <- sum(residuals(model)^2)
  
  k <- length(coef(model))
  
  BIC <- n*log(rss/n) + log(n)*k
  
  return(BIC)
}
AIC_plm(fe_UHC)
AIC_plm(fe_UHCW)
AIC_plm(fe_RMNC)
AIC_plm(fe_UHCID)
AIC_plm(fe_SCI_NCD)

BIC_plm(fe_UHC)
BIC_plm(fe_UHCW)
BIC_plm(fe_RMNC)
BIC_plm(fe_UHCID)
BIC_plm(fe_SCI_NCD)

data.frame(
  Model = c("Doctors + Nurses",
            "Total Workforce"),
  AIC = c(
    AIC_plm(fe_UHC),
    AIC_plm(fe_UHCW)
  ),
  BIC = c(
    BIC_plm(fe_UHC),
    BIC_plm(fe_UHCW)
  ),
  R2 = c(
    summary(fe_UHC)$r.squared["rsq"],
    summary(fe_UHCW)$r.squared["rsq"]
  )
)

## workforce density UHC-SCI

thresholds <- seq(5, 80, by = 1)   # adjust if needed
rss <- numeric(length(thresholds))

for(i in seq_along(thresholds)) {
  
  k <- thresholds[i]
  
  datr$W_kink <- pmax(datr$Workforce_Density - k, 0)
  
  model <- plm(
    UHC_SCI ~ Workforce_Density + W_kink +
      GGHED_to_GDP_Ratio+Population_Density+
      factor(Year),
    data = datr,
    index = c("Country","Year"),
    model = "random"
  )
  
  rss[i] <- sum(residuals(model)^2)
}

best_k_re <- thresholds[which.max(rss)]
best_k_re

###LE
thresholds <- seq(5, 80, by = 1)   # adjust if needed
rss <- numeric(length(thresholds))





for(i in seq_along(thresholds)) {
  
  k <- thresholds[i]
  
  datr$W_kink <- pmax(datr$Workforce_Density - k, 0)
  
  model <- plm(
    HALE ~ Workforce_Density + W_kink +
      GGHED_to_GDP_Ratio+Population_Density+
      factor(Year),
    data = datr,
    index = c("Country","Year"),
    model = "within"
  )
  
  rss[i] <- sum(residuals(model)^2)
}

best_k_re <- thresholds[which.min(rss)]
best_k_re

###plot

plot(thresholds, rss, type="l")
abline(v = best_k_re, col="red", lty=2)

plot(thresholds, rss, type="l", lwd=2,
     xlab="Workforce Density Threshold (per 10,000)",
     ylab="Residual Sum of Squares",
     main="Grid Search for Optimal Workforce Threshold")

abline(v = best_k_re, col="red", lwd=2, lty=2)
points(best_k_re, min(rss), pch=19, col="red")

text(best_k_re,
     min(rss),
     labels = paste("Optimal k =", best_k_re),
     pos = 4,
     col = "red")


model <- plm(
 LE ~ Workforce_Density +
    Population_Density+ GGHED_to_GDP_Ratio+
    factor(Year),
  data = datr,
  index = c("Country","Year"),
  model = "within"
)
summary(model)

model1 <- plm(
  LE ~ Workforce_Density +
    Population_Density+ GGHED_to_GDP_Ratio+
    factor(Year),
  data = datr,
  index = c("Country","Year"),
  model = "random"
)
summary(model1)


phtest(model, model1)


datr <- pdata.frame(datr, index=c("Country","Year"))

datr$WD_l1 <- lag(datr$Workforce_Density, 1)
datr$WD_l2 <- lag(datr$Workforce_Density, 2)
datr$WD_l3 <- lag(datr$Workforce_Density, 3)

m_le_lags <- plm(
  LE ~ Workforce_Density + WD_l1 + WD_l2 + WD_l3 +
    Population_Density + GGHED_to_GDP_Ratio + factor(Year),
  data = datr,
  model = "within"
)

summary(m_le_lags)

###NMR


thresholds <- seq(5, 80, by = 1)   # adjust if needed
rss <- numeric(length(thresholds))

for(i in seq_along(thresholds)) {
  
  k <- thresholds[i]
  
  datr$W_kink <- pmax(datr$Workforce_Density - k, 0)
  
  model <- plm(
    NMR ~ Workforce_Density + W_kink +
      GGHED_to_GDP_Ratio+Population_Density+
      factor(Year),
    data = datr,
    index = c("Country","Year"),
    model = "within"
  )
  
  rss[i] <- sum(residuals(model)^2)
}

best_k_re <- thresholds[which.max(rss)]
best_k_re




###March
summary(lm_model)
library(plm)
library(segmented)
library(ggplot2)

model_fe <- plm(LE ~ Workforce_Density + GGHED_to_GDP_Ratio,
                data = datr,
                 model = "within")

summary(model_fe)

lm_model <- lm(LE ~ Doctors + GGHED_to_GDP_Ratio,data=datr)


summary(lm_model)
library(plm)
library(segmented)
library(ggplot2)

kink_model <- segmented(lm_model, seg.Z = ~Doctors)

summary(kink_model)


plot(kink_model)

plot(datr$Doctors, datr$LE)
plot(kink_model, add=TRUE)

####

lm(LE ~ Doctors + GGHED_to_GDP_Ratio + factor(Country), data = datr)

datr$Workforce_above38 <- pmax(0, datr$Workforce_Density - 38)
model_pw <- lm(LE ~ Workforce_Density + Workforce_above38 + GGHED_to_GDP_Ratio,
               data = datr)
summary(model_pw)

datr$threshold_group <- ifelse(datr$Workforce_Density < 38,
                               "Below Threshold",
                               "Above Threshold")
datr$distance_to_threshold <- datr$Workforce_Density - 38

library(ggplot2)

ggplot(datr, aes(x = Workforce_Density, y = LE, color = threshold_group)) +
  geom_point() +
  geom_vline(xintercept = 38, linetype = "dashed", color = "red") +
  labs(title = "Workforce Density vs Life Expectancy",
       x = "Workforce Density (per 10,000)",
       y = "Life Expectancy") +
  theme_minimal()
##
datr$threshold_group <- ifelse(datr$Workforce_Density < 38,
                               "Below Threshold", "Above Threshold")
library(plotly)
library(dplyr)
tabPanel("Map", plotlyOutput("mapPlot"))
output$mapPlot <- renderPlotly({
  
  df <- datr %>%
    filter(!is.na(Workforce_Density))
  
  plot_ly(
    data = df,
    type = "choropleth",
    locations = ~Country,
    locationmode = "country names",
    z = ~as.numeric(threshold_group == "Above Threshold"),
    text = ~paste(Country,
                  "<br>Workforce:", Workforce_Density,
                  "<br>LE:", LE,
                  "<br>Status:", threshold_group),
    colors = c("red", "green")
  ) %>%
    layout(
      title = "Countries by Workforce Threshold (38 per 10,000)",
      geo = list(showframe = FALSE,
                 showcoastlines = TRUE)
    )
})

####other trials

install.packages("PanelThreshold")
library(PanelThreshold)

###data preparation
datr <- na.omit(datr)

y <- datr$UHC_SCI   # or LE
x <- as.matrix(datr[, c("Workforce_Density",
                        "Population_Density",
                        "GGHED_to_GDP_Ratio")])

q <- datr$Workforce_Density   # threshold variable
id <- datr$Country
time <- datr$Year
###run threshold model
model_th <- ptm(
  y = y,
  x = x,
  q = q,
  index = cbind(id, time),
  model = "within",
  effect = "individual",
  trim = 0.05,        # avoids extreme thresholds
  nboot = 300         # bootstrap for significance
  
  
 ## by country
 
  
  library(dplyr)
  
  find_k_by_country <- function(country_name) {
    
    data_sub <- datr %>% filter(Country == country_name)
    
    thresholds <- seq(5, 80, by = 1)
    rss <- numeric(length(thresholds))
    
    for(i in seq_along(thresholds)) {
      
      k <- thresholds[i]
      
      data_sub$W_kink <- pmax(data_sub$Workforce_Density - k, 0)
      
      model <- lm(
        UHC_SCI ~ Workforce_Density + W_kink +
          GGHED_to_GDP_Ratio + Population_Density + factor(Year),
        data = data_sub
      )
      
      rss[i] <- sum(residuals(model)^2)
    }
    
    best_k <- thresholds[which.min(rss)]
    
    return(best_k)
  }
  
  

  
  ###threshold for others
  
  
  ## workforce density UHC-SCI
  
  thresholds <- seq(5, 80, by = 1)   # adjust if needed
  rss <- numeric(length(thresholds))
  
  for(i in seq_along(thresholds)) {
    
    k <- thresholds[i]
    
    datr$W_kink <- pmax(datr$Workforce_Density - k, 0)
    
    model <- plm(
      UHC_SCI ~ Workforce_Density + W_kink +
        GGHED_to_GDP_Ratio+Population_Density+
        factor(Year),
      data = datr,
      index = c("Country","Year"),
      model = "random"
    )
    
    rss[i] <- sum(residuals(model)^2)
  }
  
  best_k_re <- thresholds[which.min(rss)]
  best_k_re
  
  
  ###plot
  
  plot(thresholds, rss, type="l")
  abline(v = best_k_re, col="red", lty=2)
  
  plot(thresholds, rss, type="l", lwd=2,
       xlab="Workforce Density Threshold (per 10,000)",
       ylab="Residual Sum of Squares",
       main="Grid Search for Workforce Threshold Overall UHC_SCI")
  
  abline(v = best_k_re, col="red", lwd=2, lty=2)
  points(best_k_re, min(rss), pch=19, col="red")
  
  text(best_k_re,
       min(rss),
       labels = paste("Optimal k =", best_k_re),
       pos = 4,
       col = "red")
  
  
  ## workforce density RMNCI
  
  
  thresholds <- seq(5, 80, by = 1)   # adjust if needed
  rss <- numeric(length(thresholds))
  
  for(i in seq_along(thresholds)) {
    
    k <- thresholds[i]
    
    datr$W_kink <- pmax(datr$Workforce_Density - k, 0)
    
    model <- plm(
      SCI_RMNC ~ Workforce_Density + W_kink +
        GGHED_to_GDP_Ratio+Population_Density+
        factor(Year),
      data = datr,
      index = c("Country","Year"),
      model = "within"
    )
    
    rss[i] <- sum(residuals(model)^2)
  }
  
  best_k_re <- thresholds[which.min(rss)]
  best_k_re
  names(datr)
  summary(model)
  ###plot
  
  plot(thresholds, rss, type="l")
  abline(v = best_k_re, col="red", lty=2)
  
  plot(thresholds, rss, type="l", lwd=2,
       xlab="Workforce Density Threshold (per 10,000)",
       ylab="Residual Sum of Squares",
       main="Grid Search for Optimal Workforce Threshold SCI RMNC")
  
  abline(v = best_k_re, col="red", lwd=2, lty=2)
  points(best_k_re, min(rss), pch=19, col="red")
  
  text(best_k_re,
       min(rss),
       labels = paste("Optimal k =", best_k_re),
       pos = 4,
       col = "red")
  
  
  thresholds <- seq(5, 80, by = 1)   # adjust if needed
  rss <- numeric(length(thresholds))
  
  for(i in seq_along(thresholds)) {
    
    k <- thresholds[i]
    
    datr$W_kink <- pmax(datr$Workforce_Density - k, 0)
    
    model <- plm(
      SCI_NCD ~ Workforce_Density + W_kink +
        GGHED_to_GDP_Ratio+Population_Density+
        factor(Year),
      data = datr,
      index = c("Country","Year"),
      model = "random"
    )
    
    rss[i] <- sum(residuals(model)^2)
  }
  
  best_k_re <- thresholds[which.min(rss)]
  best_k_re
  names(datr)
  
  ###plot
  
  plot(thresholds, rss, type="l")
  abline(v = best_k_re, col="red", lty=2)
  
  plot(thresholds, rss, type="l", lwd=2,
       xlab="Workforce Density Threshold (per 10,000)",
       ylab="Residual Sum of Squares",
       main="Grid Search for Optimal Workforce Threshold SCI NCDs")
  
  abline(v = best_k_re, col="red", lwd=2, lty=2)
  points(best_k_re, min(rss), pch=19, col="red")
  
  text(best_k_re,
       min(rss),
       labels = paste("Optimal k =", best_k_re),
       pos = 4,
       col = "red")
  
  
  
  ## workforce density NCDs
  
  
  thresholds <- seq(5, 80, by = 1)   # adjust if needed
  rss <- numeric(length(thresholds))
  
  for(i in seq_along(thresholds)) {
    
    k <- thresholds[i]
    
    datr$W_kink <- pmax(datr$Workforce_Density - k, 0)
    
    model <- plm(
      SCI_NCD ~ Workforce_Density + W_kink +
        GGHED_to_GDP_Ratio+Population_Density+
        factor(Year),
      data = datr,
      index = c("Country","Year"),
      model = "random"
    )
    
    rss[i] <- sum(residuals(model)^2)
  }
  
  best_k_re <- thresholds[which.min(rss)]
  best_k_re
  names(datr)
  summary(model)
  ###plot
  
  plot(thresholds, rss, type="l")
  abline(v = best_k_re, col="red", lty=2)
  
  plot(thresholds, rss, type="l", lwd=2,
       xlab="Workforce Density Threshold (per 10,000)",
       ylab="Residual Sum of Squares",
       main="Grid Search for Optimal Workforce Threshold SCI NCD")
  
  abline(v = best_k_re, col="red", lwd=2, lty=2)
  points(best_k_re, min(rss), pch=19, col="red")
  
  text(best_k_re,
       min(rss),
       labels = paste("Optimal k =", best_k_re),
       pos = 4,
       col = "red")
  
  
  
  
  
  
  ## workforce density ID
  
  
  thresholds <- seq(5, 80, by = 1)   # adjust if needed
  rss <- numeric(length(thresholds))
  
  for(i in seq_along(thresholds)) {
    
    k <- thresholds[i]
    
    datr$W_kink <- pmax(datr$Workforce_Density - k, 0)
    
    model <- plm(
      SCI_ID ~ Workforce_Density + W_kink +
        GGHED_to_GDP_Ratio+Population_Density+
        factor(Year),
      data = datr,
      index = c("Country","Year"),
      model = "within"
    )
    
    rss[i] <- sum(residuals(model)^2)
  }
  
  best_k_re <- thresholds[which.min(rss)]
  best_k_re
  names(datr)
  summary(model)
  ###plot
  
  plot(thresholds, rss, type="l")
  abline(v = best_k_re, col="red", lty=2)
  
  plot(thresholds, rss, type="l", lwd=2,
       xlab="Workforce Density Threshold (per 10,000)",
       ylab="Residual Sum of Squares",
       main="Grid Search for Optimal Workforce Threshold SCI ID")
  
  abline(v = best_k_re, col="red", lwd=2, lty=2)
  points(best_k_re, min(rss), pch=19, col="red")
  
  text(best_k_re,
       min(rss),
       labels = paste("Optimal k =", best_k_re),
       pos = 4,
       col = "red")
  
  k <- 7
  
  datr$W_kink <- pmax(datr$Workforce_Density - k, 0)
  
  id_threshold <- plm(
    SCI_ID ~ Workforce_Density +
      W_kink +
      GGHED_to_GDP_Ratio +
      Population_Density +
      factor(Year),
    data = datr,
    model = "within",
    index = c("Country","Year")
  )
  library(lmtest)
  library(sandwich)
  
  coeftest(
    id_threshold,
    vcov = vcovHC(
      id_threshold,
      method = "arellano",
      type = "HC1",
      cluster = "group"
    )
  )
  
  

  
 