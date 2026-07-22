###Dynamic models-3SLS Structural Pathways from
#Health Workforce to Productivity: A Three-Stage Least
#Squares

library(readxl)
my_data<-read_excel("Data Synthesis_WHO_AFRO.xlsx", sheet="Assimilated_WHO_Data")

#converting macro variables to log
#combining the cadres
library(dplyr)
my_data <- my_data %>%
  mutate(
    total_density = Dentists+Doctors+Nursing_Midwifery+Pharmacists,
    log_density = log(total_density),
    log_gdp = log(`GDP per capita, PPP`),
    log_dalys = log(DALYS),
    employed=`Labor Force`*(1-`Unemployment Rate`/100),
    log_productivity = log(GDP/employed)
  )
#View(my_data)
library(systemfit)
eq1 <- UHC_SCI ~ log_density + log_gdp 
eq2 <- log_dalys ~ UHC_SCI + log_gdp
eq3 <- log_productivity ~ log_dalys + HALE

system <- list(eq1 = eq1, eq2 = eq2, eq3 = eq3)

#using 3SLS
fit_model<-systemfit(system,
                     method = "3SLS",
                     inst = ~ log_density + log_gdp  + HALE,
                     data = my_data)
summary(fit_model)

#cadre specific-Output in the report
df_system <- my_data %>%
  select(
    Country,
    Year,
    UHC_SCI,
    Doctors,
    Nursing_Midwifery,
    log_gdp,
    log_dalys,
    log_productivity,
    HALE
  )
View(df_system)


#distribution of dalys
library(ggplot2)
ggplot(df_system, aes(x = log_dalys)) +
  geom_histogram(bins = 15) +
  facet_wrap(~ Country, scales = "free_y") +
  theme_minimal() +
  labs(title = "Distribution of log(DALYs) by Country")

#interpolate DALYs-simple linear interpolation
library(zoo)

df_system <- df_system %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(
    log_dalys = na.approx(log_dalys, Year, na.rm = FALSE)
  ) %>%
  ungroup()

#check smoothness
plot(df_system$Year, df_system$log_dalys)
plot(my_data$Year, my_data$DALYS)

#check for missingness
summary(df_system)
colSums(is.na(df_system))


#create lags
library(plm)


# Sort properly first
df_system <- df_system %>%
  arrange(Country, Year)

# Declare panel structure
pdata <- pdata.frame(df_system, index = c("Country", "Year"))

# Create 1-year lags
pdata$lag_UHC        <- lag(pdata$UHC_SCI, 1)
pdata$lag_dalys      <- lag(pdata$log_dalys, 1)
pdata$lag_prod       <- lag(pdata$log_productivity, 1)
pdata$lag_HALE       <- lag(pdata$HALE, 1)

#remove rows where lagged values are missing
#pdata_dyn <- na.omit(pdata)

#3SLS modelling
eq1_dyn <- UHC_SCI ~ lag_UHC + Doctors + Nursing_Midwifery + log_gdp
eq2_dyn <- log_dalys ~ lag_dalys + UHC_SCI + log_gdp
eq3_dyn <- log_productivity ~ lag_prod + log_dalys + HALE + lag_HALE

library(systemfit)
pdata <- as.data.frame(pdata)
system_dyn <- list(
  uhc  = eq1_dyn,
  daly = eq2_dyn,
  prod = eq3_dyn
)

fit_3sls_dyn <- systemfit(
  system_dyn,
  method = "3SLS",
  inst = ~ lag_UHC + lag_dalys + lag_prod +
    Doctors + Nursing_Midwifery + log_gdp +
    HALE + lag_HALE,
  data = pdata
)

summary(fit_3sls_dyn)
#extracting confidence intervals
confint(fit_3sls_dyn)


##Structural modelling by country
#by country
library(systemfit)
library(dplyr)

run_3sls_country <- function(df, country_name) {
  
  # Subset one country
  df_c <- df[df$Country == country_name, ]
  
  # Sort properly
  df_c <- df_c[order(df_c$Year), ]
  
  # Create lags
  df_c$lag_UHC   <- dplyr::lag(df_c$UHC_SCI, 1)
  df_c$lag_dalys <- dplyr::lag(df_c$log_dalys, 1)
  df_c$lag_prod  <- dplyr::lag(df_c$log_productivity, 1)
  df_c$lag_HALE  <- dplyr::lag(df_c$HALE, 1)
  
  # Remove first observation (due to lag)
  # df_c <- na.omit(df_c)
  
  # Define system
  eq1 <- UHC_SCI ~ lag_UHC + Doctors + Nursing_Midwifery + log_gdp
  eq2 <- log_dalys ~ lag_dalys + UHC_SCI + log_gdp
  eq3 <- log_productivity ~ lag_prod + log_dalys + HALE + lag_HALE
  
  system <- list(
    uhc  = eq1,
    daly = eq2,
    prod = eq3
  )
  # Explicit instrument matrix
  inst <- ~ lag_UHC + lag_dalys + lag_prod +
    Doctors + Nursing_Midwifery + log_gdp +
    HALE + lag_HALE
  
  fit <- tryCatch(
    systemfit(system, method = "3SLS",inst = inst, data = df_c),
    error = function(e) return(NULL)
  )
  
  return(fit)
}

#run for one country
fit_kenya <- run_3sls_country(df_system, "Kenya")

summary(fit_kenya)

#run for all countries
countries <- unique(df_system$Country)

results_list <- lapply(countries, function(cty) {
  run_3sls_country(df_system, cty)
})

names(results_list) <- countries



#extract long run effects
extract_long_run <- function(fit) {
  
  if (is.null(fit)) return(NA)
  
  coefs <- coef(fit)
  
  alpha <- coefs["prod_lag_prod"]
  b0    <- coefs["prod_HALE"]
  b1    <- coefs["prod_lag_HALE"]
  
  if (is.na(alpha)) return(NA)
  
  (b0 + b1) / (1 - alpha)
}

long_run_effects <- sapply(results_list, extract_long_run)

long_run_effects


library(systemfit)
library(dplyr)
library(tidyr)

coef_wide <- lapply(unique(pdata$Country), function(cty) {
  
  fit <- tryCatch(
    systemfit(system_dyn, method = "3SLS", inst = inst,
              data = filter(pdata, Country == cty)),
    error = function(e) NULL
  )
  
  if (is.null(fit)) return(NULL)
  
  data.frame(
    Country = cty,
    t(coef(fit))   # transpose → wide format directly
  )
  
}) %>%
  bind_rows()

# Explicit instrument matrix
inst1 <- ~ lag_UHC + lag_dalys + lag_prod +
  log_gdp 

countries <- unique(pdata$Country)

coef_wide <- lapply(countries, function(cty) {
  
  df_c <- pdata %>%
    filter(as.character(Country) == cty) %>%
    na.omit()
  
  if (nrow(df_c) < 1) return(NULL)
  
  fit <- tryCatch(
    systemfit(system_dyn, method = "3SLS", inst = inst1, data = df_c),
    error = function(e) NULL
  )
  
  if (is.null(fit)) return(NULL)
  
  data.frame(
    Country = cty,
    t(coef(fit))
  )
  
}) %>% bind_rows()
write.csv(coef_wide, "3sls_0coefficients_by_country.csv", row.names = FALSE)

#country specific
df_test <- pdata %>% filter(Country == "Algeria") %>% na.omit()

systemfit(system_dyn, method = "3SLS", inst = inst, data = df_test)


#missing variables
missing_summary <- pdata %>%
  summarise(across(everything(), ~ sum(is.na(.))), .by = Country) %>%
  pivot_longer(
    cols = -Country,
    names_to = "Variable",
    values_to = "Missing_Count"
  ) %>%
  arrange(Country, desc(Missing_Count))
write.csv(missing_summary, "missing.csv", row.names = FALSE)

library(systemfit)
library(dplyr)

#inst1 <- ~ lag_UHC + lag_dalys + lag_prod + log_gdp
inst1<-~lag_UHC + lag_dalys + lag_prod +
  Doctors + Nursing_Midwifery + log_gdp +
  HALE + lag_HALE

countries <- unique(pdata$Country)

coef_wide <- lapply(countries, function(cty) {
  
  df_c <- pdata %>%
    filter(as.character(Country) == cty) %>%
    na.omit()
  
  if (nrow(df_c) < 5) return(NULL)   # avoid extremely small samples
  
  # Try 3SLS first
  fit <- tryCatch(
    systemfit(system_dyn, method = "3SLS", inst = inst1, data = df_c),
    error = function(e) NULL
  )
  
  method_used <- "3SLS"
  
  # If 3SLS fails → try SUR
  if (is.null(fit)) {
    fit <- tryCatch(
      systemfit(system_dyn, method = "SUR", data = df_c),
      error = function(e) NULL
    )
    method_used <- "SUR"
  }
  
  # If both fail → skip
  if (is.null(fit)) return(NULL)
  
  # Extract coefficients
  out <- data.frame(
    Country = cty,
    Method = method_used,
    t(coef(fit))
  )
  
  return(out)
  
}) %>% bind_rows()

write.csv(coef_wide, "coefficients_by_country_with_fallback.csv", row.names = FALSE)