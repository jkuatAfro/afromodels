


######Loading data


# Load required package
library(readxl)

# Define file path and sheet name
file_path <- "D:/ROI HRH/ROI_DATASET_Compiled_Model.xlsx"
sheet_name <- "COMBINED_Eds"

# Read the SEM sheet into a data frame
glm_data <- read_excel(path = file_path, sheet = sheet_name)

# View the first few rows
head(glm_data)



# Load libraries
library(ggplot2)
library(dplyr)
library(ggrepel)
library(viridis) 



##

# Calculate Spearman's correlation and p-value
spearman_test <- cor.test(glm_data$Density_7_cadres_2023,
                          glm_data$DALYS_2021_All.Causes.per.capita.,
                          method = "spearman")

# Extract values
spearman_rho <- round(spearman_test$estimate, 2)
spearman_pval <- signif(spearman_test$p.value, 3)

# Create annotated label
correlation_label <- paste0("Spearman's ρ = ", spearman_rho,
                            ", p = ", spearman_pval)

# Plot with annotation in red
ggplot(glm_data, aes(x = Density_7_cadres_2023,
                              y = DALYS_2021_All.Causes.per.capita.)) +
  geom_point(aes(color = DALYS_2021_All.Causes.per.capita.), size = 4, alpha = 0.9) +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_smooth(formula = y ~ x, method = "loess", se = FALSE,
              color = "grey30", linewidth = 1.2) +
  annotate("text",
           x = max(glm_data_filtered$Density_7_cadres_2023) * 0.75,
           y = max(glm_data_filtered$DALYS_2021_All.Causes.per.capita.) * 0.95,
           label = correlation_label,
           size = 5, fontface = "italic", color = "red") +
  labs(
    title = "DALYs per Capita vs Density of 7 Core Cadres",
    x = "Density of 7 Core Health Workforce Cadres",
    y = "DALYs per Capita",
    color = "DALYs per Capita"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  scale_color_viridis(option = "D", direction = -1)


#####without Sychelles


# Exclude Seychelles
glm_data_filtered <- glm_data %>% filter(ISO != "SYC")

# Calculate Spearman's correlation and p-value
spearman_test <- cor.test(glm_data_filtered$Density_7_cadres_2023,
                          glm_data_filtered$DALYS_2021_All.Causes.per.capita.,
                          method = "spearman")

# Extract values
spearman_rho <- round(spearman_test$estimate, 2)
spearman_pval <- signif(spearman_test$p.value, 3)

# Create annotated label
correlation_label <- paste0("Spearman's ρ = ", spearman_rho,
                            ", p = ", spearman_pval)

# Plot with annotation in red
ggplot(glm_data_filtered, aes(x = Density_7_cadres_2023,
                              y = DALYS_2021_All.Causes.per.capita.)) +
  geom_point(aes(color = DALYS_2021_All.Causes.per.capita.), size = 4, alpha = 0.9) +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_smooth(formula = y ~ x, method = "loess", se = FALSE,
              color = "grey30", linewidth = 1.2) +
  annotate("text",
           x = max(glm_data_filtered$Density_7_cadres_2023) * 0.75,
           y = max(glm_data_filtered$DALYS_2021_All.Causes.per.capita.) * 0.95,
           label = correlation_label,
           size = 5, fontface = "italic", color = "red") +
  labs(
    title = "DALYs per Capita vs Density of 7 Core Cadres (Excluding SYC)",
    x = "Density of 7 Core Health Workforce Cadres",
    y = "DALYs per Capita",
    color = "DALYs per Capita"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  scale_color_viridis(option = "D", direction = -1)





#####Total DALYs


# Load libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(viridis)

# Spearman correlation and p-value
spearman_test <- cor.test(glm_data$Density_7_cadres_2023,
                          glm_data$Total.DALYs,
                          method = "spearman")

# Extract values
rho <- round(spearman_test$estimate, 2)
pval <- signif(spearman_test$p.value, 3)
cor_label <- paste0("Spearman's ρ = ", rho, ", p = ", pval)

# Create annotated plot
ggplot(glm_data, aes(x = Density_7_cadres_2023, y = Total.DALYs)) +
  geom_point(aes(color = Total.DALYs), size = 4, alpha = 0.9) +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_smooth(formula = y ~ x, method = "loess", se = FALSE,
              color = "grey30", linewidth = 1.2) +
  annotate("text",
           x = max(glm_data$Density_7_cadres_2023) * 0.75,
           y = max(glm_data$Total.DALYs) * 0.95,
           label = cor_label,
           size = 5, fontface = "italic", color = "red") +
  labs(
    title = "Total DALYs vs Density of 7 Core Cadres",
    x = "Density of 7 Core Health Workforce Cadres",
    y = "Total DALYs",
    color = "Total DALYs"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  scale_color_viridis(option = "D", direction = -1)




##########


# Load libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(viridis)



# Compute Spearman's correlation and p-value for SCI_Average
spearman_test <- cor.test(glm_data$Density_7_cadres_2023,
                          glm_data$SCI_Average,
                          method = "spearman")

# Extract values
rho <- round(spearman_test$estimate, 2)
pval <- signif(spearman_test$p.value, 3)
cor_label <- paste0("Spearman's ρ = ", rho, ", p = ", pval)

# Create annotated plot
ggplot(glm_data, aes(x = Density_7_cadres_2023, y = SCI_Average)) +
  geom_point(aes(color = SCI_Average), size = 4, alpha = 0.9) +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_smooth(formula = y ~ x, method = "loess", se = FALSE,
              color = "grey30", linewidth = 1.2) +
  annotate("text",
           x = max(glm_data$Density_7_cadres_2023) * 0.75,
           y = max(glm_data$SCI_Average) * 0.95,
           label = cor_label,
           size = 5, fontface = "italic", color = "red") +
  labs(
    title = "UHC SCI vs Density of 7 Core Health Workforce Cadres",
    x = "Density of 7 Core Health Workforce Cadres",
    y = "UHC SCI",
    color = "UHC SCI"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  scale_color_viridis(option = "D", direction = -1)



######

# Load libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(viridis)


# Load libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(viridis)



# Compute Spearman's correlation and p-value
spearman_test <- cor.test(glm_data$Density_7_cadres_2023,
                          glm_data$GDP_Growth_per_capita,
                          method = "spearman")

# Extract correlation coefficient and p-value
rho <- round(spearman_test$estimate, 2)
pval <- signif(spearman_test$p.value, 3)
cor_label <- paste0("Spearman's ρ = ", rho, ", p = ", pval)

# Create annotated plot with visible correlation label
ggplot(glm_data, aes(x = Density_7_cadres_2023, y = GDP_Growth_per_capita)) +
  geom_point(aes(color = GDP_Growth_per_capita), size = 4, alpha = 0.9) +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_smooth(formula = y ~ x, method = "loess", se = FALSE,
              color = "grey30", linewidth = 1.2) +
  annotate("text",
           x = 100,  # fixed x-position inside plot bounds
           y = 6,  # fixed y-position inside plot bounds
           label = cor_label,
           size = 5, fontface = "italic", color = "red") +
  labs(
    title = "GDP Growth per Capita vs Density of 7 Core Health Workforce Cadres",
    x = "Density of 7 Core Health Workforce Cadres",
    y = "GDP Growth per Capita",
    color = "GDP Growth per Capita"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  scale_color_viridis(option = "D", direction = -1)





#########

# Load libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(viridis)



# Compute Spearman's correlation and p-value for GDP_PPP_2024
spearman_test <- cor.test(glm_data$Density_7_cadres_2023,
                          glm_data$GDP_PPP_2024,
                          method = "spearman")

# Extract correlation coefficient and p-value
rho <- round(spearman_test$estimate, 2)
pval <- signif(spearman_test$p.value, 3)
cor_label <- paste0("Spearman's ρ = ", rho, ", p = ", pval)

# Build the plot
ggplot(glm_data, aes(x = Density_7_cadres_2023, y = GDP_PPP_2024)) +
  geom_point(aes(color = GDP_PPP_2024), size = 4, alpha = 0.9) +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_smooth(formula = y ~ x, method = "loess", se = FALSE,
              color = "grey30", linewidth = 1.2) +
  annotate("text",
           x = 100,        # fixed visible x-position
           y = 35000,     # fixed visible y-position (adjust based on range of your data)
           label = cor_label,
           size = 5, fontface = "italic", color = "red") +
  labs(
    title = "GDP (PPP) vs Density of 7 Core Health Workforce Cadres",
    x = "Density of 7 Core Health Workforce Cadres",
    y = "GDP per Capita (PPP)",
    color = "GDP (PPP)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  scale_color_viridis(option = "D", direction = -1)




######

# Load libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(viridis)



# Compute Spearman correlation and p-value between SCI_Average and DALYs per capita
spearman_test <- cor.test(glm_data$SCI_Average,
                          glm_data$DALYS_2021_All.Causes.per.capita.,
                          method = "spearman")

# Extract results
rho <- round(spearman_test$estimate, 2)
pval <- signif(spearman_test$p.value, 3)
cor_label <- paste0("Spearman's ρ = ", rho, ", p = ", pval)

# Create scatter plot with annotation
ggplot(glm_data, aes(x = SCI_Average, y = DALYS_2021_All.Causes.per.capita.)) +
  geom_point(aes(color = DALYS_2021_All.Causes.per.capita.), size = 4, alpha = 0.9) +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_smooth(formula = y ~ x, method = "loess", se = FALSE,
              color = "grey30", linewidth = 1.2) +
  annotate("text",
           x = 60,  # fixed visible x position — adjust if needed
           y = max(glm_data$DALYS_2021_All.Causes.per.capita.) * 0.9,
           label = cor_label,
           size = 5, fontface = "italic", color = "red") +
  labs(
    title = "DALYs per Capita vs UHC SCI",
    x = "UHC SCI",
    y = "DALYs per Capita",
    color = "DALYs per Capita"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  scale_color_viridis(option = "D", direction = -1)



#########

# Load required libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(viridis)



# Compute Spearman correlation and p-value between SCI_Average and GDP_PPP_2024
spearman_test <- cor.test(glm_data$SCI_Average,
                          glm_data$GDP_PPP_2024,
                          method = "spearman")

# Extract results
rho <- round(spearman_test$estimate, 2)
pval <- signif(spearman_test$p.value, 3)
cor_label <- paste0("Spearman's ρ = ", rho, ", p = ", pval)

# Create annotated scatter plot
ggplot(glm_data, aes(x = SCI_Average, y = GDP_PPP_2024)) +
  geom_point(aes(color = GDP_PPP_2024), size = 4, alpha = 0.9) +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_smooth(formula = y ~ x, method = "loess", se = FALSE,
              color = "grey30", linewidth = 1.2) +
  annotate("text",
           x = 60,  # Adjusted to be safely within SCI range
           y = max(glm_data$GDP_PPP_2024) * 0.9,
           label = cor_label,
           size = 5, fontface = "italic", color = "red") +
  labs(
    title = "GDP (PPP) vs UHC SCI",
    x = "UHC SCI",
    y = "GDP per Capita (PPP)",
    color = "GDP (PPP)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  scale_color_viridis(option = "D", direction = -1)






########

# Load required libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(viridis)



# Compute Spearman correlation and p-value between DALYs per capita and GDP_PPP_2024
spearman_test <- cor.test(glm_data$DALYS_2021_All.Causes.per.capita.,
                          glm_data$GDP_PPP_2024,
                          method = "spearman")

# Extract correlation coefficient and p-value
rho <- round(spearman_test$estimate, 2)
pval <- signif(spearman_test$p.value, 3)
cor_label <- paste0("Spearman's ρ = ", rho, ", p = ", pval)

# Create annotated scatter plot
ggplot(glm_data, aes(x = DALYS_2021_All.Causes.per.capita., y = GDP_PPP_2024)) +
  geom_point(aes(color = GDP_PPP_2024), size = 4, alpha = 0.9) +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_smooth(formula = y ~ x, method = "loess", se = FALSE,
              color = "grey30", linewidth = 1.2) +
  annotate("text",
           x = min(glm_data$DALYS_2021_All.Causes.per.capita.) + 0.2,
           y = max(glm_data$GDP_PPP_2024) * 0.9,
           label = cor_label,
           size = 5, fontface = "italic", color = "red") +
  labs(
    title = "GDP (PPP) vs DALYs per Capita",
    x = "DALYs per Capita",
    y = "GDP per Capita (PPP)",
    color = "GDP (PPP)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  scale_color_viridis(option = "D", direction = -1)






#################################


# Build GLM model
glm_sci <- glm(SCI_Average ~ Density_7_cadres_2023,
               data = glm_data,
               family = gaussian(link = "identity"))

# Summarize model
summary(glm_sci)


# Fit linear GLM
glm_sci <- glm(SCI_Average ~ Density_7_cadres_2023,
               data = glm_data,
               family = gaussian(link = "identity"))

# Generate predicted SCI values
glm_data$Predicted_SCI <- predict(glm_sci, type = "response")

# Plot the observed vs fitted values
ggplot(glm_data, aes(x = Density_7_cadres_2023, y = SCI_Average)) +
  geom_point(size = 4, alpha = 0.9, color = "steelblue") +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_line(aes(y = Predicted_SCI), color = "firebrick", linewidth = 1.2) +
  labs(
    title = "Fitted GLM: SCI Average vs Density of 7 Core Health Workforce Cadres",
    x = "Density of 7 Core Cadres",
    y = "UHC SCI"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank() )




# Load required libraries
library(dplyr)
library(ggplot2)
library(ggrepel)


# Fit GLM model with log-transformed predictor
glm_log <- glm(SCI_Average ~ log(Density_7_cadres_2023),
               data = glm_data, family = gaussian(link = "identity"))

summary(glm_log)

# Predict values and confidence intervals
pred_df <- predict(glm_log, type = "response", se.fit = TRUE)

# Add predictions and CIs to the data frame
glm_data$Predicted_SCI <- pred_df$fit
glm_data$Lower_CI <- pred_df$fit - 1.96 * pred_df$se.fit
glm_data$Upper_CI <- pred_df$fit + 1.96 * pred_df$se.fit

# Plot with confidence ribbon
ggplot(glm_data, aes(x = Density_7_cadres_2023, y = SCI_Average)) +
  geom_point(size = 4, alpha = 0.9, color = "darkcyan") +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_line(aes(y = Predicted_SCI), color = "firebrick", linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI),
              fill = "firebrick", alpha = 0.2) +
  labs(
    title = "Fitted GLM with 95% CI: SCI Average ~ log(Density of 7 Core Cadres)",
    x = "Density of 7 Core Health Workforce Cadres",
    y = "UHC SCI"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  )



####

# Load libraries
library(dplyr)
library(ggplot2)
library(ggrepel)



# Fit the quadratic model
glm_quad <- glm(SCI_Average ~ Density_7_cadres_2023 + I(Density_7_cadres_2023^2),
                data = glm_data, family = gaussian(link = "identity"))

# Generate predictions and confidence intervals
pred_df <- predict(glm_quad, type = "response", se.fit = TRUE)
glm_data$Predicted_SCI <- pred_df$fit
glm_data$Lower_CI <- pred_df$fit - 1.96 * pred_df$se.fit
glm_data$Upper_CI <- pred_df$fit + 1.96 * pred_df$se.fit

# Plot with confidence ribbon
ggplot(glm_data, aes(x = Density_7_cadres_2023, y = SCI_Average)) +
  geom_point(size = 4, alpha = 0.9, color = "steelblue") +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_line(aes(y = Predicted_SCI), color = "firebrick", linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI),
              fill = "firebrick", alpha = 0.2) +
  labs(
    title = "Fitted Quadratic GLM with 95% CI: SCI Average ~ Density + Density²",
    x = "Density of 7 Core Health Workforce Cadres",
    y = "UHC SCI"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank() )


#########


# Fit the GLM
glm_dalys <- glm(DALYS_2021_All.Causes.per.capita. ~ Density_7_cadres_2023,
                 data = glm_data,
                 family = gaussian(link = "identity"))

# View model summary
summary(glm_dalys)


# Load required libraries
library(dplyr)
library(ggplot2)
library(ggrepel)



# Fit GLM: DALYs per capita ~ Cadre density
glm_dalys <- glm(DALYS_2021_All.Causes.per.capita. ~ Density_7_cadres_2023,
                 data = glm_data,
                 family = gaussian(link = "identity"))

# Predict DALYs with standard errors for CI
pred <- predict(glm_dalys, type = "response", se.fit = TRUE)
glm_data$Predicted_DALYs <- pred$fit
glm_data$Lower_CI <- pred$fit - 1.96 * pred$se.fit
glm_data$Upper_CI <- pred$fit + 1.96 * pred$se.fit

# Plot the fitted model with confidence ribbon
ggplot(glm_data, aes(x = Density_7_cadres_2023, y = DALYS_2021_All.Causes.per.capita.)) +
  geom_point(size = 4, alpha = 0.9, color = "darkorchid") +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_line(aes(y = Predicted_DALYs), color = "firebrick", linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI),
              fill = "firebrick", alpha = 0.2) +
  labs(
    title = "Fitted GLM with 95% CI: DALYs per Capita ~ Density of 7 Core Cadres",
    x = "Density of 7 Core Health Workforce Cadres",
    y = "DALYs per Capita"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()  )



glm_log_dalys <- glm(log(DALYS_2021_All.Causes.per.capita.) ~ Density_7_cadres_2023,
                     data = glm_data, family = gaussian(link = "identity"))
summary(glm_log_dalys)



#####

# Load required libraries
library(dplyr)
library(ggplot2)
library(ggrepel)


# Fit the log-linear GLM
glm_log_dalys <- glm(log(DALYS_2021_All.Causes.per.capita.) ~ Density_7_cadres_2023,
                     data = glm_data, family = gaussian(link = "identity"))

# Predict log(DALYs) with standard errors
pred <- predict(glm_log_dalys, type = "response", se.fit = TRUE)
glm_data$Predicted_log_DALYs <- pred$fit
glm_data$Lower_CI <- pred$fit - 1.96 * pred$se.fit
glm_data$Upper_CI <- pred$fit + 1.96 * pred$se.fit

# Convert back to DALYs per capita scale for plotting
glm_data$Predicted_DALYs <- exp(glm_data$Predicted_log_DALYs)
glm_data$Lower_CI_exp <- exp(glm_data$Lower_CI)
glm_data$Upper_CI_exp <- exp(glm_data$Upper_CI)

# Plot: DALYs per capita vs Cadre Density with fitted model
ggplot(glm_data, aes(x = Density_7_cadres_2023, y = DALYS_2021_All.Causes.per.capita.)) +
  geom_point(size = 4, alpha = 0.9, color = "darkgreen") +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_line(aes(y = Predicted_DALYs), color = "firebrick", linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower_CI_exp, ymax = Upper_CI_exp),
              fill = "firebrick", alpha = 0.2) +
  labs(
    title = "Fitted GLM with 95% CI: log(DALYs per Capita) ~ Cadre Density",
    x = "Density of 7 Core Health Workforce Cadres (2023)",
    y = "DALYs per Capita"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()  )




######Gamma


# Load required libraries
library(dplyr)
library(ggplot2)
library(ggrepel)



# Fit Gamma GLM with log link
glm_gamma_dalys <- glm(DALYS_2021_All.Causes.per.capita. ~ Density_7_cadres_2023,
                       data = glm_data, family = Gamma(link = "log"))

# Predict fitted values with standard errors
pred <- predict(glm_gamma_dalys, type = "link", se.fit = TRUE)

# Back-transform predictions and confidence intervals
glm_data$Predicted_DALYs <- exp(pred$fit)
glm_data$Lower_CI <- exp(pred$fit - 1.96 * pred$se.fit)
glm_data$Upper_CI <- exp(pred$fit + 1.96 * pred$se.fit)

# Plot the fitted Gamma model
ggplot(glm_data, aes(x = Density_7_cadres_2023, y = DALYS_2021_All.Causes.per.capita.)) +
  geom_point(size = 4, alpha = 0.9, color = "darkorange") +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_line(aes(y = Predicted_DALYs), color = "firebrick", linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI),
              fill = "firebrick", alpha = 0.2) +
  labs(
    title = "Fitted Gamma GLM with 95% CI: DALYs per Capita ~ Cadre Density",
    x = "Density of 7 Core Health Workforce Cadres (2023)",
    y = "DALYs per Capita"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  )




##########################


# Fit GLM: GDP PPP as outcome, Cadre Density as predictor
glm_gdp <- glm(GDP_PPP_2024 ~ Density_7_cadres_2023,
               data = glm_data,
               family = gaussian(link = "identity"))

# Summarize the model
summary(glm_gdp)



# Load required libraries
library(dplyr)
library(ggplot2)
library(ggrepel)


# Fit GLM model
glm_gdp <- glm(GDP_PPP_2024 ~ Density_7_cadres_2023,
               data = glm_data, family = gaussian(link = "identity"))

# Generate predicted values and confidence intervals
pred <- predict(glm_gdp, type = "response", se.fit = TRUE)
glm_data$Predicted_GDP <- pred$fit
glm_data$Lower_CI <- pred$fit - 1.96 * pred$se.fit
glm_data$Upper_CI <- pred$fit + 1.96 * pred$se.fit

# Plot with confidence ribbon
ggplot(glm_data, aes(x = Density_7_cadres_2023, y = GDP_PPP_2024)) +
  geom_point(size = 4, alpha = 0.9, color = "royalblue") +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_line(aes(y = Predicted_GDP), color = "firebrick", linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI),
              fill = "firebrick", alpha = 0.2) +
  labs(
    title = "Fitted GLM with 95% CI: GDP (PPP, 2024) ~ Cadre Density",
    x = "Density of 7 Core Health Workforce Cadres (2023)",
    y = "GDP per Capita (PPP, 2024)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  )




#####

glm_log_gdp <- glm(log(GDP_PPP_2024) ~ Density_7_cadres_2023,
                   data = glm_data, family = gaussian(link = "identity"))

summary(glm_log_gdp)


# Load libraries
library(dplyr)
library(ggplot2)
library(ggrepel)



# Fit GLM: log-transformed GDP ~ Cadre Density
glm_log_gdp <- glm(log(GDP_PPP_2024) ~ Density_7_cadres_2023,
                   data = glm_data, family = gaussian(link = "identity"))

# Predict log(GDP) with standard errors
pred <- predict(glm_log_gdp, type = "response", se.fit = TRUE)

# Back-transform predictions to GDP scale
glm_data$Predicted_GDP <- exp(pred$fit)
glm_data$Lower_CI <- exp(pred$fit - 1.96 * pred$se.fit)
glm_data$Upper_CI <- exp(pred$fit + 1.96 * pred$se.fit)

# Plot with confidence ribbon
ggplot(glm_data, aes(x = Density_7_cadres_2023, y = GDP_PPP_2024)) +
  geom_point(size = 4, alpha = 0.9, color = "darkslateblue") +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_line(aes(y = Predicted_GDP), color = "firebrick", linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI),
              fill = "firebrick", alpha = 0.2) +
  labs(
    title = "Fitted Log-Linear GLM with 95% CI: log(GDP PPP 2024) ~ Cadre Density",
    x = "Density of 7 Core Health Workforce Cadres (2023)",
    y = "GDP per Capita (PPP, 2024)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  )


glm_quad_gdp <- glm(GDP_PPP_2024 ~ Density_7_cadres_2023 + I(Density_7_cadres_2023^2),
                    data = glm_data, family = gaussian(link = "identity"))

summary(glm_quad_gdp)
######################

glm_loglog_gdp <- glm(log(GDP_PPP_2024) ~ log(Density_7_cadres_2023),
                      data = glm_data, family = gaussian(link = "identity"))

summary(glm_loglog_gdp)



# Load required libraries
library(dplyr)
library(ggplot2)
library(ggrepel)



# Fit log-log GLM
glm_loglog_gdp <- glm(log(GDP_PPP_2024) ~ log(Density_7_cadres_2023),
                      data = glm_data, family = gaussian(link = "identity"))

# Predict log(GDP) with standard errors
pred <- predict(glm_loglog_gdp, type = "response", se.fit = TRUE)

# Back-transform to original GDP scale
glm_data$Predicted_GDP <- exp(pred$fit)
glm_data$Lower_CI <- exp(pred$fit - 1.96 * pred$se.fit)
glm_data$Upper_CI <- exp(pred$fit + 1.96 * pred$se.fit)

# Plot observed vs fitted model with confidence interval
ggplot(glm_data, aes(x = Density_7_cadres_2023, y = GDP_PPP_2024)) +
  geom_point(size = 4, alpha = 0.9, color = "forestgreen") +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_line(aes(y = Predicted_GDP), color = "firebrick", linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI),
              fill = "firebrick", alpha = 0.2) +
  labs(
    title = "Fitted Log-Log GLM with 95% CI: GDP (PPP, 2024) ~ log(Cadre Density)",
    x = "Density of 7 Core Health Workforce Cadres (2023)",
    y = "GDP per Capita (PPP, 2024)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  )




######

glm_gamma_gdp <- glm(GDP_PPP_2024 ~ Density_7_cadres_2023,
                     data = glm_data, family = Gamma(link = "log"))

summary(glm_gamma_gdp)

# Load libraries
library(dplyr)
library(ggplot2)
library(ggrepel)



# Fit Gamma GLM with log link
glm_gamma_gdp <- glm(GDP_PPP_2024 ~ Density_7_cadres_2023,
                     data = glm_data, family = Gamma(link = "log"))

# Generate link-scale predictions and standard errors
pred <- predict(glm_gamma_gdp, type = "link", se.fit = TRUE)

# Back-transform predictions to original GDP scale
glm_data$Predicted_GDP <- exp(pred$fit)
glm_data$Lower_CI <- exp(pred$fit - 1.96 * pred$se.fit)
glm_data$Upper_CI <- exp(pred$fit + 1.96 * pred$se.fit)

# Plot fitted curve and confidence interval
ggplot(glm_data, aes(x = Density_7_cadres_2023, y = GDP_PPP_2024)) +
  geom_point(size = 4, alpha = 0.9, color = "darkgoldenrod") +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_line(aes(y = Predicted_GDP), color = "firebrick", linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI),
              fill = "firebrick", alpha = 0.2) +
  labs(
    title = "Fitted Gamma GLM with 95% CI: GDP (PPP, 2024) ~ Cadre Density",
    x = "Density of 7 Core Health Workforce Cadres (2023)",
    y = "GDP per Capita (PPP, 2024)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  )



#######################


# Fit the model
glm_dalys_sci <- glm(DALYS_2021_All.Causes.per.capita. ~ SCI_Average,
                     data = glm_data,
                     family = gaussian(link = "identity"))

# Summarize model
summary(glm_dalys_sci)

# Load required libraries
library(dplyr)
library(ggplot2)
library(ggrepel)


# Fit the model
glm_dalys_sci <- glm(DALYS_2021_All.Causes.per.capita. ~ SCI_Average,
                     data = glm_data, family = gaussian(link = "identity"))

# Generate predicted values and standard errors
pred <- predict(glm_dalys_sci, type = "response", se.fit = TRUE)
glm_data$Predicted_DALYs <- pred$fit
glm_data$Lower_CI <- pred$fit - 1.96 * pred$se.fit
glm_data$Upper_CI <- pred$fit + 1.96 * pred$se.fit

# Plot observed vs fitted model
ggplot(glm_data, aes(x = SCI_Average, y = DALYS_2021_All.Causes.per.capita.)) +
  geom_point(size = 4, alpha = 0.9, color = "darkmagenta") +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_line(aes(y = Predicted_DALYs), color = "firebrick", linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI),
              fill = "firebrick", alpha = 0.2) +
  labs(
    title = "Fitted GLM with 95% CI: DALYs per Capita ~ SCI_Average",
    x = "UHC SCI",
    y = "DALYs per Capita (2021, All Causes)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()  )





######

glm_log_dalys_sci <- glm(log(DALYS_2021_All.Causes.per.capita.) ~ SCI_Average,
                         data = glm_data, family = gaussian(link = "identity"))


summary(glm_log_dalys_sci)



# Fit GLM: GDP PPP as outcome, SCI Average as predictor
glm_gdp_sci <- glm(GDP_PPP_2024 ~ SCI_Average,
                   data = glm_data,
                   family = gaussian(link = "identity"))

# Model summary
summary(glm_gdp_sci)


#####

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(ggrepel)



# Fit the GLM
glm_gdp_sci <- glm(GDP_PPP_2024 ~ SCI_Average,
                   data = glm_data, family = gaussian(link = "identity"))

# Generate predictions and 95% CI
pred <- predict(glm_gdp_sci, type = "response", se.fit = TRUE)
glm_data$Predicted_GDP <- pred$fit
glm_data$Lower_CI <- pred$fit - 1.96 * pred$se.fit
glm_data$Upper_CI <- pred$fit + 1.96 * pred$se.fit

# Plot results
ggplot(glm_data, aes(x = SCI_Average, y = GDP_PPP_2024)) +
  geom_point(size = 4, alpha = 0.9, color = "deepskyblue4") +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_line(aes(y = Predicted_GDP), color = "firebrick", linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI),
              fill = "firebrick", alpha = 0.2) +
  labs(
    title = "Fitted GLM with 95% CI: GDP (PPP) ~ SCI Average",
    x = "UHC SCI",
    y = "GDP per Capita (PPP)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()  )



#####


glm_log_gdp_sci <- glm(log(GDP_PPP_2024) ~ SCI_Average,
                       data = glm_data, family = gaussian(link = "identity"))


summary(glm_log_gdp_sci)



# Load libraries
library(dplyr)
library(ggplot2)
library(ggrepel)



# Fit the GLM
glm_log_gdp_sci <- glm(log(GDP_PPP_2024) ~ SCI_Average,
                       data = glm_data, family = gaussian(link = "identity"))

# Predict log(GDP) with standard errors
pred <- predict(glm_log_gdp_sci, type = "response", se.fit = TRUE)

# Back-transform predictions to original GDP scale
glm_data$Predicted_GDP <- exp(pred$fit)
glm_data$Lower_CI <- exp(pred$fit - 1.96 * pred$se.fit)
glm_data$Upper_CI <- exp(pred$fit + 1.96 * pred$se.fit)

# Plot: SCI vs GDP PPP (back-transformed)
ggplot(glm_data, aes(x = SCI_Average, y = GDP_PPP_2024)) +
  geom_point(size = 4, alpha = 0.9, color = "darkturquoise") +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_line(aes(y = Predicted_GDP), color = "firebrick", linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI),
              fill = "firebrick", alpha = 0.2) +
  labs(
    title = "Fitted Log-Linear GLM with 95% CI: log(GDP PPP) ~ SCI_Average",
    x = "UHC SCI ",
    y = "GDP per Capita (PPP)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank())




# Load necessary libraries
library(dplyr)
library(ggplot2)
library(ggrepel)



# Fit quadratic GLM
glm_quad_gdp_sci <- glm(GDP_PPP_2024 ~ SCI_Average + I(SCI_Average^2),
                        data = glm_data, family = gaussian(link = "identity"))

# Predict fitted values and standard errors
pred <- predict(glm_quad_gdp_sci, type = "response", se.fit = TRUE)
glm_data$Predicted_GDP <- pred$fit
glm_data$Lower_CI <- pred$fit - 1.96 * pred$se.fit
glm_data$Upper_CI <- pred$fit + 1.96 * pred$se.fit

# Plot quadratic fit with confidence ribbon
ggplot(glm_data, aes(x = SCI_Average, y = GDP_PPP_2024)) +
  geom_point(size = 4, alpha = 0.9, color = "mediumvioletred") +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_line(aes(y = Predicted_GDP), color = "firebrick", linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), 
              fill = "firebrick", alpha = 0.2) +
  labs(
    title = "Fitted Quadratic GLM with 95% CI: GDP (PPP, 2024) ~ SCI_Average²",
    x = "SCI Average (Service Capacity & Access)",
    y = "GDP per Capita (PPP, 2024)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  )
####


# Load libraries
library(dplyr)
library(ggplot2)
library(ggrepel)



# Fit the Gamma GLM
glm_gamma_gdp_sci <- glm(GDP_PPP_2024 ~ SCI_Average,
                         data = glm_data, family = Gamma(link = "log"))

# Predict on the link scale with standard errors
pred <- predict(glm_gamma_gdp_sci, type = "link", se.fit = TRUE)

# Back-transform predictions and CI to GDP scale
glm_data$Predicted_GDP <- exp(pred$fit)
glm_data$Lower_CI <- exp(pred$fit - 1.96 * pred$se.fit)
glm_data$Upper_CI <- exp(pred$fit + 1.96 * pred$se.fit)

# Plot the fitted Gamma model
ggplot(glm_data, aes(x = SCI_Average, y = GDP_PPP_2024)) +
  geom_point(size = 4, alpha = 0.9, color = "seagreen") +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_line(aes(y = Predicted_GDP), color = "firebrick", linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI),
              fill = "firebrick", alpha = 0.2) +
  labs(
    title = "Fitted Gamma GLM with 95% CI: GDP (PPP) ~ SCI Average",
    x = "UHC SCI",
    y = "GDP per Capita (PPP)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()  )





# Fit the model
glm_gdp_dalys <- glm(GDP_PPP_2024 ~ DALYS_2021_All.Causes.per.capita.,
                     data = glm_data, family = gaussian(link = "identity"))

# View model summary
summary(glm_gdp_dalys)

# Load required libraries
library(dplyr)
library(ggplot2)
library(ggrepel)



# Fit the GLM
glm_gdp_dalys <- glm(GDP_PPP_2024 ~ DALYS_2021_All.Causes.per.capita.,
                     data = glm_data, family = gaussian(link = "identity"))

# Predict fitted values and standard errors
pred <- predict(glm_gdp_dalys, type = "response", se.fit = TRUE)
glm_data$Predicted_GDP <- pred$fit
glm_data$Lower_CI <- pred$fit - 1.96 * pred$se.fit
glm_data$Upper_CI <- pred$fit + 1.96 * pred$se.fit

# Plot the fitted model
ggplot(glm_data, aes(x = DALYS_2021_All.Causes.per.capita., y = GDP_PPP_2024)) +
  geom_point(size = 4, alpha = 0.9, color = "steelblue") +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_line(aes(y = Predicted_GDP), color = "firebrick", linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI),
              fill = "firebrick", alpha = 0.2) +
  labs(
    title = "Fitted GLM with 95% CI: GDP (PPP) ~ DALYs per Capita",
    x = "DALYs per Capita (All Causes)",
    y = "GDP per Capita (PPP)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()  )



glm_log_gdp_dalys <- glm(log(GDP_PPP_2024) ~ DALYS_2021_All.Causes.per.capita.,
                         data = glm_data, family = gaussian(link = "identity"))


# Load libraries
library(dplyr)
library(ggplot2)
library(ggrepel)



# Fit the model
glm_log_gdp_dalys <- glm(log(GDP_PPP_2024) ~ DALYS_2021_All.Causes.per.capita.,
                         data = glm_data, family = gaussian(link = "identity"))

# Predict log(GDP) with standard errors
pred <- predict(glm_log_gdp_dalys, type = "response", se.fit = TRUE)

# Back-transform to original GDP scale
glm_data$Predicted_GDP <- exp(pred$fit)
glm_data$Lower_CI <- exp(pred$fit - 1.96 * pred$se.fit)
glm_data$Upper_CI <- exp(pred$fit + 1.96 * pred$se.fit)

# Plot results
ggplot(glm_data, aes(x = DALYS_2021_All.Causes.per.capita., y = GDP_PPP_2024)) +
  geom_point(size = 4, alpha = 0.9, color = "dodgerblue4") +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_line(aes(y = Predicted_GDP), color = "firebrick", linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI),
              fill = "firebrick", alpha = 0.2) +
  labs(
    title = "Fitted Log-Linear GLM with 95% CI: log(GDP PPP) ~ DALYs per Capita",
    x = "DALYs per Capita (All Causes)",
    y = "GDP per Capita (PPP)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()  )






# Load required libraries
library(dplyr)
library(ggplot2)
library(ggrepel)



# Fit log-log GLM
glm_loglog_gdp_dalys <- glm(log(GDP_PPP_2024) ~ log(DALYS_2021_All.Causes.per.capita.),
                            data = glm_data, family = gaussian(link = "identity"))

# Predict log(GDP) and transform back with CI
pred <- predict(glm_loglog_gdp_dalys, type = "response", se.fit = TRUE)
glm_data$Predicted_GDP <- exp(pred$fit)
glm_data$Lower_CI <- exp(pred$fit - 1.96 * pred$se.fit)
glm_data$Upper_CI <- exp(pred$fit + 1.96 * pred$se.fit)

# Plot
ggplot(glm_data, aes(x = DALYS_2021_All.Causes.per.capita., y = GDP_PPP_2024)) +
  geom_point(size = 4, alpha = 0.9, color = "darkorange3") +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_line(aes(y = Predicted_GDP), color = "firebrick", linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI),
              fill = "firebrick", alpha = 0.2) +
  labs(
    title = "Fitted Log-Log GLM with 95% CI: GDP (PPP) ~ log(DALYs per Capita)",
    x = "DALYs per Capita (All Causes)",
    y = "GDP per Capita (PPP)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()  )





# Load necessary libraries
library(dplyr)
library(ggplot2)
library(ggrepel)



# Fit the Gamma GLM with log link
glm_gamma_gdp_dalys <- glm(GDP_PPP_2024 ~ DALYS_2021_All.Causes.per.capita.,
                           data = glm_data, family = Gamma(link = "log"))

# Predict on the link scale with standard errors
pred <- predict(glm_gamma_gdp_dalys, type = "link", se.fit = TRUE)

# Back-transform predictions to GDP scale
glm_data$Predicted_GDP <- exp(pred$fit)
glm_data$Lower_CI <- exp(pred$fit - 1.96 * pred$se.fit)
glm_data$Upper_CI <- exp(pred$fit + 1.96 * pred$se.fit)

# Plot with confidence interval ribbon
ggplot(glm_data, aes(x = DALYS_2021_All.Causes.per.capita., y = GDP_PPP_2024)) +
  geom_point(size = 4, alpha = 0.9, color = "darkorchid4") +
  geom_text_repel(aes(label = ISO), size = 4.2, max.overlaps = 50) +
  geom_line(aes(y = Predicted_GDP), color = "firebrick", linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), 
              fill = "firebrick", alpha = 0.2) +
  labs(
    title = "Fitted Gamma GLM with 95% CI: GDP (PPP) ~ DALYs per Capita",
    x = "DALYs per Capita (All Causes)",
    y = "GDP per Capita (PPP)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()  )

