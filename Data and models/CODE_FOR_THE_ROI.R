


######Loading data


# Load required package
library(readxl)

# Define file path and sheet name
file_path <- "C:/Users/kbediakon/OneDrive - World Health Organization/AFRO HWF UNIT/HWF RETURN ON INVESTMENT/ROI_DATASET_Compiled_Model.xlsx"
sheet_name <- "SEM_HWF"

# Read the SEM sheet into a data frame
sem_data <- read_excel(path = file_path, sheet = sheet_name)

# View the first few rows
head(sem_data)



###################################

### SEM in R package

###################################

library(seminr)
library(rsvg)

library(plotly)
library(ggplot2)

library(DiagrammeR)




###########################################################################


#######
####  SEM model

###HRH Investment--->Output---->Outcomes---->Impacts

#################################without the levers

###1. Create a measurement model

phc_rep_mm <- constructs(
  composite("HRH_AVAILABILITY",        multi_items("HR_", 1:7)),
  composite("OUTPUT", multi_items("Output_", 1:3)),
  composite("OUTCOME", multi_items("Outcome_", 1:3)),
  composite("IMPACT", multi_items("Impact_", 1)))

#####2. Create a structural model


phc_rep_sm <- relationships(
  paths(from = c("HRH_AVAILABILITY"), to = c("OUTPUT")),
  paths(from = c("OUTPUT"), to = c("OUTCOME")),
  paths(from = c("OUTCOME"), to = c("IMPACT")))


####3. Estimating the model

phc_rep_pls_model <- estimate_pls(data = sem_data,
                                  measurement_model = phc_rep_mm,
                                  structural_model  = phc_rep_sm,
                                  inner_weights = path_weighting,
                                  missing = mean_replacement,
                                  missing_value = "-99")

####4. Summarizing the model

summary_phc_rep <- summary(phc_rep_pls_model)
summary_phc_rep

summary_phc_rep$paths
summary_phc_rep$reliability


plot(summary_phc_rep$reliability)


#### to check if it converged

summary_phc_rep$iterations


###summary statistics

summary_phc_rep$descriptives$statistics$items


summary_phc_rep$descriptives$statistics$constructs



###### 5. Bootstrapping the model

# Step 1: Create a custom temp folder if not exists
dir.create("C:/Temp", showWarnings = FALSE)

# Step 2: Force R to use your custom directory across its session
Sys.setenv(TMP = "C:/Temp", TMPDIR = "C:/Temp")

# Step 3: Redefine R’s internal tempdir function
assignInNamespace("tempdir", function() "C:/Temp", ns = "base")

# Step 4: Try creating the cluster again
library(parallel)
cl <- makeCluster(parallel::detectCores() - 1)

##To obtain the standard errors and confidence intervals

boot_phc_rep <- bootstrap_model(seminr_model = phc_rep_pls_model,
                                nboot = 1000,
                                cores = NULL,
                                seed = 123)


sum_boot_phc_rep <- summary(boot_phc_rep)

sum_boot_phc_rep


##### Evaluation of reflective measurement models



#a. Indicator reliability: 0.708 (retain), 0.4 - 0.7(may retain), below 0.4(remove them)
summary_phc_rep$loadings

### b. Internal consistency reliability  (min 0.6 for exploratory research)

summary_phc_rep$reliability

plot(summary_phc_rep$reliability)

####c. Convergent validity: The average variance extracted (AVE) is the mean of a construct indicator’s squared loadings. The minimum acceptable AVE is 0.50 or higher

summary_phc_rep$validity$fl_criteria

plot(boot_phc_rep, size=3)


#plot_ly(plot(boot_phc_rep, size=3))


######Loading data


# Load required package
library(readxl)

# Define file path and sheet name
file_path <- "C:/Users/kbediakon/OneDrive - World Health Organization/AFRO HWF UNIT/HWF RETURN ON INVESTMENT/ROI_DATASET_Compiled_Model.xlsx"
sheet_name <- "SEM_HWF"

# Read the SEM sheet into a data frame
sem_data <- read_excel(path = file_path, sheet = sheet_name)

# View the first few rows
head(sem_data)



###################################

### SEM in R package

###################################

library(seminr)
library(rsvg)

library(plotly)
library(ggplot2)

library(DiagrammeR)




###########################################################################


#######
####  SEM model 2

###HRH Investment--->Output---->Outcomes---->Impacts

#################################without the levers

###1. Create a measurement model

phc_rep_mm <- constructs(
  composite("HRH_AVAILABILITY",        multi_items("HR_", 1:7)),
  composite("OUTPUT", multi_items("Output_", 1:3)),
  composite("OUTCOME", multi_items("Outcome_", 1:3)),
  composite("IMPACT", multi_items("Impact_", 1)))

#####2. Create a structural model


phc_rep_sm <- relationships(
  paths(from = c("HRH_AVAILABILITY"), to = c("OUTPUT")),
  paths(from = c("HRH_AVAILABILITY"), to = c("OUTCOME")),
  paths(from = c("OUTPUT"), to = c("OUTCOME")),
  paths(from = c("OUTCOME"), to = c("IMPACT")))


####3. Estimating the model

phc_rep_pls_model <- estimate_pls(data = sem_data,
                                  measurement_model = phc_rep_mm,
                                  structural_model  = phc_rep_sm,
                                  inner_weights = path_weighting,
                                  missing = mean_replacement,
                                  missing_value = "-99")

####4. Summarizing the model

summary_phc_rep <- summary(phc_rep_pls_model)
summary_phc_rep

summary_phc_rep$paths
summary_phc_rep$reliability


plot(summary_phc_rep$reliability)


#### to check if it converged

summary_phc_rep$iterations


###summary statistics

summary_phc_rep$descriptives$statistics$items


summary_phc_rep$descriptives$statistics$constructs



###### 5. Bootstrapping the model

# Step 1: Create a custom temp folder if not exists
dir.create("C:/Temp", showWarnings = FALSE)

# Step 2: Force R to use your custom directory across its session
Sys.setenv(TMP = "C:/Temp", TMPDIR = "C:/Temp")

# Step 3: Redefine R’s internal tempdir function
assignInNamespace("tempdir", function() "C:/Temp", ns = "base")

# Step 4: Try creating the cluster again
library(parallel)
cl <- makeCluster(parallel::detectCores() - 1)

##To obtain the standard errors and confidence intervals

boot_phc_rep <- bootstrap_model(seminr_model = phc_rep_pls_model,
                                nboot = 1000,
                                cores = NULL,
                                seed = 123)


sum_boot_phc_rep <- summary(boot_phc_rep)

sum_boot_phc_rep


##### Evaluation of reflective measurement models



#a. Indicator reliability: 0.708 (retain), 0.4 - 0.7(may retain), below 0.4(remove them)
summary_phc_rep$loadings

### b. Internal consistency reliability  (min 0.6 for exploratory research)

summary_phc_rep$reliability

plot(summary_phc_rep$reliability)

####c. Convergent validity: The average variance extracted (AVE) is the mean of a construct indicator’s squared loadings. The minimum acceptable AVE is 0.50 or higher

summary_phc_rep$validity$fl_criteria

plot(boot_phc_rep, size=3)


#plot_ly(plot(boot_phc_rep, size=3))


###########################################################################


#######
####  SEM model 3

###HRH Investment--->Output---->Outcomes---->Impacts

#################################without the levers

###1. Create a measurement model

phc_rep_mm <- constructs(
  composite("HRH_AVAILABILITY",        multi_items("HR_", 1:7)),
  composite("OUTPUT", multi_items("Output_", 1:3)),
  composite("OUTCOME", multi_items("Outcome_", 1:3)),
  composite("IMPACT", multi_items("Impact_", 1)))

#####2. Create a structural model


phc_rep_sm <- relationships(
  paths(from = c("HRH_AVAILABILITY"), to = c("OUTPUT")),
  paths(from = c("HRH_AVAILABILITY"), to = c("OUTCOME")),
  paths(from = c("HRH_AVAILABILITY"), to = c("IMPACT")),
  paths(from = c("OUTPUT"), to = c("OUTCOME")),
  paths(from = c("OUTPUT"), to = c("IMPACT")),
  paths(from = c("OUTCOME"), to = c("IMPACT")))


####3. Estimating the model

phc_rep_pls_model <- estimate_pls(data = sem_data,
                                  measurement_model = phc_rep_mm,
                                  structural_model  = phc_rep_sm,
                                  inner_weights = path_weighting,
                                  missing = mean_replacement,
                                  missing_value = "-99")

####4. Summarizing the model

summary_phc_rep <- summary(phc_rep_pls_model)
summary_phc_rep

summary_phc_rep$paths
summary_phc_rep$reliability


plot(summary_phc_rep$reliability)


#### to check if it converged

summary_phc_rep$iterations


###summary statistics

summary_phc_rep$descriptives$statistics$items


summary_phc_rep$descriptives$statistics$constructs



###### 5. Bootstrapping the model

# Step 1: Create a custom temp folder if not exists
dir.create("C:/Temp", showWarnings = FALSE)

# Step 2: Force R to use your custom directory across its session
Sys.setenv(TMP = "C:/Temp", TMPDIR = "C:/Temp")

# Step 3: Redefine R’s internal tempdir function
assignInNamespace("tempdir", function() "C:/Temp", ns = "base")

# Step 4: Try creating the cluster again
library(parallel)
cl <- makeCluster(parallel::detectCores() - 1)

##To obtain the standard errors and confidence intervals

boot_phc_rep <- bootstrap_model(seminr_model = phc_rep_pls_model,
                                nboot = 1000,
                                cores = NULL,
                                seed = 123)


sum_boot_phc_rep <- summary(boot_phc_rep)

sum_boot_phc_rep


##### Evaluation of reflective measurement models



#a. Indicator reliability: 0.708 (retain), 0.4 - 0.7(may retain), below 0.4(remove them)
summary_phc_rep$loadings

### b. Internal consistency reliability  (min 0.6 for exploratory research)

summary_phc_rep$reliability

plot(summary_phc_rep$reliability)

####c. Convergent validity: The average variance extracted (AVE) is the mean of a construct indicator’s squared loadings. The minimum acceptable AVE is 0.50 or higher

summary_phc_rep$validity$fl_criteria

plot(boot_phc_rep, size=3)


#plot_ly(plot(boot_phc_rep, size=3))

###########################################################################


#######
####  SEM model 4

###HRH Investment--->Output---->Outcomes---->Impacts

#################################without the levers


# Load required package
library(readxl)

# Define file path and sheet name
file_path <- "C:/Users/kbediakon/OneDrive - World Health Organization/AFRO HWF UNIT/HWF RETURN ON INVESTMENT/ROI_DATASET_Compiled_Model.xlsx"
sheet_name <- "SEM_HWF"

# Read the SEM sheet into a data frame
sem_data <- read_excel(path = file_path, sheet = sheet_name)

# View the first few rows
head(sem_data)

###1. Create a measurement model

phc_rep_mm <- constructs(
  composite("HRH_AVAILABILITY",        multi_items("HR_", 1:7)),
  composite("OUTPUT", multi_items("Output_", 1:3)),
  composite("OUTCOME", multi_items("Outcome_", 1:3)),
  composite("IMPACT", multi_items("Impact_", 1)))

#####2. Create a structural model


phc_rep_sm <- relationships(
  paths(from = c("HRH_AVAILABILITY"), to = c("OUTPUT")),
  paths(from = c("HRH_AVAILABILITY"), to = c("OUTCOME")),
  paths(from = c("HRH_AVAILABILITY"), to = c("IMPACT")),
  paths(from = c("OUTPUT"), to = c("OUTCOME")),
  paths(from = c("OUTPUT"), to = c("IMPACT")),
  paths(from = c("OUTCOME"), to = c("IMPACT")))


####3. Estimating the model

phc_rep_pls_model <- estimate_pls(data = sem_data,
                                  measurement_model = phc_rep_mm,
                                  structural_model  = phc_rep_sm,
                                  inner_weights = path_weighting,
                                  missing = mean_replacement,
                                  missing_value = "-99")

####4. Summarizing the model

summary_phc_rep <- summary(phc_rep_pls_model)
summary_phc_rep

summary_phc_rep$paths
summary_phc_rep$reliability


plot(summary_phc_rep$reliability)


#### to check if it converged

summary_phc_rep$iterations


###summary statistics

summary_phc_rep$descriptives$statistics$items


summary_phc_rep$descriptives$statistics$constructs



###### 5. Bootstrapping the model

# Step 1: Create a custom temp folder if not exists
dir.create("C:/Temp", showWarnings = FALSE)

# Step 2: Force R to use your custom directory across its session
Sys.setenv(TMP = "C:/Temp", TMPDIR = "C:/Temp")

# Step 3: Redefine R’s internal tempdir function
assignInNamespace("tempdir", function() "C:/Temp", ns = "base")

# Step 4: Try creating the cluster again
library(parallel)
cl <- makeCluster(parallel::detectCores() - 1)

##To obtain the standard errors and confidence intervals

boot_phc_rep <- bootstrap_model(seminr_model = phc_rep_pls_model,
                                nboot = 1000,
                                cores = NULL,
                                seed = 123)


sum_boot_phc_rep <- summary(boot_phc_rep)

sum_boot_phc_rep


##### Evaluation of reflective measurement models



#a. Indicator reliability: 0.708 (retain), 0.4 - 0.7(may retain), below 0.4(remove them)
summary_phc_rep$loadings

### b. Internal consistency reliability  (min 0.6 for exploratory research)

summary_phc_rep$reliability

plot(summary_phc_rep$reliability)

####c. Convergent validity: The average variance extracted (AVE) is the mean of a construct indicator’s squared loadings. The minimum acceptable AVE is 0.50 or higher

summary_phc_rep$validity$fl_criteria

plot(boot_phc_rep, size=3)


#plot_ly(plot(boot_phc_rep, size=3))


###########################################################################


#######
####  SEM simple model

###HRH Investment--->Output---->Outcomes---->Impacts

#################################without the levers


# Load required package
library(readxl)

# Define file path and sheet name
file_path <- "C:/Users/kbediakon/OneDrive - World Health Organization/AFRO HWF UNIT/HWF RETURN ON INVESTMENT/Simple_Model.xlsx"
sheet_name <- "SEM_HWF"

# Read the SEM sheet into a data frame
sem_data <- read_excel(path = file_path, sheet = sheet_name)

# View the first few rows
head(sem_data)

###1. Create a measurement model

phc_rep_mm <- constructs(
  composite("HRH_AVAILABILITY",        single_item("hr")),
  composite("OUTPUT", single_item("output")),
  composite("DALYS_COM", single_item("Outcome_1")),
  composite("DALYS_NCD", single_item("Outcome_2")), 
  composite("DALYS_INJURIES", single_item("Outcome_3")),
  composite("IMPACT", single_item("impact")))

#####2. Create a structural model


phc_rep_sm <- relationships(
  paths(from = c("HRH_AVAILABILITY"), to = c("OUTPUT")),
  paths(from = c("HRH_AVAILABILITY"), to = c("DALYS_COM")),
  paths(from = c("HRH_AVAILABILITY"), to = c("DALYS_NCD")),
  paths(from = c("HRH_AVAILABILITY"), to = c("DALYS_INJURIES")),
  paths(from = c("HRH_AVAILABILITY"), to = c("IMPACT")),
  paths(from = c("OUTPUT"), to = c("DALYS_COM")),
  paths(from = c("OUTPUT"), to = c("DALYS_NCD")),
  paths(from = c("OUTPUT"), to = c("DALYS_INJURIES")),
  paths(from = c("OUTPUT"), to = c("IMPACT")),
  paths(from = c("DALYS_COM"), to = c("IMPACT")),
  paths(from = c("DALYS_NCD"), to = c("IMPACT")),
  paths(from = c("DALYS_INJURIES"), to = c("IMPACT")))




####3. Estimating the model

phc_rep_pls_model <- estimate_pls(data = sem_data,
                                  measurement_model = phc_rep_mm,
                                  structural_model  = phc_rep_sm,
                                  inner_weights = path_weighting,
                                  missing = mean_replacement,
                                  missing_value = "-99")

####4. Summarizing the model

summary_phc_rep <- summary(phc_rep_pls_model)
summary_phc_rep

summary_phc_rep$paths
summary_phc_rep$reliability


plot(summary_phc_rep$reliability)


#### to check if it converged

summary_phc_rep$iterations


###summary statistics

summary_phc_rep$descriptives$statistics$items


summary_phc_rep$descriptives$statistics$constructs



###### 5. Bootstrapping the model

# Step 1: Create a custom temp folder if not exists
dir.create("C:/Temp", showWarnings = FALSE)

# Step 2: Force R to use your custom directory across its session
Sys.setenv(TMP = "C:/Temp", TMPDIR = "C:/Temp")

# Step 3: Redefine R’s internal tempdir function
assignInNamespace("tempdir", function() "C:/Temp", ns = "base")

# Step 4: Try creating the cluster again
library(parallel)
cl <- makeCluster(parallel::detectCores() - 1)

##To obtain the standard errors and confidence intervals

boot_phc_rep <- bootstrap_model(seminr_model = phc_rep_pls_model,
                                nboot = 1000,
                                cores = NULL,
                                seed = 123)


sum_boot_phc_rep <- summary(boot_phc_rep)

sum_boot_phc_rep


##### Evaluation of reflective measurement models



#a. Indicator reliability: 0.708 (retain), 0.4 - 0.7(may retain), below 0.4(remove them)
summary_phc_rep$loadings

### b. Internal consistency reliability  (min 0.6 for exploratory research)

summary_phc_rep$reliability

plot(summary_phc_rep$reliability)

####c. Convergent validity: The average variance extracted (AVE) is the mean of a construct indicator’s squared loadings. The minimum acceptable AVE is 0.50 or higher

summary_phc_rep$validity$fl_criteria

plot(boot_phc_rep, size=3)

#######
####  SEM model 2

###HRH Investment--->Output---->Outcomes---->Impacts

#################################without the levers

###1. Create a measurement model

phc_rep_mm <- constructs(
  composite("HRH_AVAILABILITY",        single_item("hr")),
  composite("OUTPUT", single_item("output")),
  composite("OUTCOME", single_item("DADLYs All")),
  composite("IMPACT", single_item("impact")))

#####2. Create a structural model


phc_rep_sm <- relationships(
  paths(from = c("HRH_AVAILABILITY"), to = c("OUTPUT")),
  paths(from = c("HRH_AVAILABILITY"), to = c("OUTCOME")),
  paths(from = c("HRH_AVAILABILITY"), to = c("IMPACT")),
  paths(from = c("OUTPUT"), to = c("OUTCOME")),
  paths(from = c("OUTPUT"), to = c("IMPACT")),
  paths(from = c("OUTCOME"), to = c("IMPACT")))


####3. Estimating the model

phc_rep_pls_model <- estimate_pls(data = sem_data,
                                  measurement_model = phc_rep_mm,
                                  structural_model  = phc_rep_sm,
                                  inner_weights = path_weighting,
                                  missing = mean_replacement,
                                  missing_value = "-99")

####4. Summarizing the model

summary_phc_rep <- summary(phc_rep_pls_model)
summary_phc_rep

summary_phc_rep$paths
summary_phc_rep$reliability


plot(summary_phc_rep$reliability)


#### to check if it converged

summary_phc_rep$iterations


###summary statistics

summary_phc_rep$descriptives$statistics$items


summary_phc_rep$descriptives$statistics$constructs



###### 5. Bootstrapping the model

# Step 1: Create a custom temp folder if not exists
dir.create("C:/Temp", showWarnings = FALSE)

# Step 2: Force R to use your custom directory across its session
Sys.setenv(TMP = "C:/Temp", TMPDIR = "C:/Temp")

# Step 3: Redefine R’s internal tempdir function
assignInNamespace("tempdir", function() "C:/Temp", ns = "base")

# Step 4: Try creating the cluster again
library(parallel)
cl <- makeCluster(parallel::detectCores() - 1)

##To obtain the standard errors and confidence intervals

boot_phc_rep <- bootstrap_model(seminr_model = phc_rep_pls_model,
                                nboot = 1000,
                                cores = NULL,
                                seed = 123)


sum_boot_phc_rep <- summary(boot_phc_rep)

sum_boot_phc_rep


##### Evaluation of reflective measurement models



#a. Indicator reliability: 0.708 (retain), 0.4 - 0.7(may retain), below 0.4(remove them)
summary_phc_rep$loadings

### b. Internal consistency reliability  (min 0.6 for exploratory research)

summary_phc_rep$reliability

plot(summary_phc_rep$reliability)

####c. Convergent validity: The average variance extracted (AVE) is the mean of a construct indicator’s squared loadings. The minimum acceptable AVE is 0.50 or higher

summary_phc_rep$validity$fl_criteria

plot(boot_phc_rep, size=3)


#plot_ly(plot(boot_phc_rep, size=3))


###########################################################################


#######
