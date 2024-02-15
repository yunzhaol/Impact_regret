#### Preamble ####
# Purpose: We conducted various tests to verify if the imported data aligns 
# with our expectations for Experiment 1 and Experiment 2.
# Author: Yunzhao Li, Yang Cheng, Wentao Sun
# Date: 14 February 2024
# Contact: yunzhao.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: download_data.R


#### Workspace setup ####
library(MBESS)
library(psych)
library(dplyr) 
library(Hmisc)
library(effsize)
library(jmv)
library(Rcpp)
library(reshape2)
library(readr)
library(janitor)
library(tidyverse)
library(kableExtra)

#### Test data ####
# EXPERIMENT 1 (Hitchhiker-Scenario)
# Adjust the allowed_values to not include "NA" as a string
allowed_values_exp1 <- c("Routine Smith", "Exception Jones")

# Modify the check to correctly account for NA values and check whether the 
# values are one of the "Routine Smith", "Exception Jones".
all(data$Sc1_regret %in% allowed_values_exp1 | is.na(data$Sc1_regret))== TRUE
all(data$sc1_socnorms1 %in% allowed_values_exp1 | is.na(data$sc1_socnorms1))== TRUE
all(data$sc1_socnorms2 %in% allowed_values_exp1 | is.na(data$sc1_socnorms2))== TRUE
all(data$sc1_combinednorms %in% allowed_values_exp1 | is.na(data$sc1_combinednorms))== TRUE


# EXPERIMENT 2 (Car Accident-Scenario)
# Adjust the allowed_values to not include "NA" as a string
allowed_values_exp2_regret <- c("Routine Adams", "Exception White")
allowed_values_exp2_lucky <- c("Adams less lucky", "White less lucky")
# Modify the check to correctly account for NA values and check whether the 
# values are one of the "Routine Adams", "Exception White".
all(data$Sc2_regret %in% allowed_values_exp2_regret | is.na(data$Sc2_regret))== TRUE
all(data$Sc2_lucky %in% allowed_values_exp2_lucky | is.na(data$Sc2_lucky))== TRUE