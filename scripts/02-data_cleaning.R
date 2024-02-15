#### Preamble ####
# Purpose: We label the data in the questionnaire with values of 1 and 2 as 
# "Routine Smith", "Exception Jones" or "Routine Adams", "Exception White". 
# The top 6 rows of interest are shown
# Author: Yunzhao Li, Yang Cheng, Wentao Sun
# Date: 14 February 2024
# Contact: yunzhao.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: download_data.R


#### Workspace setup ####
library(ggplot2)
library(scales)
library(knitr)
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

#### Clean data ####
###################
# EXPERIMENT 1 (Hitchhiker-Scenario)

# JAMOVI requires factors, while R imports as numeric. So, need to convert from numeric to factor.
data$Sc1_regret<- factor(data$Sc1_regret)
data$sc1_socnorms1<- factor(data$sc1_socnorms1)
data$sc1_socnorms2<- factor(data$sc1_socnorms2)
data$sc1_combinednorms<- factor(data$sc1_combinednorms)

# Let's label the values better, so it's easier to understand the output.
data$Sc1_regret <- ordered(data$Sc1_regret, levels = c(1,2), labels = c("Exception Jones", "Routine Smith"))
data$sc1_socnorms1 <- ordered(data$sc1_socnorms1, levels = c(1,2), labels = c("Exception Jones", "Routine Smith"))
data$sc1_socnorms2 <- ordered(data$sc1_socnorms2, levels = c(1,2), labels = c("Exception Jones", "Routine Smith"))
data$sc1_combinednorms <- ordered(data$sc1_combinednorms, levels = c(1,2), labels = c("Exception Jones", "Routine Smith"))

# Let's label the variables better, so we'll remember what those mean and it's easier to understand the output when those are reported.
label(data$Sc1_regret) <- "Who experiences higher regret (direct replication)" 
label(data$sc1_socnorms1) <- "Descriptive norms - which is more common?" 
label(data$sc1_socnorms2) <- "Injunctive norms - who is more criticized by society?" 
label(data$sc1_combinednorms) <- "Who experiences higher regret, when asking participants to consider the norm" 

dataset_selected_exp1 <- data %>% select(Sc1_regret, sc1_socnorms1, sc1_socnorms2, sc1_combinednorms) 
kable(head(dataset_selected_exp1))

write_csv(
  x = data,
  file = here::here("data/analysis_data/exp1-labeled-data.csv")
)

# Let's run the JAMOVI imported syntax 
# Descriptives for the main variables.
# Plots appear in the R Studio Plots section
jmv::descriptives(
  data=data,
  vars=c(
    "Sc1_regret",
    "sc1_socnorms1",
    "sc1_socnorms2",
    "sc1_combinednorms"),
  freq=TRUE)

# binomial Z
jmv::propTest2(
  data=data,
  vars=c(
    "Sc1_regret",
    "sc1_socnorms1",
    "sc1_socnorms2",
    "sc1_combinednorms"),
  ci=TRUE)

###################
# EXPERIMENT 2 (Car Accident-Scenario)

#Measure correction: All particpants who indicated (5- somewhat agree) on the question regarding random chance (data$Sc2_random_1 or data$Sc2_random_2) had accidently assigned the value '56' instead of '5' in Qualtrics
data$Sc2_random_1[data$Sc2_random_1==56] <- 5
data$Sc2_random_2[data$Sc2_random_2==56] <- 5

# Conversion from numeric to factors 
data$Sc2_regret<- factor(data$Sc2_regret)
data$Sc2_lucky<- factor(data$Sc2_lucky)


# Label values
data$Sc2_regret <- ordered(data$Sc2_regret, levels = c(1,2), labels = c("Routine Adams", "Exception White"))
data$Sc2_lucky <- ordered(data$Sc2_lucky, levels = c(1,2), labels = c("Adams less lucky", "White less lucky"))
names (data$Sc2_random_1) <- c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")
names (data$Sc2_random_2) <- c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")

# Label Variables
label(data$Sc2_regret) <- "Who feels more upset (direct replication)"
label(data$Sc2_random_1) <- "Adam's (Routine) accident is a random coincidence"
label(data$Sc2_random_2) <- "White's' (Exception) accident is a random coincidence"
label(data$Sc2_lucky) <- "Who is less lucky"

dataset_selected_exp2 <- data %>% select(Sc2_regret, Sc2_lucky) 
kable(head(dataset_selected_exp2))


# Save the cleaned data
write_csv(
  x = data,
  file = here::here("data/analysis_data/exp2-labeled-data.csv")
)

# Descriptives for main variables
jmv::descriptives(
  data=data,
  vars=c(
    "Sc2_regret",
    "Sc2_random_1",
    "Sc2_random_2",
    "Sc2_lucky"),
  freq=TRUE)

# binomial Z
jmv::propTest2(
  data=data,
  vars=c(
    "Sc2_regret",
    "Sc2_lucky"),
  ci=TRUE)


