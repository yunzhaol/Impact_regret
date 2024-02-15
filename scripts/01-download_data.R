#### Preamble ####
# Purpose:We acquired a CSV file containing raw data relevant to 'Routines and 
# Regret: An Examination of Behavioral Norms and Emotional Responses.' 
# Subsequently, we inspected the first six rows and assessed the demographic details.
# Author: Yunzhao Li, Yang Cheng, Wentao Sun
# Date: 14 February 2024
# Contact: yunzhao.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: None


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

#### Download data ####
options(scipen=999.99, digits =7)

# load our dataset 
data <- read.csv(here::here("data/raw_data/osf-past-normality-regret-replication-exp1-data.csv"), header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")

#Check the top six rows of data of interest
dataset_selected <- data %>% select(Sc1_regret, sc1_socnorms1, sc1_socnorms2, sc1_combinednorms, Sc2_regret, Sc2_lucky) 
kable(head(dataset_selected))

#Show demographics
#Age
data$gender
table(data$gender)
#Gender
data$age
data$age[data$age==99] <- NA
mean(data$age, na.rm = TRUE)
sd(data$age, na.rm = TRUE)




         
