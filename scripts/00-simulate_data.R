#### Preamble ####
# Purpose: We simulated the dataset using the tipple function and determined the
# feasibility of the plan.
# Author: Yunzhao Li, Yang Cheng, Wentao Sun
# Date: 14 February 2024
# Contact: yunzhao.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: None


#### Workspace setup ####
library(tidyverse)
library(janitor)
library(knitr)
# [...UPDATE THIS...]

#### Simulate data ####

#### Simulate data for Exp1####
set.seed(23)

simulated_exp1 <-
  tibble(
    id = c(1:342),
    regret = sample(c("J", "S"), 342, replace = TRUE),
    social_norm_injunctive = sample(c("J", "S"), 342, replace = TRUE),
    social_norm_descriptive = sample(c("J", "S"), 342, replace = TRUE),
    negative_effect = sample(c("J", "S"), 342, replace = TRUE)
  )

head(simulated_exp1) |>
  kable(
    col.names = c("id", "regret", "social_norm_injunctive", "social_norm_descriptive", "negative_effect"),
    booktabs = TRUE
  )

#### Simulate data for Exp2####
set.seed(23)
simulated_exp2 <-
  tibble(
    id = c(1:342),
    regret = sample(c("A", "W"), 342, replace = TRUE),
    luck = sample(c("A", "W"), 342, replace = TRUE)
  )

head(simulated_exp2) |>
  kable(
    col.names = c("id", "regret", "luck"),
    booktabs = TRUE
  )


