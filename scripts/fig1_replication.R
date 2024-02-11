#### Preamble ####
# Purpose: Figure 1 replication
# Author: Kimlin Chin
# Date: 22 February 2022
# Contact: kimlin.chin@mail.utoronto.ca
# License: MIT

## Data Sources:

# Martin JA, Hamilton BE, Osterman MJK, et al. Births: Final data for 2015. 
# National vital statistics report; vol 66, no 1. Hyattsville, MD: National Center for Health Statistics. 
# 2017. https://www.cdc.gov/nchs/data/nvsr/nvsr66/nvsr66_01.pdf.
# Data can be found on page 35,
# Table 9. Birth rates, by live-birth order and race and Hispanic origin of mother: United States, 1980–2015
# Columns: Fertility Rate, All races[years 1980-2015]

# Martin JA, Hamilton BE, Osterman MJK, Driscoll AK. Births: Final Data for 2019. National
# Vital Statistics Reports; vol 70 no 2. Hyattsville, MD: National Center for Health
# Statistics. 2021. DOI: https://dx.doi.org/10.15620/cdc:100472.
# Data can be found on page 13,
# Table 1. Births and birth rates: United States, 2010–2019, and by race and Hispanic origin, United States, 2016–2019
# Columns: Fertility Rate, All races and origins[years 2016-2018]

# Hamilton BE, Martin JA, Osterman MJK. Births: Provisional data for 2020. Vital Statistics
# Rapid Release; no 12. Hyattsville, MD: National Center for Health Statistics. May
# 2021. DOI: https://doi.org/10.15620/cdc:104993.
# Data can be found on page 6,
# Table 1. Births and birth rates, by age of mother: United States, final 2019 and provisional 2020
# Columns: 2020 Rate, 2019 Rate, Age of mother[All ages]

# I crosschecked the data from these 3 reports with the data in fig_1.csv provided by 
# the authors of the paper 
# Kearney, Melissa S., Phillip B. Levine, and Luke Pardue. 2022. 
# "The Puzzle of Falling US Birth Rates since the Great Recession." 
# Journal of Economic Perspectives, 36 (1): 151-76. DOI: 10.1257/jep.36.1.151 
# and it is the same.


#### Workspace setup ####
# Load libraries
library(tidyverse)
library(ggprism)

# Read in the raw data.
fig1_data <- read_csv("inputs/data/fig_1.csv")

# Make plot
fig1_data %>%
  ggplot(aes(x = year,
             y = brate_all)) +
  geom_line(color = "#08519C") +
  geom_vline(xintercept = 2007,
             linetype = "dashed",
             color = "darkgrey") +
  scale_x_continuous(
    guide = "prism_minor",
    limits = c(1980, 2020),
    breaks = seq(1980, 2020, by = 5),
    minor_breaks = seq(1980, 2020, by = 1)
  ) +
  scale_y_continuous(limits = c(50, 80), breaks = seq(50, 80, by = 5)) +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(),
    axis.ticks.length.x = unit(7, "pt"),
    prism.ticks.length.x = unit(4, "pt")
  ) +
  geom_text(
    aes(x = 2007, y = 70),
    nudge_x = 2,
    nudge_y = 1,
    label = "2007"
  ) +
  labs(
    title = "Trend in US Birth Rates",
    x = "",
    y = "Births per 1,000 women age 15-44",
    caption = "Source: Birth Rates collected from CDC Vital Statistics Births Reports for 2015, 2019 and 2020."
  )

# Save plot
ggsave(
  "replication/plots/figure1.png",
  height = 100,
  width = 133.33,
  units = "mm",
  dpi = 900
)
