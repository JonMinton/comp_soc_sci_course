# Script 0_0 : Master script 
# 6 / 10 / 2016

# Description


# Standard sequence:

# 0) Clear workspace and load prereq packages 
# 1) Load and tidy raw data (once) 
# 2) Load tidied data 
# 3) Ad hoc analyses 
# 4) Automated analyses 



# 1) Clear workspace
rm(list = ls())

# 2) load pre-requisites 

# Using pacman : a package for managing packages 

#install.packages("pacman") # Do this first time only

pacman::p_load(
  
  # LOADING AND TIDYING
  readr, # reads simple text files
  tidyr, dplyr, # piping and data manipulation packages 
  stringr, # work with character strings 
  forcats, # working with categorical variables 
  
  # AUTOMATING
  purrr, # functional programming package
  broom, # tidying model outputs 
  
  # GRAPHING
  RColorBrewer, # Better options for colour palettes
  ggplot2, # Grammar of Graphics packages - simple exploratory graphs
  lattice, latticeExtra # Lattice graphics package - for contour maps
)


# 1) Load and tidy raw data  ----------------------------------------------

# # 1.1 HFD harvesting (Once)
# source("scripts/1_1_harvest_hfd.R")
# # 1.2 HMD harvesting (Once)
# source("scripts/1_2_harvest_hmd.R")



# 2) Exploratory data analysis scripts ------------------------------------

# # 2.1  Exploratory data analysis for HMD
# source("scripts/2_1_hmd_exploratory_data_analysis.R")
# # 2.2 Exploratory data analysis for HFD 
# source("scripts/2_2_hfd_exploratory_data_analysis.R")


# 3) Automating analyses 

# HMD 

# Fall in infant mortality over time 

dta_hmd <- read_csv("tidied_data/tidied_hmd.csv")


# Fit against time/sex model without interaction
dta_hmd %>% 
  filter(age == 0) %>% 
  mutate(imr = deaths / exposure, limr = log(imr, 10)) %>% 
  group_by(country_code) %>% 
  mutate(years_since_1900 = year - 1900) %>% 
  nest() %>% 
  mutate(mdl = map(data, function(x) {lm(limr ~ years_since_1900 + sex, data = x)})) %>%
  mutate(fit = map_dbl(mdl, AIC)) %>% 
  mutate(country_code = fct_reorder(country_code, fit)) %>% 
  ggplot(., aes(x = country_code, y = fit)) + geom_bar(stat = "identity") + coord_flip()


dta_hmd %>% 
  filter(age == 0) %>% 
  mutate(imr = deaths / exposure, limr = log(imr, 10)) %>% 
  group_by(country_code) %>% 
  mutate(years_since_1900 = year - 1900) %>% 
  nest() %>% 
  mutate(mdl = map(data, function(x) {lm(limr ~ years_since_1900 + sex, data = x)})) %>%
  mutate(mdl2 = map(data, function(x) {lm(limr ~ years_since_1900 * sex, data = x)})) %>%
  mutate(fit = map_dbl(mdl, AIC)) %>% 
  mutate(fit2 = map_dbl(mdl2, AIC)) %>% 
  arrange(fit)


dta_hmd %>% 
  filter(age == 0) %>% 
  mutate(imr = deaths / exposure, limr = log(imr, 10)) %>% 
  group_by(country_code) %>% 
  mutate(years_since_1900 = year - 1900) %>% 
  nest() %>% 
  mutate(mdl = map(data, function(x) {lm(limr ~ years_since_1900 + sex, data = x)})) %>% 
  mutate(intercept = map_dbl(mdl, function(x) {x %>% coefficients %>% .[["(Intercept)"]] })) %>% 
  mutate(trend = map_dbl(mdl, function(x) {x %>% coefficients %>% .[["years_since_1900"]]})) %>% 
  ggplot(., aes(x = intercept, y = trend)) + 
  geom_text(aes(label = country_code)) +
  stat_smooth( method = "lm", se = F)





  

         

