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

  # AUTOMATING
  purrr, # functional programming package
  
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



# 2) Load tidied data 

dta_hmd <- read_csv("tidied_data/tidied_hmd.csv")
dta_hfd <- read_csv("tidied_data/tidied_hfd.csv")


# Example analyses 
# infant mortality by year in different countries 

dta_hmd %>% 
  filter(age == 0) %>% 
  group_by(country_code, year) %>% 
  summarise(deaths = sum(deaths)) %>% 
  ggplot(., aes(x = year, y = deaths, group = country_code)) + 
  geom_line()

# Infant mortality death rate by year in different countries 

dta_hmd %>% 
  filter(age == 0) %>% 
  group_by(country_code, year) %>% 
  summarise(
    deaths = sum(deaths),
    exposure = sum(exposure)
    ) %>% 
  mutate(imr = deaths / exposure ) %>% 
  ggplot(., aes(x = year, y = imr, group = country_code)) + 
  geom_line()

# Infant mortality death rate by year in different countries - log scale

dta_hmd %>% 
  filter(age == 0) %>% 
  group_by(country_code, year) %>% 
  summarise(
    deaths = sum(deaths),
    exposure = sum(exposure)
  ) %>% 
  mutate(imr = deaths / exposure ) %>% 
  mutate(limr = log(imr, 10)) %>% 
  ggplot(., aes(x = year, y = limr, group = country_code)) + 
  geom_line()

dta_hmd %>% 
  filter(age == 0) %>% 
  group_by(country_code, year) %>% 
  summarise(
    deaths = sum(deaths),
    exposure = sum(exposure)
  ) %>% 
  mutate(imr = deaths / exposure ) %>% 
  ggplot(., aes(x = year, y = imr, group = country_code)) + 
  geom_line() + 
  scale_y_log10()

# Faceted display for above 

dta_hmd %>% 
  filter(age == 0) %>% 
  group_by(country_code, year) %>% 
  summarise(
    deaths = sum(deaths),
    exposure = sum(exposure)
  ) %>% 
  mutate(imr = deaths / exposure ) %>% 
  ggplot(., aes(x = year, y = imr)) + 
  geom_line() + 
  scale_y_log10() + 
  facet_wrap(~country_code)


# Infant mortality rate for whole of HMD 
# n.b. filter out some country codes to avoid double counting

dta_hmd %>% 
  filter(age == 0) %>% 
  filter(!(country_code %in% c("DEUTNP", "CARTNP", "GBR_NP", "GBRTENW", "NZL_MA", "NZL_NM"))) %>% 
  group_by(year) %>% 
  summarise(
    deaths = sum(deaths),
    exposure = sum(exposure)
  ) %>% 
  mutate(mr = deaths / exposure ) %>% 
  ggplot(., aes(x = year, y = mr)) + 
  geom_line() + 
  scale_y_log10() 

# male and demale death rates aged 15-30, all HMD, change over time 

dta_hmd %>% 
  filter(age >=15, age <=30) %>% 
  filter(!(country_code %in% c("DEUTNP", "CARTNP", "GBR_NP", "GBRTENW", "NZL_MA", "NZL_NM"))) %>% 
  group_by(year, sex) %>% 
  summarise(
    deaths = sum(deaths),
    exposure = sum(exposure)
  ) %>% 
  mutate(mr = deaths / exposure ) %>% 
  ggplot(., aes(x = year, y = mr, group = sex, linetype = sex)) + 
  geom_line() + 
  scale_y_log10() 

#ratio of male to female deaths in this age group 

dta_hmd %>% 
  filter(age >=15, age <=30) %>% 
  filter(!(country_code %in% c("DEUTNP", "CARTNP", "GBR_NP", "GBRTENW", "NZL_MA", "NZL_NM"))) %>% 
  group_by(year, sex) %>% 
  summarise(
    deaths = sum(deaths),
    exposure = sum(exposure)
  ) %>% 
  mutate(mr = deaths / exposure ) %>%
  select(-deaths, -exposure) %>% 
  spread(sex, mr) %>% 
  mutate(ratio = male / female) %>% 
  ggplot(., aes(x = year, y = ratio)) + 
  geom_line() 
  

# less crude approach to doing the above 

dta_hmd %>% 
  filter(age >=15, age <=30) %>% 
  filter(!(country_code %in% c("DEUTNP", "CARTNP", "GBR_NP", "GBRTENW", "NZL_MA", "NZL_NM"))) %>% 
  mutate(mr = deaths / exposure ) %>%
  filter(!is.na(mr)) %>% 
  select(-deaths,-population, -exposure) %>%
  group_by(country_code, year, age) %>% 
  spread(sex, mr) %>% 
  mutate(ratio = male / female) %>% 
  ungroup() %>% 
  select(country_code, year, age, ratio) %>% 
  group_by(country_code, year) %>% 
  summarise(
    mean_ratio = mean(ratio), 
    median_ratio = median(ratio)
            ) %>% 
  ggplot(., aes(x = year)) + 
  geom_line(aes(y = mean_ratio), colour = "red") + 
  geom_line(aes(y = median_ratio), colour = "blue", linetype = "dashed") + 
  facet_wrap(~country_code)


# Bathtub curves by 25 year intervals 

dta_hmd %>% 
  filter(year %in% seq(1850, 2000, by = 25)) %>% 
  filter(age <= 90) %>% 
  filter(!(country_code %in% c("DEUTNP", "CARTNP", "GBR_NP", "GBRTENW", "NZL_MA", "NZL_NM"))) %>% 
  mutate(mr = deaths / exposure) %>% 
  filter(!is.na(mr)) %>% 
  ggplot(., aes(x = age, y = mr, group = country_code)) + 
  geom_line(alpha = 0.2) + 
  facet_grid(sex ~ year) + 
  scale_y_log10()


# Exploration by cohort 

dta_hmd %>% 
  mutate(birth_year = year - age) %>% 
  filter(age <= 90) %>% 
  filter(birth_year %in% c(1875, 1900, 1925, 1950, 1975)) %>% 
  mutate(birth_year = factor(birth_year)) %>% 
  filter(country_code == "FRACNP") %>% 
  mutate(mr = deaths / exposure) %>% 
  ggplot(., aes(x = age, y = mr, group = birth_year, colour = birth_year)) + 
  geom_line() +
  facet_wrap(~sex) + scale_y_log10()

# Comparison between France, Italy, England & Wales
dta_hmd %>% 
  mutate(birth_year = year - age) %>% 
  filter(age <= 90) %>% 
  filter(birth_year %in% c(1875, 1900, 1925, 1950, 1975)) %>% 
  mutate(birth_year = factor(birth_year)) %>% 
  filter(country_code %in% c("FRACNP", "GBRCENW", "ITA")) %>% 
  mutate(mr = deaths / exposure) %>% 
  ggplot(., aes(x = age, y = mr, group = country_code, colour = country_code )) + 
  geom_line() +
  facet_grid(birth_year~sex) + scale_y_log10()


# WW1 cohort effect in France

dta_hmd %>% 
  mutate(birth_year = year - age) %>% 
  filter(age <= 90) %>% 
  filter(birth_year %in% 1916:1922) %>% 
  mutate(birth_year = factor(birth_year)) %>% 
  filter(country_code == "FRACNP") %>% 
  mutate(mr = deaths / exposure) %>% 
  ggplot(., aes(x = age, y = mr, group = birth_year, colour = birth_year)) + 
  geom_line() +
  facet_wrap(~sex) + 
  scale_y_log10()


# Examples using HFD 

# Total births by year 
dta_hfd %>% 
  group_by(year) %>% 
  summarise(total_births = sum(total)) %>% 
  ggplot(., aes(x = year, y = total_births)) + 
  geom_line()


# Unique countries in each year 
dta_hfd %>% 
  group_by(year) %>% 
  summarise(num_countries = length(unique(code))) %>% 
  ggplot(. , aes(x = year, y = num_countries)) + 
  geom_step()


# Completed fertility by period in select countries %>% 
dta_hfd %>%

