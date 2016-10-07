# Exploratory data analysis, HMD



# 2) Load tidied data 

dta_hmd <- read_csv("tidied_data/tidied_hmd.csv")


# Example analyses 
# infant mortality by year in different countries 

dta_hmd %>% 
  filter(age == 0) %>% 
  group_by(country_code, year) %>% 
  summarise(deaths = sum(deaths)) %>% 
  ggplot(., aes(x = year, y = deaths, group = country_code)) + 
  geom_line(alpha = 0.2)

dta_hmd %>% 
  filter(age == 0) %>% 
  group_by(country_code, year) %>% 
  summarise(deaths = sum(deaths)) %>% 
  ggplot(., aes(x = year, y = deaths, group = country_code)) + 
  geom_line(alpha = 0.2) + scale_y_log10()



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
  geom_line(alpha = 0.2)

dta_hmd %>% 
  filter(age == 0) %>% 
  group_by(country_code, year) %>% 
  summarise(
    deaths = sum(deaths),
    exposure = sum(exposure)
  ) %>% 
  mutate(imr = deaths / exposure ) %>% 
  ggplot(., aes(x = year, y = imr, group = country_code)) + 
  geom_line(alpha = 0.2) + scale_y_log10()


# Change in life expectancy/mean age of death
dta_hmd %>% 
  group_by(country_code, year, sex) %>% 
  mutate(
    age_death = age * deaths 
  ) %>% 
  summarise(mean_death = sum(age_death) / sum(deaths)) %>% 
  ggplot(., aes(x = year, y = mean_death, group = country_code)) + 
  geom_line(alpha = 0.2) + facet_wrap(~sex)


# Change in conditional mean age of death, age 5 or over 

dta_hmd %>% 
  group_by(country_code, year, sex) %>% 
  mutate(
    age_death = age * deaths 
  ) %>% 
  summarise(mean_death = sum(age_death[age >=5]) / sum(deaths[age >= 5])) %>% 
  ggplot(., aes(x = year, y = mean_death, group = country_code)) + 
  geom_line(alpha = 0.2) + facet_wrap(~sex)

# Change in conditional mean age of death, age 35 or over 

dta_hmd %>% 
  group_by(country_code, year, sex) %>% 
  mutate(
    age_death = age * deaths 
  ) %>% 
  summarise(mean_death = sum(age_death[age >=35]) / sum(deaths[age >= 35])) %>% 
  ggplot(., aes(x = year, y = mean_death, group = country_code)) + 
  geom_line(alpha = 0.2) + facet_wrap(~sex)




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



