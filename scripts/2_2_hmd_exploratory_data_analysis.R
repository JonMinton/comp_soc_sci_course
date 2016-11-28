# Exploratory data analysis, HMD



# 2.2.1) Load tidied data 

dta_hmd <- read_csv("tidied_data/tidied_hmd.csv")


# 2.2.2 - Infant mortality trends

dta_hmd %>% 
  filter(age == 0) %>% 
  group_by(country_code, year) %>% 
  summarise(deaths = sum(deaths), exposure = sum(exposure)) %>%
  mutate(imr = deaths / exposure) %>% 
  ggplot(., aes(x = year, y = imr, group = country_code)) + 
  geom_line(alpha = 0.2)

# Many mortality trends and relationships become clearer when looking at the log scale rather than 
# on an identity scale

dta_hmd %>% 
  filter(age == 0) %>% 
  group_by(country_code, year) %>% 
  summarise(deaths = sum(deaths), exposure = sum(exposure)) %>%
  mutate(imr = deaths / exposure) %>% 
  ggplot(., aes(x = year, y = imr, group = country_code)) + 
  geom_line(alpha = 0.2) + scale_y_log10()

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



# 2.2.3 - change in mean age of death (period life expectancy)
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


# 2.2.4 - Death rate between ages 15 and 30


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

# Sex specific death rates, by country 
dta_hmd %>% 
  filter(age >=15, age <=30) %>% 
  filter(!(country_code %in% c("DEUTNP", "CARTNP", "GBR_NP", "GBRTENW", "NZL_MA", "NZL_NM"))) %>% 
  group_by(country_code, year, sex) %>% 
  summarise(
    deaths = sum(deaths),
    exposure = sum(exposure)
  ) %>% 
  mutate(mr = deaths / exposure ) %>% 
  arrange(country_code, sex, year) %>% 
  ggplot(., aes(x = year, y = mr, group = sex, linetype = sex)) + 
  geom_point(alpha = 0.05) + 
  scale_y_log10() + 
  facet_wrap(~country_code) +
  stat_smooth(method = "lm", se = F)

# We might also want to look at a more flexible curve fitting method:

dta_hmd %>% 
  filter(age >=15, age <=30) %>% 
  filter(!(country_code %in% c("DEUTNP", "CARTNP", "GBR_NP", "GBRTENW", "NZL_MA", "NZL_NM"))) %>% 
  group_by(country_code, year, sex) %>% 
  summarise(
    deaths = sum(deaths),
    exposure = sum(exposure)
  ) %>% 
  mutate(mr = deaths / exposure ) %>% 
  arrange(country_code, sex, year) %>% 
  ggplot(., aes(x = year, y = mr, group = sex, linetype = sex)) + 
  geom_point(alpha = 0.05) + 
  scale_y_log10() + 
  facet_wrap(~country_code) +
  stat_smooth(se = F)

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


# Ratio for individual countries 

dta_hmd %>% 
  filter(age >=15, age <=30) %>% 
  filter(!(country_code %in% c("DEUTNP", "CARTNP", "GBR_NP", "GBRTENW", "NZL_MA", "NZL_NM"))) %>% 
  group_by(country_code, year, sex) %>% 
  summarise(
    deaths = sum(deaths),
    exposure = sum(exposure)
  ) %>% 
  mutate(mr = deaths / exposure ) %>% 
  select(country_code, year, sex, mr) %>% 
  spread(sex, mr) %>% 
  mutate(sex_ratio = male / female) %>% 
  ggplot(., aes(x = year, y = sex_ratio)) +
  geom_line() + 
  facet_wrap(~country_code) 

# Since 1950 
dta_hmd %>% 
  filter(age >=15, age <=30) %>%
  filter(year >= 1950) %>% 
  filter(!(country_code %in% c("DEUTNP", "CARTNP", "GBR_NP", "GBRTENW", "NZL_MA", "NZL_NM"))) %>% 
  group_by(country_code, year, sex) %>% 
  summarise(
    deaths = sum(deaths),
    exposure = sum(exposure)
  ) %>% 
  mutate(mr = deaths / exposure ) %>% 
  select(country_code, year, sex, mr) %>% 
  spread(sex, mr) %>% 
  mutate(sex_ratio = male / female) %>% 
  ggplot(., aes(x = year, y = sex_ratio)) +
  geom_line() + 
  facet_wrap(~country_code) 

# 2.2.5 - Bathtub curves by 25 year intervals 

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


# Exploration by cohort , for France

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

# Comparison between France, Italy, England & Wales for select cohorts
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

# Synthetic cohort comparison for the same countries 

dta_hmd %>% 
  filter(age <= 90) %>% 
  filter(year %in% c(1900, 1950, 1975, 2000)) %>% 
  mutate(year = factor(year)) %>% 
  filter(country_code %in% c("FRACNP", "GBRCENW", "ITA")) %>% 
  mutate(mr = deaths / exposure) %>% 
  ggplot(., aes(x = age, y = mr, group = country_code, colour = country_code )) + 
  geom_line() +
  facet_grid(year~sex) + scale_y_log10()

# Re-arranging the above so that faceting is by country and sex, and grouping's by year

dta_hmd %>% 
  filter(age <= 90) %>% 
  filter(year %in% c(1900, 1950, 1975, 2000)) %>% 
  mutate(year = factor(year)) %>% 
  filter(country_code %in% c("FRACNP", "GBRCENW", "ITA")) %>% 
  mutate(mr = deaths / exposure) %>% 
  ggplot(., aes(x = age, y = mr, group = year, colour = year )) + 
  geom_line() +
  facet_grid(country_code~sex) + scale_y_log10()


# 2.2.6. Population structure 

# What proportion of the HMD world's population are aged 80 or over? 

dta_hmd %>% 
  group_by(year) %>% 
  filter(age <= 109) %>% 
  filter(!(country_code %in% c("DEUTNP", "CARTNP", "GBR_NP", "GBRTENW", "NZL_MA", "NZL_NM"))) %>% 
  summarise(
    pop = sum(population, na.rm = T),
    pop_80_plus = sum(population[age >= 80], na.rm = T)
  ) %>% 
  mutate(prop_over_80 = pop_80_plus / pop) %>% 
  ggplot(., aes(x = year, y = prop_over_80)) + 
  geom_line()

# What proportion of the HMD world's population are aged 20 or younger? 

dta_hmd %>% 
  group_by(year) %>% 
  filter(!(country_code %in% c("DEUTNP", "CARTNP", "GBR_NP", "GBRTENW", "NZL_MA", "NZL_NM"))) %>% 
  summarise(
    pop = sum(population, na.rm = T),
    pop_20_under = sum(population[age <= 20], na.rm = T)
  ) %>% 
  mutate(prop_20_under = pop_20_under / pop) %>% 
  ggplot(., aes(x = year, y = prop_20_under)) + 
  geom_line()

# Male dependency ratio - all countries

dta_hmd %>% 
  group_by(year) %>% 
  filter(!(country_code %in% c("DEUTNP", "CARTNP", "GBR_NP", "GBRTENW", "NZL_MA", "NZL_NM"))) %>% 
  summarise(
    pop = sum(population, na.rm = T),
    pop_16_under = sum(population[age <= 16], na.rm = T),
    pop_65_older = sum(population[age >= 65], na.rm = T)
  ) %>% 
  mutate(dep_ratio = (pop - pop_16_under - pop_65_older) / (pop_16_under + pop_65_older)) %>% 
  ggplot(., aes(x = year, y = dep_ratio)) + 
  geom_line()

# Male dependency ratio - by country
dta_hmd %>% 
  group_by(country_code, year) %>% 
  filter(!(country_code %in% c("DEUTNP", "CARTNP", "GBR_NP", "GBRTENW", "NZL_MA", "NZL_NM"))) %>% 
  summarise(
    pop = sum(population, na.rm = T),
    pop_16_under = sum(population[age <= 16], na.rm = T),
    pop_65_older = sum(population[age >= 65], na.rm = T)
  ) %>% 
  mutate(dep_ratio = (pop - pop_16_under - pop_65_older) / (pop_16_under + pop_65_older)) %>% 
  ggplot(., aes(x = year, y = dep_ratio)) + 
  geom_line() + 
  facet_wrap(~country_code)



