
# 2) Load tidied data 

dta_hfd <- read_csv("tidied_data/tidied_hfd.csv")


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


# Completed fertility by period in select countries 
dta_hfd %>%
  group_by(code, year) %>% 
  arrange(age) %>% 
  mutate(asfr = total / exposure) %>% 
  mutate(cumulative_fert = cumsum(asfr)) %>%
  filter(age %in% seq(20, 45, by = 5)) %>% 
  mutate(age = factor(age)) %>% 
  ggplot(. , aes(x = year, y = cumulative_fert, group = code, colour = code)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~age)

# Age at which different (synthetic, period) cohorts reach replacement fertility levels 

dta_hfd %>%
  group_by(code, year) %>% 
  arrange(age) %>% 
  mutate(asfr = total / exposure) %>% 
  mutate(cumulative_fert = cumsum(asfr)) %>% 
  mutate(replacement = cumulative_fert > 2.05) %>% 
  filter(replacement) %>% 
  summarise(age_of_replacement = min(age)) %>% 
  ggplot(., aes(x = year, y = age_of_replacement)) + 
  geom_line() + 
  facet_wrap(~ code)

#teenage pregnancy rates over time 

dta_hfd %>% 
  filter(age < 20) %>% 
  group_by(code, year) %>% 
  summarise(total = sum(total), exposure = sum(exposure)) %>% 
  mutate(teenage_fert = total / exposure) %>% 
  ggplot(., aes(x = year, y = teenage_fert)) + 
  geom_line() + 
  facet_wrap(~code)


# Older age pregnancy rates over time 

dta_hfd %>% 
  filter(age >= 40) %>% 
  group_by(code, year) %>% 
  summarise(total = sum(total), exposure = sum(exposure)) %>% 
  mutate(teenage_fert = total / exposure) %>% 
  ggplot(., aes(x = year, y = teenage_fert)) + 
  geom_line() + 
  facet_wrap(~code)

# Looking at post 1990

dta_hfd %>% 
  filter(year >= 1990) %>% 
  filter(age >= 40) %>% 
  group_by(code, year) %>% 
  summarise(total = sum(total), exposure = sum(exposure)) %>% 
  mutate(teenage_fert = total / exposure) %>% 
  ggplot(., aes(x = year, y = teenage_fert)) + 
  geom_line() + 
  facet_wrap(~code)



# Examples of tables 
# Ranking of teenage pregnancy rates in 1980 and 2010 


dta_hfd %>% 
  filter(age < 20) %>% 
  group_by(code, year) %>% 
  summarise(total = sum(total), exposure = sum(exposure)) %>% 
  mutate(teenage_fert = total / exposure) %>% 
  ungroup() %>% 
  filter(year %in% c(1980, 2000)) %>% 
  select(code, year, teenage_fert) %>% 
  spread(year, teenage_fert) %>% 
  filter(!is.na(`1980`) & !is.na(`2000`)) %>% 
  arrange(`1980`) %>% 
  mutate(rank_1980 = rank(`1980`), rank_2000 = rank(`2000`)) %>% 
  mutate(change_rank = rank_2000 - rank_1980) %>% 
  View()


# Another exploration: How has the median age increased in different countries?


