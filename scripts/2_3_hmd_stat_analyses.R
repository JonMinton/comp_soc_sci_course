
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


# What about trends in life expectancy? 

dta_hmd %>% 
  group_by(country_code, year, sex) %>% 
  mutate(
    age_death = age * deaths 
  ) %>% 
  summarise(mean_death = sum(age_death) / sum(deaths)) %>% 
  ungroup() %>% 
  mutate(years_since_1900 = year - 1900) -> e0_dta

lm(mean_death ~ years_since_1900, data = e0_dta) -> mdl_01
lm(mean_death ~ years_since_1900 + sex, data = e0_dta) -> mdl_02
lm(mean_death ~ years_since_1900 * sex, data = e0_dta) -> mdl_03
lm(mean_death ~ years_since_1900 * sex + country_code, data = e0_dta) -> mdl_04

anova(mdl_01, mdl_02)
anova(mdl_02, mdl_03)
anova(mdl_03, mdl_04)

AIC(mdl_01, mdl_02, mdl_03, mdl_04)

mdl_04 %>% 
  tidy %>% 
  filter(str_detect(term, "country_code")) %>% 
  mutate(term = str_replace(term, "country_code", "")) %>% 
  mutate(term = reorder(term, estimate))  %>% 
  ggplot(., aes(x = estimate, y = term)) + geom_point() + 
  geom_segment(aes(x = estimate - 2 * std.error, xend = estimate + 2 * std.error, y = term, yend = term, group = term)) + 
  geom_vline(xintercept = 0)


