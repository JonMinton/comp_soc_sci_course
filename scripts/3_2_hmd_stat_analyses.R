
# 3) Automating analyses 

# In this series of exercises we are interested in running many regression models in an automated way 
# and summarising the results of these many models. The data used are from the HMD

#First we load the data 

dta_hmd <- read_csv("tidied_data/tidied_hmd.csv")


# 3_2_1 Trends in infant mortality over time 

# We will now fit a separate regression model,
# regressing log infant mortality against year since 1900, and sex
# for each country in the HMD

# For each of  these models we will extract the AIC, a measure of penalised model fit 
# and rank countries by the fit.
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


# In the previous model we assumed the trend over time was identical for both sexes. 
# In order to test the assumption we can compare the model fit from this regression model 
# against the model fit of models which also incorporate an interaction term between 
# time and sex. This includes an additional term which allows for different gradients over time 
# for each sex
dta_hmd %>% 
  filter(age == 0) %>% 
  mutate(imr = deaths / exposure, limr = log(imr, 10)) %>% 
  group_by(country_code) %>% 
  mutate(years_since_1900 = year - 1900) %>% 
  nest() %>% 
  mutate(mdl = map(data, function(x) {lm(limr ~ years_since_1900 + sex, data = x)})) %>%
  mutate(mdl2 = map(data, function(x) {lm(limr ~ years_since_1900 * sex, data = x)})) -> limr_twomodels

# We can access a model object (lm) for a single country as follows

# First row (AUS)
limr_twomodels %>% 
  .[["mdl"]] %>% 
  .[[1]]

# Second row (AUT)
limr_twomodels %>% 
  .[["mdl"]] %>% # The column
  .[[2]] # the element in the vector specified by the column

# Example of extracting a model for a country by name not position

limr_twomodels %>% 
  filter(country_code == "DEUTE") %>% # Filters on a row
  .[["mdl"]] %>% # specify a column in that row
  .[[1]] 

# NOTE: Even though specifying a single row, then a single column within that row, 
# might be expected to produce a single element, instead it produces a list vector containing 
# a single element. The final .[[1]] is needed in order to extract that single element from the list.
# To see this re-run the above without the final line

limr_twomodels %>% 
  filter(country_code == "DEUTE") %>% # Filters on a row
  .[["mdl"]]


# The broom package makes it easier to work with model outputs by putting coefficients into 
# dataframes with a tidy data structure

#Using the model without interactions
limr_twomodels %>% 
  mutate(coeffs = map(mdl, tidy)) %>% 
  select(country_code, coeffs) %>% 
  unnest()

# And now using the model with interactions
limr_twomodels %>% 
  mutate(coeffs = map(mdl2, tidy)) %>% 
  select(country_code, coeffs) %>% 
  unnest()

# Let's compare interaction term (difference between male and female trend over time)
# for each country

limr_twomodels %>% 
  mutate(coeffs = map(mdl2, tidy)) %>% 
  select(country_code, coeffs) %>% 
  unnest() %>% 
  filter(term == "years_since_1900:sexmale") %>% 
  mutate(upper = estimate + 2 * std.error, lower = estimate - 2 * std.error) %>% 
  mutate(country_code = reorder(country_code, estimate))  %>% 
  ggplot(., aes(x = estimate, y = country_code)) + geom_point() + 
  geom_segment(aes(x = lower, xend = upper, y = country_code, yend = country_code, group = country_code)) + 
  geom_vline(xintercept = 0)



# We can also compare the model fit of the without-interaction with the with-interaction models 

limr_twomodels %>% 
  mutate(fit = map_dbl(mdl, AIC)) %>% 
  mutate(fit2 = map_dbl(mdl2, AIC)) %>%
  mutate(dif_fit = fit2 - fit) %>% 
  mutate(country_code = reorder(country_code, dif_fit)) %>% 
  ggplot(., aes(x = dif_fit, y = country_code)) + geom_point() + 
  geom_vline(xintercept = 0)

#We can also extract individual coefficients without using broom functions. In the example below we 
# are comparing the intercept against the trend for each country
dta_hmd %>% 
  filter(age == 0) %>% 
  mutate(imr = deaths / exposure, limr = log(imr, 10)) %>% 
  group_by(country_code) %>% 
  mutate(years_since_1900 = year - 1900) %>% 
  nest() %>% 
  mutate(mdl = map(data, function(x) {lm(limr ~ years_since_1900 + sex, data = x)})) %>% 
  mutate(intercept = map_dbl(mdl, function(x) {x %>% coefficients %>% .[["(Intercept)"]] })) %>% 
  mutate(trend = map_dbl(mdl, function(x) {x %>% coefficients %>% .[["years_since_1900"]]})) -> trend_intercept

# Let's plot this:
trend_intercept %>%  
  ggplot(., aes(x = intercept, y = trend)) + 
  geom_text(aes(label = country_code)) +
  stat_smooth( method = "lm", se = F)

# This looks like a strong negative correlation, implying some kind of 
# (affluent) worldwide convergence in infant mortality rates. Let's see how strong this correlation is 
# using the cor function

trend_intercept %>% 
  select(intercept, trend) %>% 
  cor


# 3_2_1 Trends in life expectancy

dta_hmd %>% 
  group_by(country_code, year, sex) %>% 
  mutate(
    age_death = age * deaths 
  ) %>% 
  summarise(mean_death = sum(age_death) / sum(deaths)) %>% 
  ungroup() %>% 
  mutate(years_since_1900 = year - 1900) -> e0_dta

# Here's a slightly less 'tidy' way of comparing model fits 
lm(mean_death ~ years_since_1900, data = e0_dta) -> mdl_01
lm(mean_death ~ years_since_1900 + sex, data = e0_dta) -> mdl_02
lm(mean_death ~ years_since_1900 * sex, data = e0_dta) -> mdl_03
lm(mean_death ~ years_since_1900 * sex + country_code, data = e0_dta) -> mdl_04

# Note that most of the models above are 'nested', 
#i.e. model 1 is model 2 with the 'sex' coefficient set to 0
# model 2 is model 3 with the 'sex:trend' coefficient set to 0
# model 3 is model 4 with the country dummy coefficients set to 0

# This nesting structure means we can do pairwise comparisons between models:
anova(mdl_01, mdl_02)
anova(mdl_02, mdl_03)
anova(mdl_03, mdl_04)

# In each case the pairwise comparisons suggest the additional model terms lead to 
# statistically significant improvements in model fit (i.e. the Pr(>F) values are very low)

# We can also compare more than two models at a time
anova(mdl_01, mdl_02, mdl_03, mdl_04)


# If the models were not nested we could compare using something like AIC or BIC
AIC(mdl_01, mdl_02, mdl_03, mdl_04)
BIC(mdl_01, mdl_02, mdl_03, mdl_04)
# In each case the results are substantively the same: we can and should include 
# individual country terms. 

# Given this let's look at some of the key results from model 4
mdl_04 %>% 
  tidy %>% 
  filter(str_detect(term, "country_code")) %>% 
  mutate(term = str_replace(term, "country_code", "")) %>% 
  mutate(term = reorder(term, estimate))  %>% 
  ggplot(., aes(x = estimate, y = term)) + geom_point() + 
  geom_segment(aes(x = estimate - 2 * std.error, xend = estimate + 2 * std.error, y = term, yend = term, group = term)) + 
  geom_vline(xintercept = 0)


# Question : which countries, or populations within countries, are the clearest outliers in these results?
