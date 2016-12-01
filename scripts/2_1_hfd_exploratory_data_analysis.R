
# 2.1.1 - Load the dataset

dta_hfd <- read_csv("tidied_data/tidied_hfd.csv")


# 2.1.2 - Calculate total births by year

dta_hfd %>% 
  group_by(year) %>% 
  summarise(total_births = sum(total)) %>% 
  ggplot(., aes(x = year, y = total_births)) + 
  geom_line()

# Note, the number of countries with records submitted and accepted by the HFD are different 
# for each year, which will affect the number of births. We can see this as follows:

# 2.1.3 - Unique countries in each year 
dta_hfd %>% 
  group_by(year) %>% 
  summarise(num_countries = length(unique(code))) %>% 
  ggplot(. , aes(x = year, y = num_countries)) + 
  geom_step()


# 2.1.4 - Cumulative period fertility by different ages 
# Many countries have undergone drastic changes in fertility. We can explore this by first calculating the 
# age-specific fertility levels (ASFRs), and then for each year and each country, 'adding up' 
# different ASFRs to get cumulative (period) fertility by different ages. This is done in the code below

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

# 2.1.5 - Age at which synthetic cohorts reach replacement fertility levels 
# We might be especially interested firstly in whether, and secondly by which age, populations 
# in different countries reach replacement fertility levels, i.e. slightly over two babies per woman. 

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

# 2.1.6 - Teenage pregnancy rates over time 
# Another important comparison is in rates of teenage pregnancy in different countries. 

dta_hfd %>% 
  filter(age < 20) %>% 
  group_by(code, year) %>% 
  summarise(total = sum(total), exposure = sum(exposure)) %>% 
  mutate(teenage_fert = total / exposure) %>% 
  ggplot(., aes(x = year, y = teenage_fert)) + 
  geom_line() + 
  facet_wrap(~code)

# 2.1.7 - Rank of teenage pregancy rates in 1980, 1990 and 2000
# We might also be interested in how, in any particular year, one country ranks with others in its 
# levels of teenage pregnancies.

# By default, the first five rows of a (tibble) dataframe object are displayed.

dta_hfd %>% 
  filter(age < 20) %>% 
  group_by(code, year) %>% 
  summarise(total = sum(total), exposure = sum(exposure)) %>% 
  mutate(teenage_fert = total / exposure) %>%
  filter(year == 1980) %>% 
  ungroup() %>% 
  arrange(teenage_fert)

# We can change the order in which rows are displayed by using the desc function

dta_hfd %>% 
  filter(age < 20) %>% 
  group_by(code, year) %>% 
  summarise(total = sum(total), exposure = sum(exposure)) %>% 
  mutate(teenage_fert = total / exposure) %>%
  filter(year == 1980) %>% 
  ungroup() %>% 
  arrange(desc(teenage_fert))

# We can display a larger number of rows, and start to filter and arrange the tables ourselves, using 
# the View() function

dta_hfd %>% 
  filter(age < 20) %>% 
  group_by(code, year) %>% 
  summarise(total = sum(total), exposure = sum(exposure)) %>% 
  mutate(teenage_fert = total / exposure) %>%
  filter(year %in% c(1970, 1980, 1990, 2000, 2010)) %>% 
  group_by(year) %>% 
  mutate(rnk = min_rank(teenage_fert)) %>%
  select(code, year, rnk) %>% 
  spread(year, rnk) %>% 
  View()

# Finally, we may want to filter in on a small number of countries to see how their 
# teenage prenancy ranks have changed over time

dta_hfd %>% 
  filter(age < 20) %>% 
  group_by(code, year) %>% 
  summarise(total = sum(total), exposure = sum(exposure)) %>% 
  mutate(teenage_fert = total / exposure) %>%
  filter(year %in% c(1970, 1980, 1990, 2000, 2010)) %>% 
  group_by(year) %>% 
  mutate(rnk = min_rank(teenage_fert)) %>%
  select(code, year, rnk) %>% 
  ungroup() %>% 
  filter(code %in% c("GBRTENW", "FRATNP", "GBR_SCO", "USA", "RUS", "JPN")) %>% 
  ggplot(., aes(x = year, y = rnk, colour = code, group = code, shape = code)) + 
  geom_line() + geom_point()


# 2.1.8 Older age pregnancy rates over time 
# We might also be interested in fertility levels in different countries at older ages, in this case,
# at the age of 40 years and above

dta_hfd %>% 
  filter(age >= 40) %>% 
  group_by(code, year) %>% 
  summarise(total = sum(total), exposure = sum(exposure)) %>% 
  mutate(teenage_fert = total / exposure) %>% 
  ggplot(., aes(x = year, y = teenage_fert)) + 
  geom_line() + 
  facet_wrap(~code)

# We might be interested primarily in fertility at older ages in more recent years, in this case 
# after 1990.

dta_hfd %>% 
  filter(year >= 1990) %>% 
  filter(age >= 40) %>% 
  group_by(code, year) %>% 
  summarise(total = sum(total), exposure = sum(exposure)) %>% 
  mutate(teenage_fert = total / exposure) %>% 
  ggplot(., aes(x = year, y = teenage_fert)) + 
  geom_line() + 
  facet_wrap(~code)


# We might also want to know how rates of fertility in older ages have changed 
# between two periods, in the example below, between 1980 and 2000

dta_hfd %>% 
  filter(age >= 40) %>% 
  group_by(code, year) %>% 
  summarise(total = sum(total), exposure = sum(exposure)) %>% 
  mutate(older_fert = total / exposure) %>% 
  ungroup() %>% 
  filter(year %in% c(1980, 2000)) %>% 
  select(code, year, older_fert) %>% 
  spread(year, older_fert) %>% 
  filter(!is.na(`1980`) & !is.na(`2000`)) %>% 
  arrange(`1980`) %>% 
  mutate(rank_1980 = rank(`1980`), rank_2000 = rank(`2000`)) %>% 
  mutate(change_rank = rank_2000 - rank_1980) %>% 
  View()


# 2.1.9 - Age of peak fertility
# Total period fertility has fallen in many countries, 
# and alongside this age of peak fertility has increased, and the 
# level of fertility at this age has decreased. We can explore the latter two 
# changes as follows.

dta_hfd %>% 
  mutate(fert = total / exposure) %>% 
  group_by(code, year) %>% 
  filter(fert == max(fert)) %>% 
  ungroup() %>% 
  ggplot(., aes(x = year, y = age, colour = fert)) +
  geom_line() +
  facet_wrap(~ code) +
  scale_colour_gradient(low = "white", high = "black")


# 2.1.10 - Mean age of fertility 
# We might also be interested in a combination the mean age of fertility and 
# the variance around this mean age. The formulae for calculating both is available in the following: 

# Using formulae from :
# http://ocw.jhsph.edu/courses/demographicmethods/PDFs/idm-sec9.pdf

# We can calculate and plot the changing relationship over time using the following code:

dta_hfd %>% 
  mutate(fert = total / exposure) %>% 
  group_by(code, year) %>% 
  mutate(
    mn_age = sum((age + 0.5) * fert) / sum(fert), 
    df_mn_age_sq = (age - mn_age)^2
    ) %>% 
  summarise(mn_age = mn_age[1], var_age = sum(df_mn_age_sq * fert) / sum(fert)) %>% 
  ungroup() %>% 
  arrange(code, year) %>% 
  ggplot(., aes(x = mn_age, y = var_age, colour = year)) + 
  geom_point() + 
  facet_wrap(~ code) + 
  scale_colour_gradient(low = "white", high = "black") + 
  labs(x = "Mean age of childbearing (years)", y = "Variance of age of childbearing (years squared)")


# We could also produce an even more complex visualisation which represents more dimensions of the data 

dta_hfd %>% 
  mutate(fert = total / exposure) %>% 
  group_by(code, year) %>% 
  mutate(
    mn_age = sum((age + 0.5) * fert) / sum(fert), 
    df_mn_age_sq = (age - mn_age)^2
    
  ) %>% 
  summarise(
    total_fert = sum(fert),
    mn_age = mn_age[1], 
    var_age = sum(df_mn_age_sq * fert) / total_fert) %>% 
  ungroup() %>% 
  arrange(code, year) %>% 
  ggplot(., aes(x = mn_age, y = var_age, alpha = year, colour = total_fert)) + 
  geom_point(stroke = 0) + 
  facet_wrap(~ code) + 
  scale_colour_distiller(palette = "Spectral") + 
  labs(x = "Mean age of childbearing (years)", y = "Variance of age of childbearing (years squared)")


# We might be more interested only in trends after 1970, as fertility fell in many countries in the 1960s
# due, for example, to more easily available and effective contraception
dta_hfd %>% 
  filter(year >= 1970) %>% 
  mutate(fert = total / exposure) %>% 
  group_by(code, year) %>% 
  mutate(
    mn_age = sum((age + 0.5) * fert) / sum(fert), 
    df_mn_age_sq = (age - mn_age)^2
  ) %>% 
  summarise(mn_age = mn_age[1], var_age = sum(df_mn_age_sq * fert) / sum(fert)) %>% 
  ungroup() %>% 
  arrange(code, year) %>% 
  ggplot(., aes(x = mn_age, y = var_age, colour = year)) + 
  geom_point() + 
  facet_wrap(~ code) + 
  scale_colour_gradient(low = "white", high = "black") + 
  labs(x = "Mean age of childbearing (years)", y = "Variance of age of childbearing (years squared)")

# Similarly, we may wish to at an even more recent period of time, after 1990, as we may expect 
# large-scale changes due to the collapse of  the USSR
dta_hfd %>% 
  filter(year >= 1990) %>% 
  mutate(fert = total / exposure) %>% 
  group_by(code, year) %>% 
  mutate(
    mn_age = sum((age + 0.5) * fert) / sum(fert), 
    df_mn_age_sq = (age - mn_age)^2
  ) %>% 
  summarise(mn_age = mn_age[1], var_age = sum(df_mn_age_sq * fert) / sum(fert)) %>% 
  ungroup() %>% 
  arrange(code, year) %>% 
  ggplot(., aes(x = mn_age, y = var_age, colour = year)) + 
  geom_point() + 
  facet_wrap(~ code) + 
  scale_colour_gradient(low = "white", high = "black") + 
  labs(x = "Mean age of childbearing (years)", y = "Variance of age of childbearing (years squared)")





