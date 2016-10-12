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
# # 2.3 Statistical analyses for HMD 
# source("scripts/2_3_hmd_stat_analyses.R")


# 3 Automating the production of graphs 

# Examples with HMD 

dta_hmd <- read_csv("tidied_data/tidied_hmd.csv")

# mortality at specific ages for different countries 
plot_fig <- function(df, CODE, XLIM, YLIM){
  df %>% 
    filter(age %in% c(0, 5, 20, 40, 60, 80)) %>%
    mutate(age = factor(age)) %>% 
    mutate(death_rate = deaths / exposure) %>% 
    ggplot(., aes(x = year, y = death_rate, group = age, colour = age, shape = age)) + 
    geom_line() + geom_point() + 
    facet_wrap(~sex) + 
    scale_x_continuous(name = "Year", limits = XLIM) +  
    scale_y_log10(name = "mortality risk", limits = YLIM) + 
    ggtitle(CODE) -> grph
  
  grph
}


dta_hmd %>% 
  filter(age %in% c(0, 5, 20, 40, 60, 80)) %>% 
  mutate(death_rate = deaths/ exposure) %>% 
  group_by(country_code) %>% 
  nest() %>% 
  mutate(
    graph = map2(data, country_code, plot_fig, XLIM = c(1900, 2010), YLIM = c(10^-5, 10^-0))
    ) -> figs_nested

         
pdf("figures/hmd_pdf.pdf", width = 8, height = 8)
figs_nested %>% .[["graph"]] %>% 
  walk(print)
dev.off()


# Population structure over time for different countries 

# Want to produce a separate pdf book for each year, and within each pdf book want to set ylim to 
# max pop size observed 

make_pop_pyramid_book <- function(df, code, POP_INC = 5000, AGE_LIMS = c(0, 90)){
  max_pop <- max(df$population)
  max_x_scale <- max_pop %/% POP_INC * POP_INC + POP_INC 
  
  make_single_pop_pyramid <- function(df_year, YEAR){
    df_year %>% 
      ggplot(., aes(y = age)) + 
      geom_segment(aes(x = -female, xend = 0, y = age, yend = age), colour = "red") + 
      geom_segment(aes(x = 0, xend = male, y = age, yend = age), colour = "blue") + 
      coord_cartesian(xlim = c(-max_x_scale, max_x_scale), ylim = AGE_LIMS) + 
      labs(x = "Population size", y = "Age", title = YEAR) -> gplt
    gplt
  }
  
  df %>% 
    filter(age >= AGE_LIMS[1], age <= AGE_LIMS[2]) %>% 
    filter(!is.na(year)) %>% 
    arrange(year) %>% 
    select(year, age, sex, population) %>% 
    group_by(year) %>% 
    spread(sex, population) %>% 
    nest() %>% 
    mutate(plt = map2(data, year, safely(make_single_pop_pyramid))) -> df_plots
  
  pdf_loc <- paste0("figures/pop_pyramids/", code, ".pdf")
  pdf(pdf_loc, height = 10, width = 10)
  df_plots %>% .[["plt"]] %>% walk(safely(print))
  dev.off()
  NULL
}
#debug(make_pop_pyramid_book)
  
dta_hmd %>% 
  filter(!is.na(year) & !is.na(age) & !is.na(sex)) %>% 
  group_by(country_code) %>% 
  nest() %>% 
  mutate(tmp = walk2(data, country_code, make_pop_pyramid_book))



