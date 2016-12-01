# Script 0_0 : Master script 
# 6 / 10 / 2016

# Description


# Standard sequence:

# 0) Clear workspace and load prereq packages 
# 1) Load and tidy raw data (once) 
# 2) Load tidied data 
# 3) Ad hoc analyses 
# 4) Automated analyses & polished analyses/results



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

# # 2.1  Exploratory data analysis for HFD
# source("scripts/2_1_hfd_exploratory_data_analysis.R")
# # 2.2 Exploratory data analysis for HMD 
# source("scripts/2_2_hmd_exploratory_data_analysis.R")



# 3 Automating the production of graphs, files, and model outputs

# # 3.1 Automated figure and file production using HFD
#source("scripts/3_1_hfd_data_vis_and_output.R")

# # 3.2 Automated production of model outputs using HMD
#source("scripts/3_2_hmd_stat_analyses.R")







