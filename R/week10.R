# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)

# Data Import and Cleaning
gss_tbl <- read_sav(file = "../data/GSS2016.sav") %>% 
  # Remove anyone who has a missing value for the workhours (HRS1) variable
  filter(!is.na(HRS1)) %>% 
  # Rename hrs1 to workhours %>% 
  rename(workhours = HRS1) %>% 
  # Use colMeans to calculate the % of missing values in each column and retain only variables with less than 75% missingness
  select_if(colMeans(is.na(.)) < .75)

# Visualization
# Visualization of the univariate distribution of workhours




  