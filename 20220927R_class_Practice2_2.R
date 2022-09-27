# Practice 2.2 ----
# Author: Berrica
# Student no.: R11241203
# Date:2022-09-27

#read the file
rairuoho <- read.table(url('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/rairuoho.txt'), header = T)

#Replace nutrient with enriched in the data set.
rairuoho$treatment <- ifelse(rairuoho$treatment == 'nutrient', 'enriched', 'water')

#Reformat the table in order to have the day as a single variable (factor) 
#containing 6 levels (day3, day4, day5, day6, day7, day8).
library('dplyr')
library('tidyr')
rairuoho_day <- rairuoho %>% pivot_longer(cols = 1:6, 
                                          names_to = 'day', 
                                          values_to = 'length')



#Merge variables Spatial1 and Spatial2 
library('tidyverse')
rairuoho_spatial <- rairuoho_day %>%  
  mutate(spacial.coordinate = str_c(spatial1, spatial2, sep = "_")) #str_c(): to glue two characters together


#Remove variables row and column
rairuoho_goal <- rairuoho_spatial %>% select(-spatial1, -spatial2, -row, -column) 
rairuoho_goal
