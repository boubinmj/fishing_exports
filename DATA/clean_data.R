library("dplyr")
library("shiny")
library('magrittr')
library('shinydashboard')
library('DT')
library('googleVis')
library('reshape2')
library('tidyr')

##############################################
#################### GLOBAL ##################
##############################################

# Import Data Set

fish_data <-
  read.csv("Fish_FLD.csv", header = TRUE)
head(fish_data)

# Generate totals per country, per year

by_years <- fish_data %>%
  filter(., Measure == "Tonnes") %>%
  select(., Country, Year, Value) %>%
  group_by(., Country) %>%
  group_by(., Year, .add = TRUE) %>%
  summarise(., total_yr = sum(Value)) %>%
  spread(Year, total_yr) %>%
  mutate_if(is.numeric , replace_na, replace = 0)
head(by_years)

# generate totals per country, per species

by_species <- fish_data %>%
  filter(., Measure == "Tonnes") %>%
  select(., Country, Species, Value) %>%
  group_by(., Country) %>%
  group_by(., Species, .add = TRUE) %>%
  summarise(., total_sp = sum(Value)) %>%
  spread(Species, total_sp) %>%
  mutate_if(is.numeric, replace_na, replace = 0)
head(by_species)

by_Norway <- fish_data %>%
  filter(., Country == "Norway") %>%
  filter(., Measure == "Tonnes") %>%
  select(.,Species,Year, Value) %>% 
  group_by(., Species) %>% 
  spread(Species, Value) %>% 
  mutate_if(is.numeric, replace_na, replace = 0)
head(by_Norway)

write.csv(by_years, 'by_years.csv')
write.csv(by_species, 'by_species.csv')
write.csv(by_Norway, 'by_norway.csv')