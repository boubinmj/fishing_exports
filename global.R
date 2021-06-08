library("dplyr")
library("shiny")
library('magrittr')
library('shinydashboard')
library('DT')
library('googleVis')
library('reshape2')
library('tidyr')

by_years = read.csv('DATA/by_years.csv', header = TRUE)
by_species = read.csv('DATA/by_species.csv', header = TRUE)
by_Norway = read.csv('DATA/by_norway.csv', header = TRUE)

colnames(by_years)[-1][-1] = sub('X', '', colnames(by_years)[-1][-1])

head(by_years)
head(by_species)
head(by_Norway)

species <- colnames(by_Norway)[-1][-1]
years_a <- colnames(by_years)[-1][-1]
countries <- by_species["Country"]



# countries <- distinct(by_years %>% select(., Country))
# species <- distinct(by_years %>% select(., Species))
# years_a <- distinct(by_species %>% select(., Year))