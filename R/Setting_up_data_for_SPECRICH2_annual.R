## to rearrange the data so can use with SPECRICH2
# https://www.mbr-pwrc.usgs.gov/software/specrich2.html

# load libraries ####
library(here)
library(tidyverse)
library(naniar)
library(openxlsx)

# set relative pathways ####
data_path <- here::here("data")
figure_path <- here::here("figs")
Rsource_path <- here::here("R/Resources")

# any functions needed ####
`%nin%` = Negate(`%in%`) # will make it so that can filter by things that are not in

# read in data file ####
condensed_data <- read_csv(paste(data_path, "Dataset1.csv", sep="/"),
                           col_types = 
                             cols( 
                               .default = col_character()))
condensed_data

# rearrange data for use with SPECRICH2 ####
# - defining a survey as the date collected irrespective of who or where
# it was collected within Wake County

collapsed_assumed_netted_data <- condensed_data %>%
  count(scientific_name, family, day, month, year)
collapsed_assumed_netted_data

# - need to get the number of times each species is captured per year
spp_counts_per_year <- collapsed_assumed_netted_data %>%
  count(scientific_name, year) %>%
  rename(num_times_spp_collected = n) %>%
  arrange(year)
spp_counts_per_year

# - need the number of collection events (i.e. dates collected on) per year
num_unique_collection_events_per_year <- collapsed_assumed_netted_data %>%
  count(day, month, year) %>%
  count(year) %>%
  rename(num_collection_events = n) %>%
  arrange(year)
num_unique_collection_events_per_year

# - for each year, need to know the number of species collected once, twice, three times, etc...
# up to the largest number of surveys in that year

SPECRICH2_input_data <- spp_counts_per_year %>%
  count(year, num_times_spp_collected) %>%
  arrange(year,num_times_spp_collected) %>%
  rename(num_spp_coll_n_surveys = n) %>%
  rename(num_surveys = num_times_spp_collected) %>%
  left_join(num_unique_collection_events_per_year)
SPECRICH2_input_data

# - also need to know the number of species collected in each collection_event
num_spp_coll_per_event_per_year <- collapsed_assumed_netted_data %>%
  count(day, month, year) %>%
  rename(num_spp_captured = n) %>%
  arrange(year, month, day)
num_spp_coll_per_event_per_year

# check if the above mataches with the number of collection_events per year
num_unique_collection_events_per_year %>%
  #filter(year == "1902") # 8
  #filter(year == "1920") # 9
  filter(year == "2015") # 59

num_spp_coll_per_event_per_year %>%
  #filter(year == "1902") # 8
  #filter(year == "1920") # 9
  filter(year == "2015") # 59

## save annual outputs ####
# - commented out to not save over as will have additional sheets added (README) and will have output data
# from SPECRICH2 as another sheet


#https://datavizpyr.com/save-multiple-dataframes-to-a-single-excel-file-in-r/

#annual_workbook <- createWorkbook()

#addWorksheet(annual_workbook, sheetName="input_data")
#addWorksheet(annual_workbook, sheetName="num_spp_coll_n_per_year")
#addWorksheet(annual_workbook, sheetName="num_unique_n_per_year")

#writeData(annual_workbook, "input_data", SPECRICH2_input_data)
#writeData(annual_workbook, "num_spp_coll_n_per_year", num_spp_coll_per_event_per_year)
#writeData(annual_workbook, "num_unique_n_per_year", num_unique_collection_events_per_year)

#saveWorkbook(annual_workbook,
#             file = paste(data_path, "SPECRICH2_Annual_input_data_surveyAsDMYYYY.xlsx", sep="/"),
#             overwrite = FALSE)


