# checking when species were collected within a year
# seasonality and setting up for use in SPECRICH2
# - and making a figure for seasonality

# load libraries ####
library(here)
library(tidyverse)
library(naniar)
library(VennDiagram)
# to save data as excel files
library(openxlsx)

# set relative pathways ####
data_path <- here::here("data")
figure_path <- here::here("figs")
Rsource_path <- here::here("R/Resources")

# any functions needed ####
`%nin%` = Negate(`%in%`) # will make it so that can filter by things that are not in 

# read in data file ####
condensed_dataset <- read_csv(paste(data_path, "Dataset1.csv", sep="/"),
                           col_types = 
                             cols( 
                               .default = col_character()))
condensed_dataset

# set up the seasons ####

condensed_dataset %>%
  count(month) # have a few (total of 12) strange months, will remove them for the seasonal estimates

condensed_dataset2 <- condensed_dataset %>%
  mutate(month = ifelse(month == "9-Aug", "8-9", month)) %>%
  filter(month != "8-9" & !is.na(month)) %>%
  mutate(month2 = as.numeric(month))

# how many records were removed by removing records that didn't have a month or had a range for the month
condensed_dataset %>%
  mutate(month = ifelse(month == "9-Aug", "8-9", month)) %>%
  filter(month == "8-9" | is.na(month)) # 11

condensed_dataset2 %>%
  count(month, month2)

condensed_dataset3 <- condensed_dataset2 %>%
  mutate(seasonality = if_else(month == "1" | month == "2" | month == "12", "winter", month)) %>%
  mutate(seasonality = if_else(month == "3" | month == "4" | month == "5", "spring", seasonality)) %>%
  mutate(seasonality = if_else(month == "6" | month == "7" | month == "8", "summer", seasonality)) %>%
  mutate(seasonality = if_else(month == "9" | month == "10" | month == "11", "fall", seasonality)) %>%
  mutate(season_order = if_else(seasonality == "winter", "1", "NA")) %>%
  mutate(season_order = if_else(seasonality == "spring", "2", season_order)) %>%
  mutate(season_order = if_else(seasonality == "summer", "3", season_order)) %>%
  mutate(season_order = if_else(seasonality == "fall", "4", season_order)) %>%
  mutate(season_order = as.numeric(season_order))
condensed_dataset3 %>% 
  count(month, seasonality, season_order)

# make a venn diagram of when species are captured ####
winter_sampling <- condensed_dataset3 %>%
  filter(seasonality == "winter")

spring_sampling <- condensed_dataset3 %>%
  filter(seasonality == "spring")

summer_sampling <- condensed_dataset3 %>%
  filter(seasonality == "summer")

fall_sampling <- condensed_dataset3 %>%
  filter(seasonality == "fall")     

winter_sampling_spp <- winter_sampling %>%
  count(scientific_name) %>%
  rename(winter = n)
nrow(winter_sampling_spp) # 8 

spring_sampling_spp <- spring_sampling %>%
  count(scientific_name) %>%
  rename(spring = n)
nrow(spring_sampling_spp) # 243


summer_sampling_spp <- summer_sampling %>%
  count(scientific_name) %>%
  rename(summer = n)
nrow(summer_sampling_spp) # 170


fall_sampling_spp <- fall_sampling %>%
  count(scientific_name) %>%
  rename(fall =  n)
nrow(fall_sampling_spp) # 110


species_tibble <- condensed_dataset3 %>%
  count(scientific_name) %>%
  select(-n)
nrow(species_tibble) #328 species

species_list_by_season <- species_tibble %>% 
  left_join(winter_sampling_spp) %>%
  left_join(spring_sampling_spp) %>%
  left_join(summer_sampling_spp) %>%
  left_join(fall_sampling_spp)
species_list_by_season #328

area1_num <- nrow(species_list_by_season %>% filter(!is.na(winter))) # 8
area2_num <- nrow(species_list_by_season %>% filter(!is.na(spring))) # 243
area3_num <- nrow(species_list_by_season %>% filter(!is.na(summer))) # 170
area4_num <- nrow(species_list_by_season %>% filter(!is.na(fall))) # 110


n1234_num <- nrow(species_list_by_season %>%
                    filter(!is.na(winter) & !is.na(spring) & !is.na(summer) & !is.na(fall))) # 5

n123_num <- nrow(species_list_by_season %>%
                   filter(!is.na(winter) & !is.na(spring) & !is.na(summer))) # 5

n124_num <- nrow(species_list_by_season %>%
                   filter(!is.na(winter) & !is.na(spring) & !is.na(fall))) # 5

n134_num <- nrow(species_list_by_season %>%
                   filter(!is.na(winter) & !is.na(summer) & !is.na(fall))) # 5

n234_num <- nrow(species_list_by_season %>%
                   filter(!is.na(spring) & !is.na(summer) & !is.na(fall))) # 58


n12_num <- nrow(species_list_by_season %>%
                  filter(!is.na(winter) & !is.na(spring))) # 7

n13_num <- nrow(species_list_by_season %>%
                  filter(!is.na(winter) & !is.na(summer))) # 6

n14_num <- nrow(species_list_by_season %>%
                  filter(!is.na(winter) & !is.na(fall))) # 5

n23_num <- nrow(species_list_by_season %>%
                  filter(!is.na(spring) & !is.na(summer))) # 112

n24_num <- nrow(species_list_by_season %>%
                  filter(!is.na(spring) & !is.na(fall))) # 64

n34_num <- nrow(species_list_by_season %>%
                  filter(!is.na(summer) & !is.na(fall))) # 77

dev.off()


seasonality_spp_venn_color_blind <- draw.quad.venn(area1 = area1_num, area2 = area2_num, area3 = area3_num, area4 = area4_num,
                                                   n12 = n12_num, n23 = n23_num, n13 = n13_num, n14 = n14_num, 
                                                   n24 = n24_num, n34 = n24_num, n124 = n124_num,
                                                   n123 = n123_num, n134 = n134_num, n234 = n234_num, n1234 = n1234_num,
                                                   category = c("Winter", "Spring", "Summer", "Fall"),
                                                   fill = c("white", "#CC79A7", "#009E73", "#56B4E9"),
                                                   cex = 2, cat.cex = 1) #fontface = "bold", cat.fontface = "bold", cat.dist = .15

dev.off()


# save figure ####

#ggsave("spp_seasonality_color_blind.png", width = 3, height = 3,
#       units = "in", dpi = 600, plot = seasonality_spp_venn_color_blind,
#       path = figure_path, family = "Arial") 


# set up season data for obtaining seasonal estimates in SPECRICH2 ####
# - survey defined as collection date (day, month, year) but split now between the spring, summer, and fall seasons

# -- SPRING ####
spring_sampling2 <- spring_sampling %>%
  count(scientific_name, day, month, year) %>%
  select(-n)
spring_sampling2


# - need to get the number of dates each species is captured in per year

spring_spp_counts <- spring_sampling2 %>%
  count(scientific_name, year) %>%
  rename(num_times_spp_collected = n) %>%
  arrange(year)
spring_spp_counts

# - need thenumber of collection events per year
spring_collection_events <- spring_sampling2 %>%
  count(day, month, year) %>%
  count(year) %>%
  rename(num_total_collection_events = n) %>%
  arrange(year)
spring_collection_events


# - for each year, need to know the number of species collected once, twice, three times, etc...
# up to the largest number of surveys in that year 

SPECRICH2_input_spring <- spring_spp_counts %>%
  count(year, num_times_spp_collected) %>%
  arrange(year, num_times_spp_collected) %>%
  rename(num_spp_coll_n_surveys = n) %>%
  rename(num_surveys = num_times_spp_collected) %>%
  left_join(spring_collection_events)
SPECRICH2_input_spring

# - also need to know the number of species collected in each collection_event
spring_spp_collected_n_events <- spring_sampling2 %>%
  count(year, day, month) %>%
  rename(num_spp_captured = n) %>%
  arrange(year, month, day)
spring_spp_collected_n_events


# check if the above mataches with the number of collection_events per year
spring_collection_events %>%
  #filter(year == "1902") # 4
  #filter(year == "1920") # 0
  #filter(year == "2015") # 22
  filter(year == "2009") # 5

spring_spp_collected_n_events %>%
  #filter(year == "1902") # 4
  #filter(year == "1920") # 0
  #filter(year == "2015") # 22
  filter(year == "2009")
  
## save spring outputs ####
# - commented out to not save over as will have output data
# from SPECRICH2 as another sheet

#https://datavizpyr.com/save-multiple-dataframes-to-a-single-excel-file-in-r/

#spring_workbook <- createWorkbook()

#addWorksheet(spring_workbook, sheetName="SPECRICH2_input_spring")
#addWorksheet(spring_workbook, sheetName="spring_spp_collected_n_events")
#addWorksheet(spring_workbook, sheetName="spring_collection_events")

#writeData(spring_workbook, "SPECRICH2_input_spring", SPECRICH2_input_spring)
#writeData(spring_workbook, "spring_spp_collected_n_events", spring_spp_collected_n_events)
#writeData(spring_workbook, "spring_collection_events", spring_collection_events)

#saveWorkbook(spring_workbook,
#             file = paste(data_path, "SPECRICH2_Spring_input_data_surveyAsDMYYYY.xlsx", sep="/"),
#             overwrite = FALSE)


# -- SUMMER ####
summer_sampling2 <- summer_sampling %>%
  count(scientific_name, day, month, year) %>%
  select(-n)
summer_sampling2


# - need to get the number of dates each species is captured in per year

summer_spp_counts <- summer_sampling2 %>%
  count(scientific_name, year) %>%
  rename(num_times_spp_collected = n) %>%
  arrange(year)
summer_spp_counts

# - need thenumber of collection events per year
summer_collection_events <- summer_sampling2 %>%
  count(day, month, year) %>%
  count(year) %>%
  rename(num_total_collection_events = n) %>%
  arrange(year)
summer_collection_events


# - for each year, need to know the number of species collected once, twice, three times, etc...
# up to the largest number of surveys in that year 

SPECRICH2_input_summer <- summer_spp_counts %>%
  count(year, num_times_spp_collected) %>%
  arrange(year, num_times_spp_collected) %>%
  rename(num_spp_coll_n_surveys = n) %>%
  rename(num_surveys = num_times_spp_collected) %>%
  left_join(summer_collection_events)
SPECRICH2_input_summer

# - also need to know the number of species collected in each collection_event
summer_spp_collected_n_events <- summer_sampling2 %>%
  count(year, day, month) %>%
  rename(num_spp_captured = n) %>%
  arrange(year, month, day)
summer_spp_collected_n_events


# check if the above mataches with the number of collection_events per year
summer_collection_events %>%
  #filter(year == "1902") # 4
  #filter(year == "1920") # 3
  #filter(year == "2015") # 35
  filter(year == "2009") # 13

summer_spp_collected_n_events %>%
  #filter(year == "1902") # 4
  #filter(year == "1920") # 3
  #filter(year == "2015") # 35
  filter(year == "2009") # 13
  
## save summer outputs ####
# - commented out to not save over as will have output data
# from SPECRICH2 as another sheet

#https://datavizpyr.com/save-multiple-dataframes-to-a-single-excel-file-in-r/

#summer_workbook <- createWorkbook()

#addWorksheet(summer_workbook, sheetName="SPECRICH2_input_summer")
#addWorksheet(summer_workbook, sheetName="summer_spp_collected_n_events")
#addWorksheet(summer_workbook, sheetName="summer_collection_events")

#writeData(summer_workbook, "SPECRICH2_input_summer", SPECRICH2_input_summer)
#writeData(summer_workbook, "summer_spp_collected_n_events", summer_spp_collected_n_events)
#writeData(summer_workbook, "summer_collection_events", summer_collection_events)

#saveWorkbook(summer_workbook,
#             file = paste(data_path, "SPECRICH2_Summer_input_data_surveyAsDMYYYY.xlsx", sep="/"),
#             overwrite = FALSE)

# -- FALL ####
fall_sampling2 <- fall_sampling %>%
  count(scientific_name, day, month, year) %>%
  select(-n)
fall_sampling2


# - need to get the number of dates each species is captured in per year

fall_spp_counts <- fall_sampling2 %>%
  count(scientific_name, year) %>%
  rename(num_times_spp_collected = n) %>%
  arrange(year)
fall_spp_counts

# - need thenumber of collection events per year
fall_collection_events <- fall_sampling2 %>%
  count(day, month, year) %>%
  count(year) %>%
  rename(num_total_collection_events = n) %>%
  arrange(year)
fall_collection_events


# - for each year, need to know the number of species collected once, twice, three times, etc...
# up to the largest number of surveys in that year 

SPECRICH2_input_fall <- fall_spp_counts %>%
  count(year, num_times_spp_collected) %>%
  arrange(year, num_times_spp_collected) %>%
  rename(num_spp_coll_n_surveys = n) %>%
  rename(num_surveys = num_times_spp_collected) %>%
  left_join(fall_collection_events)
SPECRICH2_input_fall

# - also need to know the number of species collected in each collection_event
fall_spp_collected_n_events <- fall_sampling2 %>%
  count(year, day, month) %>%
  rename(num_spp_captured = n) %>%
  arrange(year, month, day)
fall_spp_collected_n_events


# check if the above mataches with the number of collection_events per year
fall_collection_events %>%
  #filter(year == "1902") # 0
  #filter(year == "1920") # 6
  #filter(year == "2015") # 0
  filter(year == "2009") # 2

fall_spp_collected_n_events %>%
  #filter(year == "1902") # 0
  #filter(year == "1920") # 6
  #filter(year == "2015") # 0
  filter(year == "2009") # 2

## save fall outputs ####
# - commented out to not save over as will have output data
# from SPECRICH2 as another sheet

#https://datavizpyr.com/save-multiple-dataframes-to-a-single-excel-file-in-r/

#fall_workbook <- createWorkbook()

#addWorksheet(fall_workbook, sheetName="SPECRICH2_input_fall")
#addWorksheet(fall_workbook, sheetName="fall_spp_collected_n_events")
#addWorksheet(fall_workbook, sheetName="fall_collection_events")

#writeData(fall_workbook, "SPECRICH2_input_fall", SPECRICH2_input_fall)
#writeData(fall_workbook, "fall_spp_collected_n_events", fall_spp_collected_n_events)
#writeData(fall_workbook, "fall_collection_events", fall_collection_events)

#saveWorkbook(fall_workbook,
#             file = paste(data_path, "SPECRICH2_Fall_input_data_surveyAsDMYYYY.xlsx", sep="/"),
#             overwrite = FALSE)

# -- WINTER ####
winter_sampling %>%
  count(day, month, year) %>%
  count(year)
# A tibble: 4 x 2
#year      n
#<chr> <int>
#1 2013      1
#2 2015      2
#3 2016      2
#4 2018      2

# none of the years have at least 3 unique collection events
