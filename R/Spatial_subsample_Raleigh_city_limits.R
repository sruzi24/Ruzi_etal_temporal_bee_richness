# Run occupancy models on a spatial subset of the data to help control for spatial sampling

# -- running this on the current municipal limits of Raleigh by reverse geolocating the records
# to see what city they fall under and then only analyzing data from Raleigh
# - will also check and remove records that were geolocated to Raleigh from general locality information
# (in other words, had a vague Wake Co. written in for locality which when using Google Earth to geolocate
# would end up placing the coordinates in the middle of Wake Co which is where Raleigh is approximately)

# using https://cran.r-project.org/web/packages/tidygeocoder/readme/README.html to reverse geocode

# load libraries ####
library(tidyverse)
library(here)
library(naniar)
library(tidygeocoder)
library(RPresence)
library(iNEXT)

# set relative pathways ####
data_path <- here::here("data")
figure_path <- here::here("figs")
Rsource_path <- here::here("R/Resources")

# load in additional R scripts ####
source(paste(Rsource_path, "NC_county_mapping_polygons.R", sep = "/"))

# any functions needed ####
`%nin%` = Negate(`%in%`) # will make it so that can filter by things that are not in 

# read in data file ####
condensed_data <- read_csv(paste(data_path, "Dataset1.csv", sep="/"),
                           col_types = 
                             cols( 
                               .default = col_character(),
                               decimal_longitude = col_double(),
                               decimal_latitude = col_double()))
condensed_data





# condense coordinates down to make reverse geocoding quicker ####

condensed_coordinates <- condensed_data %>%
  count(decimal_longitude, decimal_latitude) 
condensed_coordinates

# reverse geocode from coordinates ####
# the following takes 5 minutes to run, will comment it out and save the output so
# do not need to rerun this
#reverse_geocoded_locations <- condensed_coordinates %>%
#  reverse_geocode(lat = decimal_latitude, long = decimal_longitude, method = "osm",
#                  address = address_found, full_results = TRUE)

# save the output
#save(reverse_geocoded_locations, 
#     file = paste(Rsource_path, "reverse_geocoded_locations.rda", sep = "/"))

load(paste(Rsource_path,"reverse_geocoded_locations.rda", sep = "/"))

reverse_geocoded_locations
reverse_geocoded_locations %>%
  count(city)
# A tibble: 8 x 2
#city                    n
#<chr>               <int>
#1 Cary                   49
#3 Durham                  1
#4 Raleigh               171
#6 Wake Forest             1
#8 NA                     91

str(reverse_geocoded_locations)
reverse_geocoded_locations2 <- reverse_geocoded_locations %>%
  select(-county,-n) %>%
  mutate(decimal_latitude = as.numeric(decimal_latitude)) %>%
  mutate(decimal_longitude = as.numeric(decimal_longitude))

# plot locations by city color to visually inspect
city_sample_locations <- nc_wake_grey1 + 
  geom_point(data = reverse_geocoded_locations, aes(x = as.numeric(decimal_longitude), y = as.numeric(decimal_latitude), color = city), 
             inherit.aes = FALSE) +
  labs(color = "City") +
  guides(color=guide_legend(nrow=3, byrow=TRUE))+
  theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
        legend.background=element_blank(), # removes the overall border
        legend.key=element_blank(), #remove the border around each item
        legend.position = "bottom")
city_sample_locations

#ggsave("city_sample_locations.png", width = 5, height = 5.5,
#       units = "in", dpi = 600, plot = city_sample_locations,
#       path = figure_path, bg = "transparent")


# join original data and reverse geocoded city location ####
condensed_data2 <- condensed_data %>%
  # create a unique row number identifier to see if joining data causes repeats
  left_join(reverse_geocoded_locations2)
condensed_data2


condensed_data2 %>%
  # filter to only records reverse located to Raleigh
  filter(city == "Raleigh") %>%
  count(decimal_latitude, decimal_longitude, city, where_obtained_coordinates) %>%
  # remove dataset provided coordinates
  filter(where_obtained_coordinates != "dataset provided") %>%
  print(n = 50)
# there are 31 unique location records that need to be checked
# of these, some records were georeferenced in google earth using Wake County, so will remove these
# from the dataset for the subset of spatial analyses

# remove uncertain georeferencing records that were assigned to Raleigh but locality informaiton was for Wake County
condensed_data2 %>%
  filter(city == "Raleigh") #5616

spatial_subset_data <- condensed_data2 %>%
  filter(city == "Raleigh") %>%
  filter(where_obtained_coordinates != "google earth - Wake County" &
           where_obtained_coordinates != "google earth - wake county")

spatial_subset_data

spatial_subset_data %>%
filter(city == "Raleigh") %>%
  count(decimal_latitude, decimal_longitude, city, where_obtained_coordinates) %>%
  # remove dataset provided coordinates
  filter(where_obtained_coordinates != "dataset provided") %>%
  print(n = 50)



nrow(condensed_data2 %>% filter(city == "Raleigh")) # 5616
nrow(spatial_subset_data) #5408
5616 - 5408 # 208

spatial_subset_data %>%
  count(year) %>%
  #arrange(year) # 1900 still the earliest
  arrange(desc(year)) # 2018 still the latest

spatial_subset_data %>%
  count(where_obtained_coordinates)
# A tibble: 14 x 2
#where_obtained_coordinates                                       n
#<chr>                                                        <int>
#1 dataset provided                                              2385
#2 dataset provided|google earth                                    1
#3 dataset provided|google earth - Raleigh                          7
#4 dataset provided|google earth - Wake County                      1
#5 google earth                                                   142
#6 google earth - 440 Gorman St                                     3
#7 google earth - Central Utility Plant, NCSU Centennial Campus     9
#8 google earth - Ebenezer Church Rd                                1
#9 google earth - gardner hall raleigh                              1
#10 google earth - NC fairgrounds raleigh                            1
#11 google earth - NCSU                                              1
#12 google earth - raleigh                                           1
#13 google earth - Raleigh                                        2852
#14 google earth - SE, Raleigh (gives general area)                  3

# collapsing the data down
subset_collapsed <- spatial_subset_data %>%
  select(scientific_name, family, day, month, year, recorded_by, decimal_latitude, decimal_longitude, n, where_obtained_coordinates)
subset_collapsed
nrow(subset_collapsed) #5408

# summary of subset data ####
#number of species
subset_collapsed %>% count(scientific_name) #316

# number of genera
subset_collapsed %>% separate(scientific_name, into = c("genus", "species"), sep = " ") %>% count(genus) # 47

# unique collection events
spatial_subset_data %>%
  count(day, month, year, recorded_by, decimal_latitude, decimal_longitude) #2625

# number of families
spatial_subset_data %>% count(family) #6

# number of species not found in Raleigh
328 - 316 #12



# save subset coordinates ####

raleigh_coordinates <- spatial_subset_data %>%
  count(decimal_latitude, decimal_longitude, city) %>%
  rename(num_specimens = n)
raleigh_coordinates

# make a readme file for this
readme <- tibble(col_names = c("decimal_latitude", "decimal_longitude", "city", "num_specimens"),
                 description = c("latitude where samples were collected in decimals",
                                 "longitude where samples were collected in decimals",
                                 "city where samples were collected as determined by using the reverse_geocode function in the tidygeocoder package",
                                 "total number of specimens (not unique) that  were collected at this location"))
readme
# to save data as excel files
library(openxlsx)
# - commented out to not save over

#https://datavizpyr.com/save-multiple-dataframes-to-a-single-excel-file-in-r/

#Raleigh_coordinate_workbook <- createWorkbook()

#addWorksheet(Raleigh_coordinate_workbook, sheetName="raleigh_coordinates")
#addWorksheet(Raleigh_coordinate_workbook, sheetName="readme")

#writeData(Raleigh_coordinate_workbook, "raleigh_coordinates", raleigh_coordinates)
#writeData(Raleigh_coordinate_workbook, "readme", readme)

#saveWorkbook(Raleigh_coordinate_workbook,
#             file = paste(data_path, "Raleigh_subsample_coordinates.xlsx", sep="/"),
#             overwrite = FALSE)


# set up for RPresence ####
# - plan to set up and use the same primary periods as for the full Wake County datset analyses


## -- need to make a detection history file to use with RPresence ####
spp_by_year_subset <- spatial_subset_data %>%
  count(scientific_name, family, year) %>%
  # need to make a new column that anything with an n over 1 to a 1 as in present
  mutate(n2 = if_else(!is.na(n), "1", "0")) %>%
  # remove n so that won't get multiple species rows when pivot_wider
  select(-n) %>%
  pivot_wider(names_from = year, values_from = n2)
spp_by_year_subset

# saving this file as a base ####
#write.csv(spp_by_year_subset, file = paste(data_path, "2023_Rpresence_Raleigh_subset_bee_detection_histories.csv", sep="/"),
#         row.names = FALSE, na = "")

# loading in that saved data

presence_subset_raw_data <- read_csv(paste(data_path, "2023_Rpresence_Raleigh_subset_bee_detection_histories.csv", sep="/"),
                                     col_types = 
                                       cols(
                                         .default = col_double(),
                                         scientific_name = col_character(),
                                         family = col_character()))
presence_subset_raw_data


# need to fill in all the NAs with zero
presence_subset_raw_data[ is.na( presence_subset_raw_data ) ] <- 0
presence_subset_raw_data

# - to figure out which years are missing data

all_years <- seq(1900,2018,1)

subset_years <- as.vector(unique(spatial_subset_data$year))


all_years[all_years %nin% subset_years]# 1901 1967 1974 1975 1986 1998 1999 2000 2001 2002  2004 2010 2012



# to add in the missing years
presence_subset_raw_data2 <- presence_subset_raw_data %>%
  mutate('1901' = as.double(NA)) %>%
  mutate('1967' = as.double(NA)) %>%
  mutate('1974' = as.double(NA)) %>%
  mutate('1975' = as.double(NA)) %>%
  mutate('1986' = as.double(NA)) %>%
  mutate('1998' = as.double(NA)) %>%
  mutate('1999' = as.double(NA)) %>%
  mutate('2000' = as.double(NA)) %>%
  mutate('2001' = as.double(NA)) %>%
  mutate('2002' = as.double(NA)) %>%
  mutate('2004' = as.double(NA)) %>%
  mutate('2010' = as.double(NA)) %>%
  mutate('2012' = as.double(NA))
presence_subset_raw_data2 # these missing years will remain as NAs so they will not contribute
# to the overall analyses but will allow for easier running with PRESENCE than not adding them in

# to reorder the year columns in chronological order ####
col_order <- c("scientific_name", seq(1900,2018,1))
presence_subset_raw_data2 <- presence_subset_raw_data2[,col_order]

presence_subset_raw_data2


nrow(presence_subset_raw_data2) #316 species


# - to select the columns that relate to the timeperiods

name_list <- names(presence_subset_raw_data2)[2:ncol(presence_subset_raw_data2)]
name_list

focal_periods1 <- c(seq(1909, 1913, 1),
                    seq(1924, 1928, 1),
                    seq(1939, 1943, 1),
                    seq(1954, 1958, 1),
                    seq(1969, 1973, 1),
                    seq(1984, 1988, 1),
                    seq(1999, 2003, 1),
                    seq(2014, 2018, 1))
focal_periods1


# - subset the data for each of the focal periods and run the annalyses

# focal period 1 ####
presence_spatial_subset_data1 <- presence_subset_raw_data2 %>%
  pivot_longer(name_list, names_to = "year") %>%
  filter(year %in% focal_periods1) %>%
  pivot_wider(names_from = year, values_from = value) 
presence_spatial_subset_data1

# saving as .rdata so can use in other scripts
#save(presence_spatial_subset_data1, 
#     file = paste(Rsource_path, "Raleigh_presence_spatial_subset_data1.rda", sep = "/"))


# - pull out detection history
dethist1 <- presence_spatial_subset_data1[,2:ncol(presence_spatial_subset_data1)]

# - create input "pao" object, for use wit occMod function
bees_subset1 <- createPao(data = dethist1,
                          nsurveyseason = rep(5,8), # 5 surveys/season for 8 seasons
                          title = "Bees multispecies multiseason - Raleigh subset")

# - run a model that allows colonization, extinction, and detection to vary by season 
# p(.)gam(season)eps(season)det(season)

#model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset <- occMod(data = bees_subset1, type = "do.1",
#                             model = list(psi~1, p~SEASON, gamma~SEASON, epsilon~SEASON),
#                             outfile='modname')
#warnings produced

# save output for now so do not need to rerun that and wait for it
## -- save the PRESENCE output
#save(model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset, 
#     file = paste(Rsource_path, "Model_1_psi_gamSeason_epsSeason_detSeason_RaleighSubset.rda", sep = "/"))

load(paste(Rsource_path,"Model_1_psi_gamSeason_epsSeason_detSeason_RaleighSubset.rda", sep = "/"))
print(summary(model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset)) # print model summary
#Model name=psi()p(SEASON)gamma(SEASON)epsilon(SEASON)
#AIC=5161.8791
#-2*log-likelihood=5115.8791
#num. par=23
#Warning: Numerical convergence may not have been reached. Parameter esimates converged to approximately 5.09 signifcant digits.
#Warning: Problem with estimation of VC covariance. Ignore all SE's and values in VC matrix.
#NULL

# - warning probably due to primary period 7 being comprised completely of 4 "NA" secondary periods
print(unique(model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset$real$psi))
#              est         se lower_0.95 upper_0.95
#unit1_1 0.1947066 0.06764486 0.09403955  0.3602807

print(unique(model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset$derived$psi)) # print seasonal occupancy estimates
#              est         se lower_0.95 upper_0.95
#est         se lower_0.95 upper_0.95
#unit1_2 0.4955932 0.03336634 0.43064124  0.5606942
#unit1_3 0.3887108 0.03209386 0.32795243  0.4531359
#unit1_4 0.4141236 0.03116794 0.35463570  0.4762267
#unit1_5 0.1395473 0.03442993 0.08462889  0.2214812
#unit1_6 0.1562194 0.03772567 0.09555870  0.2449581
#unit1_7 0.2696469 0.19861074 0.04865921  0.7271483
#unit1_8 0.3264820 0.02863042 0.27302429  0.3848653

print(unique(model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset$real$p))
#                 est           se lower_0.95 upper_0.95
#p1_unit1  0.10401887 0.03832290 0.049300041  0.2062924
#p6_unit1  0.33970278 0.02092897 0.299958540  0.3818407
#p11_unit1 0.34192871 0.02411426 0.296346514  0.3906299
#p16_unit1 0.34845576 0.02216232 0.306367055  0.3930501
#p21_unit1 0.07710275 0.02384544 0.041519955  0.1387653
#p26_unit1 0.09622128 0.02867279 0.052841949  0.1688629
#p31_unit1 0.02347184 0.02373244 0.003148866  0.1546162
#p36_unit1 0.39547009 0.02540261 0.346914247  0.4461790

print(unique(model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset$real$gamma))
#                      est           se  lower_0.95 upper_0.95
#gamma1_unit1 4.048904e-01 5.410749e-02 3.046506e-01  0.5137443
#gamma2_unit1 1.930111e-01 3.981434e-02 1.265778e-01  0.2830136
#gamma3_unit1 2.363955e-01 3.732424e-02 1.711070e-01  0.3170664
#gamma4_unit1 3.648216e-15 7.184858e-10 0.000000e+00  1.0000000
#gamma5_unit1 1.937588e-02 2.217302e-02 2.002439e-03  0.1628826
#gamma6_unit1 1.413795e-01 2.273770e-01 4.172455e-03  0.8661469
#gamma7_unit1 7.781856e-02 2.491145e-01 9.362683e-05  0.9870212

print(unique(model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset$real$epsilon))
#                        est           se   lower_0.95 upper_0.95
#epsilon1_unit1 1.292664e-01 8.387640e-02 0.0333102336  0.3900959
#epsilon2_unit1 4.121092e-01 5.099303e-02 0.3169600309  0.5143140
#epsilon3_unit1 3.063800e-01 5.254189e-02 0.2138815750  0.4176298
#epsilon4_unit1 6.630297e-01 8.009179e-02 0.4935552568  0.7988995
#epsilon5_unit1 1.174844e-21          NaN          NaN        NaN
#epsilon6_unit1 3.754815e-02 7.399675e-02 0.0007046655  0.6833829
#epsilon7_unit1 8.941228e-10 1.900274e-05 0.0000000000  1.0000000


# to get the number of unique records
spatial_subset_data2 <- spatial_subset_data %>%
  # condense down to all unique collection events
  #count(day, month, year, recorded_by, decimal_latitude, decimal_longitude) %>%
  mutate(year = as.numeric(year)) %>%
  # filter down to only the focal period years
  filter(year %in% focal_periods1) %>%
  # label the different focal periods
  mutate(primary_period = ifelse(year >= 1909 & year <= 1913, "1", NA)) %>%
  mutate(primary_period = ifelse(year >= 1924 & year <= 1928, "2", primary_period)) %>%
  mutate(primary_period = ifelse(year >= 1939 & year <= 1943, "3", primary_period)) %>%
  mutate(primary_period = ifelse(year >= 1954 & year <= 1958, "4", primary_period)) %>%
  mutate(primary_period = ifelse(year >= 1969 & year <= 1973, "5", primary_period)) %>%
  mutate(primary_period = ifelse(year >= 1984 & year <= 1988, "6", primary_period)) %>%
  mutate(primary_period = ifelse(year >= 1999 & year <= 2003, "7", primary_period)) %>%
  mutate(primary_period = ifelse(year >= 2014 & year <= 2018, "8", primary_period))

spatial_subset_data2 %>%
  # get the number of unique records from each primary period
  count(primary_period)
# A tibble: 7 x 2
#primary_period     n
#<chr>          <int>
#1 1                 38
#2 2                465
#3 3                376
#4 4                344
#5 5                 30
#6 6                 19
#7 7                  2
#8 8               1048


spatial_subset_data2 %>%
  count(day, month, year, recorded_by, decimal_latitude, decimal_longitude, primary_period) %>%
  # get the number of unique collection events from each primary period
  count(primary_period)
# A tibble: 7 x 2
#primary_period     n
#<chr>          <int>
#1 1                 20
#2 2                236
#3 3                228
#4 4                178
#5 5                 24
#6 6                 19
#7 7                  1
#8 8                310


# get the full wake county information
condensed_data %>%
  mutate(year = as.numeric(year)) %>%
  # filter down to only the focal period years
  filter(year %in% focal_periods1) %>%
  # label the different focal periods
  mutate(primary_period = ifelse(year >= 1909 & year <= 1913, "1", NA)) %>%
  mutate(primary_period = ifelse(year >= 1924 & year <= 1928, "2", primary_period)) %>%
  mutate(primary_period = ifelse(year >= 1939 & year <= 1943, "3", primary_period)) %>%
  mutate(primary_period = ifelse(year >= 1954 & year <= 1958, "4", primary_period)) %>%
  mutate(primary_period = ifelse(year >= 1969 & year <= 1973, "5", primary_period)) %>%
  mutate(primary_period = ifelse(year >= 1984 & year <= 1988, "6", primary_period)) %>%
  mutate(primary_period = ifelse(year >= 1999 & year <= 2003, "7", primary_period)) %>%
  mutate(primary_period = ifelse(year >= 2014 & year <= 2018, "8", primary_period)) %>%
  count(primary_period)
# A tibble: 8 x 2
#primary_period     n
#<chr>          <int>
#1 1                 38
#2 2                485
#3 3                380
#4 4                441
#5 5                 36
#6 6                 35
#7 7                  2
#8 8               1340

condensed_data %>%
  mutate(year = as.numeric(year)) %>%
  count(day, month, year, recorded_by, decimal_latitude, decimal_longitude) %>%
  # filter down to only the focal period years
  filter(year %in% focal_periods1) %>%
  # label the different focal periods
  mutate(primary_period = ifelse(year >= 1909 & year <= 1913, "1", NA)) %>%
  mutate(primary_period = ifelse(year >= 1924 & year <= 1928, "2", primary_period)) %>%
  mutate(primary_period = ifelse(year >= 1939 & year <= 1943, "3", primary_period)) %>%
  mutate(primary_period = ifelse(year >= 1954 & year <= 1958, "4", primary_period)) %>%
  mutate(primary_period = ifelse(year >= 1969 & year <= 1973, "5", primary_period)) %>%
  mutate(primary_period = ifelse(year >= 1984 & year <= 1988, "6", primary_period)) %>%
  mutate(primary_period = ifelse(year >= 1999 & year <= 2003, "7", primary_period)) %>%
  mutate(primary_period = ifelse(year >= 2014 & year <= 2018, "8", primary_period)) %>%
  count(primary_period)
# A tibble: 8 x 2
#primary_period     n
#<chr>          <int>
#1 1                 20
#2 2                241
#3 3                230
#4 4                222
#5 5                 29
#6 6                 33
#7 7                  1
#8 8                441

# RPresence estimates with time ####
# spearman

estimates <- c(unique(model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset$real$psi$est), 
               unique(model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset$derived$psi$est))
spp_rich_estimates <- estimates * 316 # multiple times total number of species in Raleigh
primary_period <- c(1:8)
cor.test(spp_rich_estimates, primary_period, method = "spearman")
#Spearman's rank correlation rho
#
#data:  spp_rich_estimates and primary_period
#S = 108, p-value = 0.5008
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#       rho 
#-0.2857143 


# map of raleigh unique collections by year ####
# only going to have the unique locations in each primary period not by unique specimens
unique_sampling_locations <- spatial_subset_data2 %>%
  count(decimal_latitude, decimal_longitude, primary_period)
unique_sampling_locations

raleigh_primary_period_sampling <- nc_wake_grey1 + 
  geom_point(data = unique_sampling_locations, aes(x = as.numeric(decimal_longitude), y = as.numeric(decimal_latitude), colour = primary_period), 
             inherit.aes = FALSE, alpha = 0.5, size = 3) +
  guides(color=guide_legend(nrow=3, byrow=TRUE))+
  labs(color = "Season") +
  theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
        legend.background=element_blank(), # removes the overall border
        legend.key=element_blank(), #remove the border around each item
        legend.position = "bottom")
raleigh_primary_period_sampling

#ggsave("raleigh_primary_period_sampling.png", width = 5, height = 5.5,
#      units = "in", dpi = 600, plot = raleigh_primary_period_sampling,
#      path = figure_path, bg = "transparent")




# plot Wake and Raleigh trends ####


# load in the RPresence data for all of Wake Co
load(paste(Rsource_path,"Model_1_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))

initial_psi <- unique(model1_psi_gamSeason_epsSeason_detSeason$real$psi)
initial_psi$primary_period <- "1909-1913"

derived_psi <- unique(model1_psi_gamSeason_epsSeason_detSeason$derived$psi)

derived_psi$primary_period <- c("1924-1928",
                                "1939-1943",
                                "1954-1958",
                                "1969-1973",
                                "1984-1988",
                                "1999-2003",
                                "2014-2018")

occupancy_estimates <- initial_psi %>%
  full_join(derived_psi) %>%
  # to get the estimated richness
  mutate(spp_rich = est*328) %>%
  # to get the confidence intervals
  mutate(spp_rich_lower_0.95 = lower_0.95*328) %>%
  mutate(spp_rich_upper_0.95 = upper_0.95*328) %>%
  # to update the se 331species levels
  mutate(se_spp = se*328) %>%
  separate(primary_period, into = c("pp_start", "pp_end"), sep = "-") %>%
  mutate(pp_start = as.numeric(pp_start)) %>%
  mutate(pp_end = as.numeric(pp_end))
occupancy_estimates 
occupancy_estimates$pp_plotting <- as.numeric(c(1911,
                                                1926,
                                                1941,
                                                1956,
                                                1971,
                                                1986,
                                                2001,
                                                2016))

occupancy_estimates

# Raleigh occupancy estimates


initial_psi_Raleigh <- unique(model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset$real$psi)
initial_psi_Raleigh$primary_period <- "1909-1913"

derived_psi_Raleigh <- unique(model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset$derived$psi)

derived_psi_Raleigh$primary_period <- c("1924-1928",
                                        "1939-1943",
                                        "1954-1958",
                                        "1969-1973",
                                        "1984-1988",
                                        "1999-2003",
                                        "2014-2018")

occupancy_estimates_Raleigh <- initial_psi_Raleigh %>%
  full_join(derived_psi_Raleigh) %>%
  # to get the estimated richness
  mutate(spp_rich = est*316) %>%
  # to get the confidence intervals
  mutate(spp_rich_lower_0.95 = lower_0.95*316) %>%
  mutate(spp_rich_upper_0.95 = upper_0.95*316) %>%
  # to update the se 331species levels
  mutate(se_spp = se*316) %>%
  separate(primary_period, into = c("pp_start", "pp_end"), sep = "-") %>%
  mutate(pp_start = as.numeric(pp_start)) %>%
  mutate(pp_end = as.numeric(pp_end))
occupancy_estimates_Raleigh 
occupancy_estimates_Raleigh$pp_plotting <- as.numeric(c(1911,
                                                        1926,
                                                        1941,
                                                        1956,
                                                        1971,
                                                        1986,
                                                        2001,
                                                        2016))

occupancy_estimates_Raleigh


# PRESENCE wake co & raleigh

Presence_Wake_vs_Raleigh <- ggplot(data = occupancy_estimates, aes(x = pp_plotting, y = spp_rich)) +
  geom_point(data = occupancy_estimates, aes(pp_plotting, y = spp_rich), size = 2, color = "red") +
  geom_point(data = occupancy_estimates_Raleigh, aes(x = pp_plotting, y = spp_rich), color = "purple", shape = 17, size = 2) +
  ylab("Species Richness") +
  xlab("Year") +
  scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  scale_y_continuous(expand = c(0,0), breaks=seq(0, 350, 25)) +
  geom_segment(data = occupancy_estimates, aes(x = pp_plotting, xend = pp_plotting, y = spp_rich_lower_0.95,
                                               yend = spp_rich_upper_0.95),
               color = "red") +
  geom_segment(data = occupancy_estimates, aes(x = pp_plotting - 2.5, xend = pp_plotting + 2.5, y = spp_rich,
                                               yend = spp_rich),
               color = "red") +
  geom_segment(data = occupancy_estimates_Raleigh, aes(x = pp_plotting, xend = pp_plotting, y = spp_rich_lower_0.95,
                                                       yend = spp_rich_upper_0.95),
               color = "purple") +
  geom_segment(data = occupancy_estimates_Raleigh, aes(x = pp_plotting - 2.5, xend = pp_plotting + 2.5, y = spp_rich,
                                                       yend = spp_rich),
               color = "purple") +
  theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
        panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
        panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
        panel.border=element_blank(), #gets rid of square going around the entire graph
        axis.line = element_line(colour = 'black', size = 0.5),#sets the axis line size
        axis.ticks=element_line(colour = 'black', size = 0.5), #sets the tick lines
        legend.background=element_blank(), # removes the overall border
        #legend.background=element_rect(fill="white", colour="black"), #puts a black box around the legend
        legend.key=element_blank(), #remove the border around each item
        legend.position = "top",
        #legend.title = element_blank(),
        axis.title.x = element_text(face="bold", size=14, color="black"), #size of x-axis title
        axis.title.y = element_text(face="bold", size=14, color="black"), #size of y-axis title
        axis.text.x = element_text(size=12, color="black"), #size of x-axis text
        axis.text.y = element_text(size=12, color="black"))#size of y-axis text
Presence_Wake_vs_Raleigh

#ggsave("Presence_Wake_vs_Raleigh_spp_richness_trend.png", width = 6, height = 3,
#       units = "in", dpi = 600, plot = Presence_Wake_vs_Raleigh,
#       path = figure_path, family = "Arial") 


# -- RPresence Wake vs. Raleigh
cor.test(occupancy_estimates$spp_rich,
         occupancy_estimates_Raleigh$spp_rich)
#Pearson's product-moment correlation
#data:  occupancy_estimates$spp_rich and occupancy_estimates_Raleigh$spp_rich
#t = 18.124, df = 6, p-value = 1.816e-06
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.9490903 0.9984333
#sample estimates:
#  cor 
#0.9909905 

cor.test(occupancy_estimates$spp_rich,
         occupancy_estimates_Raleigh$spp_rich, method = "spearman")
#Spearman's rank correlation rho
#data:  occupancy_estimates$spp_rich and occupancy_estimates_Raleigh$spp_rich
#S = 0, p-value = 4.96e-05
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#rho 
#  1

# Raleigh estimaes w time 
cor.test(occupancy_estimates_Raleigh$pp_plotting,
         occupancy_estimates_Raleigh$spp_rich, method = "spearman")
#Spearman's rank correlation rho
#data:  occupancy_estimates_Raleigh$pp_plotting and occupancy_estimates_Raleigh$spp_rich
#S = 108, p-value = 0.5008
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#       rho 
#-0.2857143 


# Rarefaction for the smaller subset ####

iNext_raw_data_subset <- presence_subset_raw_data2 %>%
  pivot_longer(name_list, names_to = "year") %>%
  filter(year %in% focal_periods1)
iNext_raw_data_subset


# need to subset the data into the different time periods before
# can pivot_wider into the matrix again

period1 <- seq(1909, 1913, 1)
period2 <- seq(1924, 1928, 1)
period3 <- seq(1939, 1943, 1)
period4 <- seq(1954, 1958, 1)
period5 <- seq(1969, 1973, 1)
period6 <- seq(1984, 1988, 1)
period7 <- seq(1999, 2003, 1)
period8 <- seq(2014, 2018, 1)

iNEXT_period1_subset <- iNext_raw_data_subset %>%
  filter(year %in% period1) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  select(-scientific_name)
iNEXT_period1_subset #5 samples

iNEXT_period2_subset <- iNext_raw_data_subset %>%
  filter(year %in% period2) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  select(-scientific_name)
iNEXT_period2_subset #5 samples

iNEXT_period3_subset <- iNext_raw_data_subset %>%
  filter(year %in% period3) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  select(-scientific_name)
iNEXT_period3_subset #5 samples

iNEXT_period4_subset <- iNext_raw_data_subset %>%
  filter(year %in% period4) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  select(-scientific_name)
iNEXT_period4_subset #5 samples

iNEXT_period5_subset <- iNext_raw_data_subset %>%
  filter(year %in% period5) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  select(-scientific_name)
iNEXT_period5_subset #5 samples

iNEXT_period6_subset <- iNext_raw_data_subset %>%
  filter(year %in% period6) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  select(-scientific_name)
iNEXT_period6_subset #4 samples
# remove the NA column
iNEXT_period6_subset2 <- iNEXT_period6_subset %>%
  select(-'1986')
iNEXT_period6_subset2

iNEXT_period7_subset <- iNext_raw_data_subset %>%
  filter(year %in% period7) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  select(-scientific_name)
iNEXT_period7_subset #1 sample

iNEXT_period8_subset <- iNext_raw_data_subset %>%
  filter(year %in% period8) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  select(-scientific_name)
iNEXT_period8_subset #5 samples

iNEXT_indicence_raw_data_subset <- list(season1 = as.matrix(iNEXT_period1_subset),
                                        season2 = as.matrix(iNEXT_period2_subset),
                                        season3 = as.matrix(iNEXT_period3_subset),
                                        season4 = as.matrix(iNEXT_period4_subset),
                                        season5 = as.matrix(iNEXT_period5_subset),
                                        season6 = as.matrix(iNEXT_period6_subset2), 
                                        #period7 = as.matrix(iNEXT_period7_subset), # doesn't have a sample so should be removed
                                        season8 = as.matrix(iNEXT_period8_subset)) 

iNEXT_indicence_raw_data_subset


# run iNEXT ####

#inext_rpresence_setup_subset <- iNEXT(iNEXT_indicence_raw_data_subset, q=c(0), datatype = "incidence_raw")
#inext_rpresence_setup_subset

## -- save the output
#save(inext_rpresence_setup_subset, 
#     file = paste(Rsource_path, "iNEXT_output_setup_like_PRESENCE_Raleigh_subset.rda", sep = "/"))

load(paste(Rsource_path,"iNEXT_output_setup_like_PRESENCE_Raleigh_subset.rda", sep = "/"))

inext_rpresence_setup_subset

ggiNEXT(inext_rpresence_setup_subset, type=1, se=TRUE, grey=FALSE)
ggiNEXT(inext_rpresence_setup_subset, type=2, se=TRUE, grey=FALSE)
ggiNEXT(inext_rpresence_setup_subset, type=3, se=TRUE, grey=FALSE)


# following Box 1 in Chao et al. 2014 ####
inext_rpresence_setup_subset$DataInfo
#     site T   U S.obs     SC Q1 Q2 Q3 Q4 Q5 Q6 Q7 Q8 Q9 Q10
#1 season1 5  32    26 0.4008 21  4  1  0  0  0  0  0  0   0
#2 season2 5 266   138 0.7721 72 27 22 11  6  0  0  0  0   0
#3 season3 5 210   107 0.8303 47 30 18 11  1  0  0  0  0   0
#4 season4 5 228   117 0.8188 53 30 23  9  2  0  0  0  0   0
#5 season5 5  17    13 0.5668  9  4  0  0  0  0  0  0  0   0
#6 season6 4  19    15 0.5340 11  4  0  0  0  0  0  0  0   0
#7 season8 5 204    95 0.8953 33 36 12  7  7  0  0  0  0   0

# so the lowest SC (sample coverage) is period 1 at 40%,
# and then period 6 at 53%

# when extrapolating for q = 0 (i.e. species richness) Chao et al. 2014
# recommends not extrapolating past 2 times the sample size (i.e. r = 2)
# --- so going to follow Box 1 in Chao et al.
# a) sample-sized-based rarefaction/extropolation
#     Step 1. Compute the maximum reference size, na = max {n1,n2,...,nk}
#     For us this is na = max{5,5,5,5,5,4,5} = 5
#     Step 2. Compute the minimum r times reference sample sizes, nb = min{rn1,rn2, ..., rnk}
#     For us this is r = 2, nb = min{10,10,10,10,10,8,10} = 8
#     Step 3. The suggested base sample size is the maximum of na and nb, nbase = max{na,nb}
#     For us this is nbase = max{5,8} = 8
# b) Coverage-based rarefaction/extrapolation
#     Step 1. Compute the maximum coverage of reference sample sizes, Ca = max{C(n1),C(n2)...,C(nk)}
#     For us this is Ca = max{0.4008, 0.7721, 0.8303, 0.8188, 0.5668, 0.5340, 0.8953} = 0.8953
#     Step 2. Compute the minimum coverage of r times reference sample sizes, Cb = min{C(n1),C(n2)...,C(nk)}
#     For us this is Cb = min{0.544, 0.864, 0.926, 0.914, 0.763, 0.804, 0.972} = 0.544
#     Step 3. The suggested base coverage is the maximum of Ca and Cb, Cbase = max{Ca,Cb}
#     For us this is Cbase = max{0.8953, 0.544} = 0.8953

# to get the r times the reference sample sizes
sample_size_8_coverage_subset <- estimateD(iNEXT_indicence_raw_data_subset, datatype = "incidence_raw", base = "size",
                                           level = 8, conf = 0.95)
sample_size_8_coverage_subset %>%
  filter(order == "0")
#     site t       method order    SC      qD  qD.LCL  qD.UCL
#1 season1 8 extrapolated     0 0.544  36.533  25.602  47.464
#2 season2 8 extrapolated     0 0.864 168.937 152.633 185.242
#3 season3 8 extrapolated     0 0.926 123.623 110.305 136.941
#4 season4 8 extrapolated     0 0.914 136.720 123.261 150.179
#5 season5 8 extrapolated     0 0.763  16.664  10.825  22.502
#6 season6 8 extrapolated     0 0.804  21.583  13.033  30.133
#7 season8 8 extrapolated     0 0.972 103.822  94.423 113.221

# generate and save the recommended sample coverage output ####
#Raleigh_subset_coverage_base_0.8953 <- estimateD(iNEXT_indicence_raw_data_subset, datatype = "incidence_raw", base = "coverage",
#                                                level = 0.8953, conf = 0.95)
# gives some warnings because of over extrapolation

# save output for now so do not need to rerun that and wait for it
## -- save the estimateD output
#save(Raleigh_subset_coverage_base_0.8953, 
#     file = paste(Rsource_path, "Raleigh_subset_sample_coverage_0.8953_iNEXT_output_setup_like_PRESENCE.rda", sep = "/"))

# plot data ####
load(paste(Rsource_path,"Raleigh_subset_sample_coverage_0.8953_iNEXT_output_setup_like_PRESENCE.rda", sep = "/"))

Raleigh_subset_coverage_base_0.8953 %>%
  filter(order == "0")
#     site  t       method order    SC      qD  qD.LCL  qD.UCL
#1 season1 24 extrapolated     0 0.894  62.270  30.485  94.055
#2 season2 10 extrapolated     0 0.903 182.277 161.312 203.242
#3 season3  7 extrapolated     0 0.903 119.528 108.482 130.573
#4 season4  7 extrapolated     0 0.890 131.701 120.155 143.247
#5 season5 12 extrapolated     0 0.894  19.112  10.682  27.542
#6 season6 11 extrapolated     0 0.898  23.861  12.550  35.173
#7 season8  3 interpolated     0 0.750  78.200  72.545  83.855

# for easier plotting need to generate values out to the max t which is 24

#Raleigh_subset_max_t <- iNEXT(iNEXT_indicence_raw_data_subset, q=c(0), datatype = "incidence_raw",
#               endpoint = 24)

# save output for now so do not need to rerun that and wait for it
## -- save the output
#save(Raleigh_subset_max_t, 
#     file = paste(Rsource_path, "Raleigh_subset_sample_coverage_iNEXT_output_setup_like_PRESENCE_extended.rda", sep = "/"))

load(paste(Rsource_path,"Raleigh_subset_sample_coverage_iNEXT_output_setup_like_PRESENCE_extended.rda", sep = "/"))

PRESENCE_rarefaction_sample_coverage_by_diversity_0.8953_Raleigh_subset <- ggiNEXT(Raleigh_subset_max_t, type=3, se=TRUE, grey=FALSE)+
  #facet_wrap(.~site, scales = "free_y" )+
  geom_vline(xintercept = 0.8953, linetype="dashed", 
             color = "black") +
  geom_vline(xintercept = 0.544, linetype="dotted", 
             color = "black") +
  ylab("Species Richness (q = 0)") +
  annotate("text", x = 0.80, y = 240, label = "0.8953") +
  annotate("text", x = 0.40, y = 240, label = "0.544") +
  theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
        panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
        panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
        panel.border=element_rect(colour="black", fill=NA),
        #panel.border=element_blank(), #gets rid of square going around the entire graph
        #axis.line = element_line(colour = 'black', size = 0.5),#sets the axis line size
        axis.ticks=element_line(colour = 'black', size = 0.5), #sets the tick lines
        #legend.position=c(0.10,0.25), # moves the location of the legend
        legend.position="top",
        legend.text=element_text(size=10),
        #legend.title=element_blank(),
        #legend.text=element_text(face="bold"),
        #legend.background=element_blank(), # removes the overall border
        #legend.background=element_rect(fill="white", colour="black"), #puts a black box around the legend
        legend.key=element_blank(), #remove the border around each item
        axis.title.x = element_text(face="bold", size=14, color="black"), #size of x-axis title
        axis.title.y = element_text(face="bold", size=14, color="black"), #size of y-axis title
        axis.text.x = element_text(size=12, color="black"), #size of x-axis text
        axis.text.y = element_text(size=12, color="black"))#size of y-axis text
PRESENCE_rarefaction_sample_coverage_by_diversity_0.8953_Raleigh_subset


# save figure ####
#ggsave("PRESENCE_rarefaction_sample_coverage_by_diversity_0.8953_Raleigh_subset.png", width = 6.13, height = 4,
#       units = "in", dpi = 600, plot = PRESENCE_rarefaction_sample_coverage_by_diversity_0.8953_Raleigh_subset,
#       path = figure_path)

# run some correlations between time and inext ####

ggplot(Raleigh_subset_coverage_base_0.8953, aes(x = site, y = qD, color = order, shape = method))+
  geom_point(stat = "identity")
Raleigh_subset_coverage_base_0.8953


# - correlation of the 0.8953 estimates with time period
Raleigh_subset_coverage_base_0.8953_2 <- Raleigh_subset_coverage_base_0.8953 %>%
  mutate(period = str_extract(site, regex("
                             \\d # a digit
                             ", comments = TRUE))) %>%
  mutate(period = as.numeric(period)) %>%
  filter(order == "0")

y_rarefaction_PRESENCE <- Raleigh_subset_coverage_base_0.8953_2$qD
x_period <- Raleigh_subset_coverage_base_0.8953_2$period

# spearman
cor.test(y_rarefaction_PRESENCE, x_period, method = "spearman")
#Spearman's rank correlation rho
#
#data:  y_rarefaction_PRESENCE and x_period
#S = 78, p-value = 0.3956
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#       rho 
#-0.3928571

