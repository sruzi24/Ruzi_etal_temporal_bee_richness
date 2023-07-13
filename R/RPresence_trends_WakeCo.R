# file to use to generate, choose, and obtain RPresence output data

# load libraries ####
library(here)
library(tidyverse)
library(naniar)
library(RPresence)

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

## -- need to make a detection history file to use with RPresence ####
spp_by_year <- condensed_data %>%
  count(scientific_name, family, year) %>%
  # need to make a new column that anything with an n over 1 to a 1 as in present
  mutate(n2 = if_else(!is.na(n), "1", "0")) %>%
  # remove n so that won't get multiple species rows when pivot_wider
  select(-n) %>%
  pivot_wider(names_from = year, values_from = n2)
spp_by_year

# saving this file as a base ####
#write.csv(spp_by_year, file = paste(data_path, "2023_Rpresence_bee_detection_histories.csv", sep="/"),
#         row.names = FALSE, na = "")

# loading in that saved data

presence_raw_data <- read_csv(paste(data_path, "2023_Rpresence_bee_detection_histories.csv", sep="/"),
                              col_types = 
                                cols(
                                  .default = col_double(),
                                  scientific_name = col_character(),
                                  family = col_character()))
presence_raw_data


# need to fill in all the NAs with zero
presence_raw_data[ is.na( presence_raw_data ) ] <- 0
presence_raw_data

# find out which years do not have any specimesn collected
year_tb <- condensed_data %>%
  count(year) %>%
  arrange(year) %>% print(n = 120)
years_list <- year_tb$year

temp_tb <- tibble(year2 = seq(1900,2018,1), x = seq(1,119,1))
temp_tb %>%
  filter(year2 %nin% years_list) %>%
  select(year2)
#year2    
#<dbl> 
#1  1901
#2  1967
#3  1986
#4  1998
#5  1999  
#6  2000  
#7  2001  
#8  2002   
#9  2004   
#10  2010 

# to add in the missing years
presence_raw_data2 <- presence_raw_data %>%
  mutate('1901' = as.double(NA)) %>%
  mutate('1967' = as.double(NA)) %>%
  mutate('1986' = as.double(NA)) %>%
  mutate('1998' = as.double(NA)) %>%
  mutate('1999' = as.double(NA)) %>%
  mutate('2000' = as.double(NA)) %>%
  mutate('2001' = as.double(NA)) %>%
  mutate('2002' = as.double(NA)) %>%
  mutate('2004' = as.double(NA)) %>%
  mutate('2010' = as.double(NA))
presence_raw_data2 # these missing years will remain as NAs so they will not contribute
# to the overall analyses but will allow for easier running with PRESENCE than not adding them in

# to reorder the year columns in chronological order ####
col_order <- c("scientific_name", seq(1900,2018,1))
presence_raw_data2 <- presence_raw_data2[,col_order]

presence_raw_data2

nrow(presence_raw_data2) #328 species

# to determine the focal time periods ####
# - working backwards from most recent time periods that feel are okay sampling wise

# - to select the columns that relate to the timeperiods

name_list <- names(presence_raw_data2)[2:ncol(presence_raw_data2)]
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

focal_periods2 <- focal_periods1 - 1
focal_periods3 <- focal_periods1 - 2
focal_periods4 <- focal_periods1 - 3
focal_periods5 <- focal_periods1 - 4
focal_periods6 <- focal_periods1 - 5
focal_periods7 <- focal_periods1 - 6
focal_periods8 <- focal_periods1 - 7
focal_periods9 <- focal_periods1 - 8
focal_periods10 <- focal_periods1 - 9

# - subset the data for each of the focal periods and run the annalyses

# focal period 1 ####
presence_subset_data1 <- presence_raw_data2 %>%
  pivot_longer(name_list, names_to = "year") %>%
  filter(year %in% focal_periods1) %>%
  pivot_wider(names_from = year, values_from = value) 
presence_subset_data1

# saving as Rdata for now
#save(presence_subset_data1, 
#     file = paste(Rsource_path, "presence_subset_data1.rda", sep = "/"))

# - pull out detection history
dethist1 <- presence_subset_data1[,2:ncol(presence_subset_data1)]

# - create input "pao" object, for use wit occMod function
bees1 <- createPao(data = dethist1,
                   nsurveyseason = rep(5,8), # 5 surveys/season for 8 seasons
                   title = "Bees multispecies multiseason")

# - run a model that allows colonization, extinction, and detection to vary by season 
# p(.)gam(season)eps(season)det(season)

#model1_psi_gamSeason_epsSeason_detSeason <- occMod(data = bees1, type = "do.1",
#                             model = list(psi~1, p~SEASON, gamma~SEASON, epsilon~SEASON),
#                             outfile='modname')

# save output for now so do not need to rerun that and wait for it
## -- save the PRESENCE output
#save(model1_psi_gamSeason_epsSeason_detSeason, 
#     file = paste(Rsource_path, "Model_1_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))

load(paste(Rsource_path,"Model_1_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))
print(summary(model1_psi_gamSeason_epsSeason_detSeason)) # print model summary
# parameter estimates converged to approximately 5.45 significant digits

# focal period 2 ####
presence_subset_data2 <- presence_raw_data2 %>%
  pivot_longer(name_list, names_to = "year") %>%
  filter(year %in% focal_periods2) %>%
  pivot_wider(names_from = year, values_from = value) 
presence_subset_data2

# - pull out detection history
dethist2 <- presence_subset_data2[,2:ncol(presence_subset_data2)]

# - create input "pao" object, for use wit occMod function
bees2 <- createPao(data = dethist2,
                   nsurveyseason = rep(5,8), # 5 surveys/season for 8 seasons
                   title = "Bees multispecies multiseason")

# - run a model that allows colonization, extinction, and detection to vary by season 
# p(.)gam(season)eps(season)det(season)

#model2_psi_gamSeason_epsSeason_detSeason <- occMod(data = bees2, type = "do.1",
#                             model = list(psi~1, p~SEASON, gamma~SEASON, epsilon~SEASON),
#                             outfile='modname')

# save output for now so do not need to rerun that and wait for it
## -- save the PRESENCE output
#save(model2_psi_gamSeason_epsSeason_detSeason, 
#     file = paste(Rsource_path, "Model_2_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))

load(paste(Rsource_path,"Model_2_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))
print(summary(model2_psi_gamSeason_epsSeason_detSeason))
# warnings produced in addition to numerical convergence

# focal period 3 ####
presence_subset_data3 <- presence_raw_data2 %>%
  pivot_longer(name_list, names_to = "year") %>%
  filter(year %in% focal_periods3) %>%
  pivot_wider(names_from = year, values_from = value) 
presence_subset_data3

# - pull out detection history
dethist3 <- presence_subset_data3[,2:ncol(presence_subset_data3)]

# - create input "pao" object, for use wit occMod function
bees3 <- createPao(data = dethist3,
                   nsurveyseason = rep(5,8), # 5 surveys/season for 8 seasons
                   title = "Bees multispecies multiseason")

# - run a model that allows colonization, extinction, and detection to vary by season 
# p(.)gam(season)eps(season)det(season)

#model3_psi_gamSeason_epsSeason_detSeason <- occMod(data = bees3, type = "do.1",
#                             model = list(psi~1, p~SEASON, gamma~SEASON, epsilon~SEASON),
#                             outfile='modname')

# save output for now so do not need to rerun that and wait for it
## -- save the PRESENCE output
#save(model3_psi_gamSeason_epsSeason_detSeason, 
#     file = paste(Rsource_path, "Model_3_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))

load(paste(Rsource_path,"Model_3_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))
print(summary(model3_psi_gamSeason_epsSeason_detSeason))
# parameter estimates converged to approximately 4.42 sig digits

# focal period 4 ####
presence_subset_data4 <- presence_raw_data2 %>%
  pivot_longer(name_list, names_to = "year") %>%
  filter(year %in% focal_periods4) %>%
  pivot_wider(names_from = year, values_from = value) 
presence_subset_data4

# - pull out detection history
dethist4 <- presence_subset_data4[,2:ncol(presence_subset_data4)]

# - create input "pao" object, for use wit occMod function
bees4 <- createPao(data = dethist4,
                   nsurveyseason = rep(5,8), # 5 surveys/season for 8 seasons
                   title = "Bees multispecies multiseason")

# - run a model that allows colonization, extinction, and detection to vary by season 
# p(.)gam(season)eps(season)det(season)

#model4_psi_gamSeason_epsSeason_detSeason <- occMod(data = bees4, type = "do.1",
#                             model = list(psi~1, p~SEASON, gamma~SEASON, epsilon~SEASON),
#                             outfile='modname')

# save output for now so do not need to rerun that and wait for it
## -- save the PRESENCE output
#save(model4_psi_gamSeason_epsSeason_detSeason, 
#     file = paste(Rsource_path, "Model_4_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))

load(paste(Rsource_path,"Model_4_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))
print(summary(model4_psi_gamSeason_epsSeason_detSeason))
# warnings produced
# parameter estimates converged to approximately4.95

# focal period 5 ####
presence_subset_data5 <- presence_raw_data2 %>%
  pivot_longer(name_list, names_to = "year") %>%
  filter(year %in% focal_periods5) %>%
  pivot_wider(names_from = year, values_from = value) 
presence_subset_data5

# - pull out detection history
dethist5 <- presence_subset_data5[,2:ncol(presence_subset_data5)]

# - create input "pao" object, for use wit occMod function
bees5 <- createPao(data = dethist5,
                   nsurveyseason = rep(5,8), # 5 surveys/season for 8 seasons
                   title = "Bees multispecies multiseason")

# - run a model that allows colonization, extinction, and detection to vary by season 
# p(.)gam(season)eps(season)det(season)

#model5_psi_gamSeason_epsSeason_detSeason <- occMod(data = bees5, type = "do.1",
#                             model = list(psi~1, p~SEASON, gamma~SEASON, epsilon~SEASON),
#                             outfile='modname')

# save output for now so do not need to rerun that and wait for it
## -- save the PRESENCE output
#save(model5_psi_gamSeason_epsSeason_detSeason, 
#     file = paste(Rsource_path, "Model_5_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))

load(paste(Rsource_path,"Model_5_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))
print(summary(model5_psi_gamSeason_epsSeason_detSeason))
# parameter estimates converged to approximately 0.92 significant digits

# focal period 6 ####
presence_subset_data6 <- presence_raw_data2 %>%
  pivot_longer(name_list, names_to = "year") %>%
  filter(year %in% focal_periods6) %>%
  pivot_wider(names_from = year, values_from = value) 
presence_subset_data6

# - pull out detection history
dethist6 <- presence_subset_data6[,2:ncol(presence_subset_data6)]

# - create input "pao" object, for use wit occMod function
bees6 <- createPao(data = dethist6,
                   nsurveyseason = rep(5,8), # 5 surveys/season for 8 seasons
                   title = "Bees multispecies multiseason")

# - run a model that allows colonization, extinction, and detection to vary by season 
# p(.)gam(season)eps(season)det(season)

#model6_psi_gamSeason_epsSeason_detSeason <- occMod(data = bees6, type = "do.1",
#                             model = list(psi~1, p~SEASON, gamma~SEASON, epsilon~SEASON),
#                             outfile='modname')

# save output for now so do not need to rerun that and wait for it
## -- save the PRESENCE output
#save(model6_psi_gamSeason_epsSeason_detSeason, 
#     file = paste(Rsource_path, "Model_6_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))

load(paste(Rsource_path,"Model_6_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))
print(summary(model6_psi_gamSeason_epsSeason_detSeason))
# parameter estimates converged to approximately 5.32 significant digits


# focal period 7 ####
presence_subset_data7 <- presence_raw_data2 %>%
  pivot_longer(name_list, names_to = "year") %>%
  filter(year %in% focal_periods7) %>%
  pivot_wider(names_from = year, values_from = value) 
presence_subset_data7

# - pull out detection history
dethist7 <- presence_subset_data7[,2:ncol(presence_subset_data7)]

# - create input "pao" object, for use wit occMod function
bees7 <- createPao(data = dethist7,
                   nsurveyseason = rep(5,8), # 5 surveys/season for 8 seasons
                   title = "Bees multispecies multiseason")

# - run a model that allows colonization, extinction, and detection to vary by season 
# p(.)gam(season)eps(season)det(season)

#model7_psi_gamSeason_epsSeason_detSeason <- occMod(data = bees7, type = "do.1",
#                             model = list(psi~1, p~SEASON, gamma~SEASON, epsilon~SEASON),
#                             outfile='modname')

# save output for now so do not need to rerun that and wait for it
## -- save the PRESENCE output
#save(model7_psi_gamSeason_epsSeason_detSeason, 
#     file = paste(Rsource_path, "Model_7_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))

load(paste(Rsource_path,"Model_7_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))
print(summary(model7_psi_gamSeason_epsSeason_detSeason))
# parameter estimates converged to approximately -2.11 significant digits 

# focal period 8 ####
presence_subset_data8 <- presence_raw_data2 %>%
  pivot_longer(name_list, names_to = "year") %>%
  filter(year %in% focal_periods8) %>%
  pivot_wider(names_from = year, values_from = value) 
presence_subset_data8

# - pull out detection history
dethist8 <- presence_subset_data8[,2:ncol(presence_subset_data8)]

# - create input "pao" object, for use wit occMod function
bees8 <- createPao(data = dethist8,
                   nsurveyseason = rep(5,8), # 5 surveys/season for 8 seasons
                   title = "Bees multispecies multiseason")

# - run a model that allows colonization, extinction, and detection to vary by season 
# p(.)gam(season)eps(season)det(season)

#model8_psi_gamSeason_epsSeason_detSeason <- occMod(data = bees8, type = "do.1",
#                             model = list(psi~1, p~SEASON, gamma~SEASON, epsilon~SEASON),
#                             outfile='modname')

# save output for now so do not need to rerun that and wait for it
## -- save the PRESENCE output
#save(model8_psi_gamSeason_epsSeason_detSeason, 
#     file = paste(Rsource_path, "Model_8_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))

load(paste(Rsource_path,"Model_8_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))
print(summary(model8_psi_gamSeason_epsSeason_detSeason))
# parameter estimates converged to approx. 2.4

# focal period 9 ####
presence_subset_data9 <- presence_raw_data2 %>%
  pivot_longer(name_list, names_to = "year") %>%
  filter(year %in% focal_periods9) %>%
  pivot_wider(names_from = year, values_from = value) 
presence_subset_data9

# - pull out detection history
dethist9 <- presence_subset_data9[,2:ncol(presence_subset_data9)]

# - create input "pao" object, for use wit occMod function
bees9 <- createPao(data = dethist9,
                   nsurveyseason = rep(5,8), # 5 surveys/season for 8 seasons
                   title = "Bees multispecies multiseason")

# - run a model that allows colonization, extinction, and detection to vary by season 
# p(.)gam(season)eps(season)det(season)

#model9_psi_gamSeason_epsSeason_detSeason <- occMod(data = bees9, type = "do.1",
#                             model = list(psi~1, p~SEASON, gamma~SEASON, epsilon~SEASON),
#                             outfile='modname')

# save output for now so do not need to rerun that and wait for it
## -- save the PRESENCE output
#save(model9_psi_gamSeason_epsSeason_detSeason, 
#     file = paste(Rsource_path, "Model_9_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))

load(paste(Rsource_path,"Model_9_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))
print(summary(model9_psi_gamSeason_epsSeason_detSeason))
# parameter estimates converged to approximately 4.88 significant digits


# focal period 10 ####
presence_subset_data10 <- presence_raw_data2 %>%
  pivot_longer(name_list, names_to = "year") %>%
  filter(year %in% focal_periods10) %>%
  pivot_wider(names_from = year, values_from = value) 
presence_subset_data10

# - pull out detection history
dethist10 <- presence_subset_data10[,2:ncol(presence_subset_data10)]

# - create input "pao" object, for use wit occMod function
bees10 <- createPao(data = dethist10,
                    nsurveyseason = rep(5,8), # 5 surveys/season for 8 seasons
                    title = "Bees multispecies multiseason")

# - run a model that allows colonization, extinction, and detection to vary by season 
# p(.)gam(season)eps(season)det(season)

#model10_psi_gamSeason_epsSeason_detSeason <- occMod(data = bees10, type = "do.1",
#                             model = list(psi~1, p~SEASON, gamma~SEASON, epsilon~SEASON),
#                             outfile='modname')

# save output for now so do not need to rerun that and wait for it
## -- save the PRESENCE output
#save(model10_psi_gamSeason_epsSeason_detSeason, 
#     file = paste(Rsource_path, "Model_10_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))

load(paste(Rsource_path,"Model_10_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))
print(summary(model10_psi_gamSeason_epsSeason_detSeason))
# parameter estimates converged to approximately 5.37 significant digits


# get the occupancy estimates for the most recent period for appendix ####
# - will also help choose which focal periods

print(unique(model1_psi_gamSeason_epsSeason_detSeason$derived$psi)) # print seasonal occupancy estimates
print(unique(model2_psi_gamSeason_epsSeason_detSeason$derived$psi)) # print seasonal occupancy estimates
print(unique(model3_psi_gamSeason_epsSeason_detSeason$derived$psi)) # print seasonal occupancy estimates
print(unique(model4_psi_gamSeason_epsSeason_detSeason$derived$psi)) # print seasonal occupancy estimates
print(unique(model5_psi_gamSeason_epsSeason_detSeason$derived$psi)) # print seasonal occupancy estimates
print(unique(model6_psi_gamSeason_epsSeason_detSeason$derived$psi)) # print seasonal occupancy estimates
print(unique(model7_psi_gamSeason_epsSeason_detSeason$derived$psi)) # print seasonal occupancy estimates
print(unique(model8_psi_gamSeason_epsSeason_detSeason$derived$psi)) # print seasonal occupancy estimates
print(unique(model9_psi_gamSeason_epsSeason_detSeason$derived$psi)) # print seasonal occupancy estimates
print(unique(model10_psi_gamSeason_epsSeason_detSeason$derived$psi)) # print seasonal occupancy estimates

# choosing model 1 to highlight in the main text ####

# run model 1 with 100 randim initial starting value vectors to see if get similar results
# default is to run with 0
#- original run 
print(summary(model1_psi_gamSeason_epsSeason_detSeason))


#Mod1_w_100_random_init <- occMod(data = bees1, type = "do.1", randinit = 100,
#                             model = list(psi~1, p~SEASON, gamma~SEASON, epsilon~SEASON),
#                             outfile='modname')

## -- save the PRESENCE output
#save(Mod1_w_100_random_init, 
#     file = paste(Rsource_path, "Mod1_w_100_random_init.rda", sep = "/"))

load(paste(Rsource_path,"Mod1_w_100_random_init.rda", sep = "/"))
summary(Mod1_w_100_random_init)
#summaries are roughly the same, the fall to different number of sig digits (4.78)

# checking values between the two models, going to report the not random initial starts in main text
# - 1st focal period occupancy estimates -- values round to the same
print(unique(model1_psi_gamSeason_epsSeason_detSeason$real$psi))
#              est         se lower_0.95 upper_0.95
#unit1_1 0.1875837 0.06515336 0.09081495  0.3479979

print(unique(Mod1_w_100_random_init$real$psi))
#              est         se lower_0.95 upper_0.95
#unit1_1 0.1875832 0.06515003 0.09081811  0.3479879

# - rest of the focal periods occupancy estimates - values are extremely similar
print(unique(model1_psi_gamSeason_epsSeason_detSeason$derived$psi)) # print seasonal occupancy estimates
#              est         se lower_0.95 upper_0.95
#unit1_2 0.4788921 0.03236994 0.41612269  0.5423351
#unit1_3 0.3747026 0.03121236 0.31578915  0.4375779
#unit1_4 0.4356033 0.02997542 0.37801261  0.4949856
#unit1_5 0.1176689 0.03404165 0.06553874  0.2022879
#unit1_6 0.1553283 0.03825463 0.09407389  0.2456522
#unit1_7 0.2382727 0.17899094 0.04331168  0.6836743
#unit1_8 0.3440129 0.02787834 0.29160747  0.4005116

print(unique(Mod1_w_100_random_init$derived$psi)) # print seasonal occupancy estimates
#              est         se lower_0.95 upper_0.95
#unit1_2 0.4787778 0.03235687 0.41603523  0.5421972
#unit1_3 0.3795323 0.03146258 0.32008156  0.4428334
#unit1_4 0.4356339 0.02998014 0.37803394  0.4950251
#unit1_5 0.1174977 0.03396396 0.06547789  0.2019158
#unit1_6 0.1551834 0.03818649 0.09402946  0.2453395
#unit1_7 0.2380661 0.17882052 0.04329305  0.6832790
#unit1_8 0.3440130 0.02787875 0.29160678  0.4005125

# - detectability across the different focal periods  - values are extremely similar
print(unique(model1_psi_gamSeason_epsSeason_detSeason$real$p))
#                 est         se lower_0.95 upper_0.95
#p1_unit1  0.10401859 0.03828233 0.049339814  0.2061526
#p6_unit1  0.34378139 0.02086242 0.304123224  0.3857444
#p11_unit1 0.34173433 0.02412970 0.296126371  0.3904699
#p16_unit1 0.39334289 0.02134320 0.352381702  0.4358610
#p21_unit1 0.09327543 0.03110560 0.047646345  0.1745909
#p26_unit1 0.12758171 0.03554784 0.072529050  0.2147463
#p31_unit1 0.02559069 0.02612086 0.003359116  0.1698778
#p36_unit1 0.42716754 0.02390137 0.381104589  0.4745292

print(unique(Mod1_w_100_random_init$real$p))
#                 est         se lower_0.95 upper_0.95
#p1_unit1  0.10401887 0.03827993 0.049342417  0.2061445
#p6_unit1  0.34386329 0.02085747 0.304213513  0.3858153
#p11_unit1 0.33899232 0.02401804 0.293619325  0.3875302
#p16_unit1 0.39331521 0.02134534 0.352350230  0.4358378
#p21_unit1 0.09341126 0.03112538 0.047741154  0.1747528
#p26_unit1 0.12770096 0.03555196 0.072629860  0.2148549
#p31_unit1 0.02561289 0.02614203 0.003362332  0.1699935
#p36_unit1 0.42716754 0.02390137 0.381104589  0.4745292

# - local colonization across the different focal periods  - values are extremely similar
print(unique(model1_psi_gamSeason_epsSeason_detSeason$real$gamma))
#                      est       se  lower_0.95 upper_0.95
#gamma1_unit1 0.389087853 0.05246141 2.924068e-01  0.4953569
#gamma2_unit1 0.182041041 0.03735722 1.198022e-01  0.2668125
#gamma3_unit1 0.262360444 0.03638522 1.974625e-01  0.3395631
#gamma4_unit1 0.002121964 0.01342127 8.558973e-09  0.9981108
#gamma5_unit1 0.042681687 0.02823426 1.137719e-02  0.1472883
#gamma6_unit1 0.113912308 0.20217174 2.529694e-03  0.8669608
#gamma7_unit1 0.138816463 0.20165027 5.874684e-03  0.8147086

print(unique(Mod1_w_100_random_init$real$gamma))
#                      est           se  lower_0.95 upper_0.95
#gamma1_unit1 0.389224063 0.05239586 2.926486e-01  0.4953513
#gamma2_unit1 0.181659419 0.03750370 1.192445e-01  0.2668456
#gamma3_unit1 0.262717465 0.03665687 1.973702e-01  0.3405214
#gamma4_unit1 0.001960109 0.01341629 2.854305e-09  0.9992605
#gamma5_unit1 0.042703307 0.02820808 1.140466e-02  0.1471149
#gamma6_unit1 0.113823110 0.20196046 2.531059e-03  0.8666943
#gamma7_unit1 0.139049982 0.20135166 5.942216e-03  0.8135596


# - local extinction across the different focal periods  - values are extremely similar
print(unique(model1_psi_gamSeason_epsSeason_detSeason$real$epsilon))
#                        est           se  lower_0.95 upper_0.95
#epsilon1_unit1 1.321700e-01 8.360324e-02 0.03521327  0.3885694
#epsilon2_unit1 4.156521e-01 5.088899e-02 0.32053882  0.5174930
#epsilon3_unit1 2.752919e-01 4.910003e-02 0.18995552  0.3809366
#epsilon4_unit1 7.326207e-01 7.473953e-02 0.56467307  0.8526791
#epsilon5_unit1 1.782948e-10 4.264778e-06 0.00000000  1.0000000
#epsilon6_unit1 8.545865e-02 7.958852e-02 0.01253902  0.4074569
#epsilon7_unit1 2.601254e-08 1.139673e-04 0.00000000  1.0000000

print(unique(Mod1_w_100_random_init$real$epsilon))
#                        est           se  lower_0.95 upper_0.95
#epsilon1_unit1 1.333677e-01 8.346970e-02 3.602178e-02 3.879197e-01
#epsilon2_unit1 4.050531e-01 5.118650e-02 3.098628e-01 5.079625e-01
#epsilon3_unit1 2.816785e-01 4.905045e-02 1.960310e-01 3.867452e-01
#epsilon4_unit1 7.328228e-01 7.462354e-02 5.651238e-01 8.527081e-01
#epsilon5_unit1 9.214309e-50 9.238306e-49 2.693267e-58 3.152435e-41
#epsilon6_unit1 8.555671e-02 7.963568e-02 1.256562e-02 4.075443e-01
#epsilon7_unit1 2.534352e-30 2.540952e-29 7.407703e-39 8.670620e-22

# to get the number of unque collection events and records from the chosen focal periods ####
# - used in a table in the main text
focal_periods1

# to get the number of unique records
condensed_data %>%
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
  # get the number of unique records from each primary period
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



# to get the number of unique collection events
condensed_data %>%
  # condense down to all unique collection events
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
  # get the number of unique collection events from each primary period
  count(primary_period)
# A tibble: 8 x 2
#   primary_period     n
#   <chr>          <int>
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

estimates <- c(unique(model1_psi_gamSeason_epsSeason_detSeason$real$psi$est), 
               unique(model1_psi_gamSeason_epsSeason_detSeason$derived$psi$est))
spp_rich_estimates <- estimates * 328 # multiple times total number of species
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

