# When species were collected over time
# and lat and long correlations with time

# load libraries ####
library(here)
library(tidyverse)
library(VennDiagram)

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

# set up time periods for the venn diagram ####
historic_sampling <- c(seq(1900,1969,1))
mid_low_sampling <- c(seq(1970,1999,1))
more_contemporary <- c(seq(2000,2018,1))


historic_sampling_spp <- condensed_data %>%
  filter(year %in% historic_sampling) %>%
  count(scientific_name) %>%
  rename(year1900to1969 = n) 
nrow(historic_sampling_spp) # 305

condensed_data %>%
  filter(year %in% historic_sampling) %>%
  count(year) %>%
  #arrange(desc(year)) # 1969 
  arrange(year) # 1900

mid_low_spp <- condensed_data %>%
  filter(year %in% mid_low_sampling) %>%
  count(scientific_name) %>%
  rename(year1970to1999 = n) 
nrow(mid_low_spp) # 74

condensed_data %>%
  filter(year %in% mid_low_sampling) %>%
  count(year) %>%
  #arrange(desc(year)) # 1997 
  arrange(year) # 1970


more_contemporary_spp <- condensed_data %>%
  filter(year %in% more_contemporary) %>%
  count(scientific_name) %>%
  rename(year2000to2018 = n) 
nrow(more_contemporary_spp) # 113

condensed_data %>%
  filter(year %in% more_contemporary) %>%
  count(year) %>%
  #arrange(desc(year)) # 2018 
  arrange(year) # 2003


family_spp_tibble <- condensed_data %>%
  count(family, scientific_name) %>%
  select(-n)

species_list_by_time_frame <- family_spp_tibble %>% 
  left_join(historic_sampling_spp) %>%
  left_join(mid_low_spp) %>%
  left_join(more_contemporary_spp)
species_list_by_time_frame
#328

# set up numbers for the venn diagram ####
species_list_by_time_frame %>% filter(!is.na(year1900to1969)) # 305
species_list_by_time_frame %>% filter(!is.na(year1970to1999)) # 74
species_list_by_time_frame %>% filter(!is.na(year2000to2018)) # 113
species_list_by_time_frame %>%
  filter(!is.na(year1900to1969) & !is.na(year1970to1999) & !is.na(year2000to2018)) #52
species_list_by_time_frame %>%
  filter(!is.na(year1900to1969) & !is.na(year1970to1999)) #70
species_list_by_time_frame %>%
  filter(!is.na(year1970to1999) & !is.na(year2000to2018)) #54
species_list_by_time_frame %>%
  filter(!is.na(year1900to1969) & !is.na(year2000to2018)) #92

# graph venn diagram ####
dev.off()

venn_diagram_spp_color_blind_w_labels <- draw.triple.venn(area1 = 305, area2 = 74, area3 = 113,
                                                          n12 = 70, n23 = 54, n13 = 92, n123 = 52,
                                                          category = c("1900-\n1969", "1970-\n1997", "2003-\n2018"),
                                                          fill = c("#D55E00", "#F0E442", "#0072B2"),
                                                          cex = 2, cat.cex = 1, cat.dist = .15,
                                                          cat.fontface = "bold") #fontface = "bold", cat.fontface = "bold", cat.dist = .15

# get percentages of species collected in certain time frames in the dataset ####
# 195 species not collected since 1969
195/328 *100 #59.5%
# 19 species only collected in 2003-2018
19/328 *100 #5.8%

# get a table of how many times each species is collected ####
# doing historic only and contemporary only

historic_or_contemp_only <- species_list_by_time_frame %>%
  filter((!is.na(year1900to1969) & is.na(year1970to1999) & is.na(year2000to2018)) |
           (!is.na(year2000to2018) & is.na(year1900to1969) & is.na(year1970to1999))) %>%
  # make a column that says historic or contemporary
  mutate(period = ifelse(!is.na(year1900to1969), "1900-1969", "2003-2018")) %>%
  # make a column that has the values in it
  mutate(value = ifelse(!is.na(year1900to1969), year1900to1969, year2000to2018))
historic_or_contemp_only

density_plot <- ggplot(historic_or_contemp_only, aes(x = value, color = period, fill = period)) +
  geom_histogram() +
  facet_grid(period~., scales = "free_y") +
  labs(y = "Species Count", x = "Num. of Unique Records") +
  #scale_x_continuous(expand = c(0,0)) +
  #scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values=c("#0072B2","#D55E00" ),
                    breaks=c("2003-2018","1900-1969"))+
  scale_colour_manual(values=c("#0072B2","#D55E00"),
                      breaks=c("2003-2018","1900-1969"))+
  theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
        panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
        panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
        panel.border=element_rect(colour="black", fill=NA),
        axis.ticks=element_line(colour = 'black', size = 0.5), #sets the tick lines
        legend.position="",
        legend.title=element_blank(),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(linetype="solid", color = "black", fill = "grey90"),
        legend.key=element_blank(), #remove the border around each item
        axis.title.x = element_text(face="bold", size=14, color="black"), #size of x-axis title
        axis.title.y = element_text(face="bold", size=14, color="black"), #size of y-axis title
        axis.text.x = element_text(size=12, color="black"), #size of x-axis text
        axis.text.y = element_text(size=12, color="black"))#size of y-axis text
density_plot

# save figures ####

#ggsave("venn_diagram_color_blind_w_labels.png", width = 3, height = 3,
#       units = "in", dpi = 600, plot = venn_diagram_spp_color_blind_w_labels,
#       path = figure_path, family = "Arial")

#ggsave("historic_or_contemporary_density_plot.png", width = 3, height = 4,
#       units = "in", dpi = 600, plot = density_plot,
#       path = figure_path, family = "Arial")

# what decades were the historic only species were captured in ####
historic_only <- historic_or_contemp_only %>%
  # pull out historic only species
  filter(period == "1900-1969") %>%
  # pull out only the "rare" species, less than 10 unique collections
  filter(value < 10)
historic_only

historic_only_list_rare <- as.vector(historic_only$scientific_name)

# what decade has the most *rare* (i.e. less than 10 unique collection events)
historic_rare_decades <- condensed_data %>%
  filter(scientific_name %in% historic_only_list_rare) %>%
  # create decade column
  mutate(decade = ifelse(year >= 1900 & year < 1910, "1900s", NA),
         decade = ifelse(year >= 1910 & year < 1920, "1910s", decade),
         decade = ifelse(year >= 1920 & year < 1930, "1920s", decade),
         decade = ifelse(year >= 1930 & year < 1940, "1930s", decade),
         decade = ifelse(year >= 1940 & year < 1950, "1940s", decade),
         decade = ifelse(year >= 1950 & year < 1960, "1950s", decade),
         decade = ifelse(year >= 1960 & year < 1970, "1960s", decade)) %>%
  # condense down to unique combinations of species and decade
  count(scientific_name, decade) %>%
  # how many rare species captured per year
  count(decade)
historic_rare_decades
# A tibble: 7 x 2
#decade     n
#<chr>  <int>
#1 1900s      3
#2 1910s     11
#3 1920s     87
#4 1930s     35
#5 1940s     56
#6 1950s     58
#7 1960s     24

historic_common <- historic_or_contemp_only %>%
  # pull out historic only species
  filter(period == "1900-1969") %>%
  arrange(desc(value)) %>%
  filter(value > 30)
historic_common

# what decades were the contemporary only species were captured in ####
contemp_only <- historic_or_contemp_only %>%
  # pull out historic only species
  filter(period == "2003-2018") %>%
  # pull out only the "rare" species, less than 10 unique collections
  filter(value < 10)
contemp_only

contemp_only_list_rare <- as.vector(contemp_only$scientific_name)

# what decade has the most *rare* (i.e. less than 10 unique collection events)
contemp_rare_decades <- condensed_data %>%
  filter(scientific_name %in% contemp_only_list_rare) %>%
  # create decade column
  mutate(decade = ifelse(year >= 2000 & year < 2010, "2000s", NA),
         decade = ifelse(year >= 2010 & year < 2019, "2010s", decade)) %>%
  # condense down to unique combinations of species and decade
  count(scientific_name, decade) %>%
  # how many rare species captured per year
  count(decade)
contemp_rare_decades
# A tibble: 2 x 2
#decade     n
#<chr>  <int>
#1 2000s      4
#2 2010s     16

contemp_common <- historic_or_contemp_only %>%
  # pull out historic only species
  filter(period == "2003-2018") %>%
  arrange(desc(value))
contemp_common # none are collected over 5 unique times and only collected recently


## geographic samping over time ####

# will remove samples that mention being collected from wake county or random point in wake county 
# from this analysis 

condensed_data %>%
  count(where_obtained_coordinates) %>%
  print(n = 30)
# A tibble: 27 x 2
#   where_obtained_coordinates                                       n
#   <chr>                                                        <int>
#1 dataset provided                                              2760
#2 dataset provided|google earth                                    1
#3 dataset provided|google earth - McCullers, Garner                1
#4 dataset provided|google earth - Raleigh                          7
#5 dataset provided|google earth - Wake County                      1 # will keep these because there are also some that condensed down with dataset provided coordinates
#6 google earth                                                   180
#7 google earth - 440 Gorman St                                     3
#8 google earth - Apex                                              1
#9 google earth - Central Utility Plant, NCSU Centennial Campus     9
#10 google earth - Ebenezer Church Rd                                1
#11 google earth - gardner hall raleigh                              1
#12 google earth - Garner                                            7
#13 google earth - Garner rd. 2542, Garner                           1
#14 google earth - Lake Crabtree County Park, Wake                   7
#15 google earth - McCullers Crossroads, Garner                      1
#16 google earth - McCullers, Garner                                16
#17 google earth - NC fairgrounds raleigh                            1
#18 google earth - NCSU                                              1
#19 google earth - picked a random spot on google earth              1 # will remove this
#20 google earth - raleigh                                           1
#21 google earth - Raleigh                                        2852
#22 google earth - SE, Raleigh (gives general area)                  3
#23 google earth - SR 1390 Raleigh                                   4
#24 google earth - State Rd 1152                                    11
#25 google earth - state rd 1604, apex                               1
#26 google earth - wake county                                       1 # will remove this
#27 google earth - Wake County                                     207 # will remove this



remove_list <- c("google earth - wake county", "google earth - Wake County", "google earth - picked a random spot on google earth")

condensed_data %>%
  filter(where_obtained_coordinates %in% remove_list) # removing 209 specimens


condensed_collection_events <- condensed_data %>%
  # remove specimens only georeferenced as Wake Co
  filter(where_obtained_coordinates %nin% remove_list) %>%
  count(day, month, year, recorded_by, decimal_latitude, decimal_longitude) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(decimal_latitude = as.numeric(decimal_latitude)) %>%
  mutate(decimal_longitude = as.numeric(decimal_longitude))
condensed_collection_events  


latitude_cor_test <- cor.test(condensed_collection_events$decimal_latitude, condensed_collection_events$year,
                              method = "pearson")
latitude_cor_test
#Pearson's product-moment correlation
#
#data:  condensed_collection_events$decimal_latitude and condensed_collection_events$year
#t = 12.371, df = 2846, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.1907541 0.2604687
#sample estimates:
#      cor 
#0.2259006 

longitude_cor_test <- cor.test(condensed_collection_events$decimal_longitude, condensed_collection_events$year,
                               method = "pearson")
longitude_cor_test
#Pearson's product-moment correlation
#
#data:  condensed_collection_events$decimal_longitude and condensed_collection_events$year
#t = -23.382, df = 2846, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.4317875 -0.3701529
#sample estimates:
#       cor 
#-0.4014246