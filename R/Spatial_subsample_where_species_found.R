# check how many species are found in different locations of Wake Co.


# load libraries ####
library(tidyverse)
library(here)
library(VennDiagram)
library(naniar)
library(readxl)
#library(tidygeocoder)

# set relative pathways ####
data_path <- here::here("data")
figure_path <- here::here("figs")
Rsource_path <- here::here("R/Resources")

# load in reverse geocoded data ####
load(paste(Rsource_path,"reverse_geocoded_locations.rda", sep = "/"))

reverse_geocoded_locations
reverse_geocoded_locations %>%
  count(city)
# A tibble: 8 x 2
#city                    n
#<chr>               <int>
#1 Cary           49
#2 Durham          1
#3 Raleigh       171
#4 Wake Forest     1
#5 NA             91

str(reverse_geocoded_locations)


reverse_geocoded_locations2 <- reverse_geocoded_locations %>%
  select(-county,-n) %>%
  mutate(decimal_latitude = as.numeric(decimal_latitude)) %>%
  mutate(decimal_longitude = as.numeric(decimal_longitude))

# read in data file ####
condensed_data <- read_csv(paste(data_path, "Dataset1.csv", sep="/"),
                           col_types = 
                             cols( 
                               .default = col_character(),
                               decimal_longitude = col_double(),
                               decimal_latitude = col_double()))
condensed_data

# join original data and reverse geocoded city location ####
condensed_data2 <- condensed_data %>%
  # create a unique row number identifier to see if joining data causes repeats
  left_join(reverse_geocoded_locations2)
condensed_data2

# need to check for records that may be placed in Raleigh because of initial georeferencing 
# from google earth off of vague locality information (e.g. Wake Co)
condensed_data2 %>%
  # filter to only records reverse located to Raleigh
  filter(city == "Raleigh") %>%
  count(decimal_latitude, decimal_longitude, city, where_obtained_coordinates) %>%
  # remove dataset provided coordinates
  filter(where_obtained_coordinates != "dataset provided") %>%
  print(n = 50)
# there are 31 location records that need to be checked
# of these, some records were georeferenced in google earth using Wake County, so will remove these
# from the dataset for the subset of spatial analyses

# remove uncertain georeferencing records that were assigned to Raleigh but locality informaiton was for Wake County
spatial_subset_data <- condensed_data2 %>%
  filter(city == "Raleigh") %>%
  filter(where_obtained_coordinates != "google earth - Wake County" &
           where_obtained_coordinates != "google earth - wake county")
spatial_subset_data

nrow(condensed_data2) #6080
nrow(condensed_data2 %>% filter(city == "Raleigh")) # 5616
nrow(spatial_subset_data) #5408
5616 - 5408 # 208 (unique records removed because likely match to Raleigh because of general locality info)
6080 - 5408 # 672 (unique records not from within Raleigh)

# to get subset of data not found in Raleigh ####
# - includes specimens that map to raleigh but had general information which is why they likely mapped to raleigh
?anti_join()

not_raleigh_subset <- condensed_data2 %>%
  select(-boundingbox) %>%
  anti_join(spatial_subset_data)
not_raleigh_subset

# to make a venn diagram of species ####
full_spp_list <- as.vector(unique(condensed_data$scientific_name))
full_spp_list # 328 species

raleigh_spp_list <- as.vector(unique(spatial_subset_data$scientific_name))
raleigh_spp_list #316 species

not_raleigh_list <- as.vector(unique(not_raleigh_subset$scientific_name))
not_raleigh_list #145 species

venn_diagram_tibble <- condensed_data2 %>%
  count(scientific_name) %>%
  mutate(wake_co = "yes") %>%
  mutate(raleigh = if_else(scientific_name %in% raleigh_spp_list, "yes", "NA")) %>%
  mutate(not_raleigh = if_else(scientific_name %in% not_raleigh_list, "yes", "NA")) %>%
  replace_with_na(replace = list(raleigh = "NA",
                                 not_raleigh = "NA"))
venn_diagram_tibble  

# set up numbers for the venn diagram ####
venn_diagram_tibble %>% filter(!is.na(raleigh)) # 316
venn_diagram_tibble %>% filter(!is.na(not_raleigh)) # 145
venn_diagram_tibble %>%
  filter(!is.na(raleigh) & !is.na(not_raleigh)) # 133


# graph venn diagram ####
dev.off()

raleigh_subsample_venndiagram <- draw.pairwise.venn(area1 = 316, area2 = 145,
                                                    cross.area = 133 ,
                                                    category = c("Raleigh", "\nOutside\nRaleigh"),
                                                    fill = c("#00BFC4", "grey40"),
                                                    cex = 2, cat.cex = 1.2, cat.dist = .04,
                                                    cat.fontface = "bold", cat.pos = c(-145, 145)) #,
#cat.prompts = TRUE) #fontface = "bold", cat.fontface = "bold", cat.dist = .15

# get percentages of species found where ####

183/328 * 100 # percent only in raleigh 55.79268
12/328 * 100 # percent only in not raleigh 3.658537
133/328 * 100 # percent found both raleigh and not raleigh 40.54878

# percent found in raleigh
316/328 * 100 #96.34146

# percent found "not in raleigh" full grey circle
145/328 * 100 # 44.20732

# total number of samples
specimens <- as.numeric(condensed_data$n)
sum(specimens) #11227

# total number of unique records
nrow(condensed_data) #6080

# number of records from the subset
spatial_subset <- as.numeric(spatial_subset_data$n)
sum(spatial_subset) #9633

# percent of total samples collected in raleigh
9633 /11227 * 100 # 85.80208

# percent of unique records collected in raleigh
nrow(spatial_subset_data)/6080 * 100 # 88.94737

# percent of total samples collected in not raleigh
not_raliegh_specimens <- as.numeric(not_raleigh_subset$n)
sum(not_raliegh_specimens) #1594

1594/11227 * 100 # 14.19792

# percent of unique records collected in not raleigh

nrow(not_raleigh_subset)/6080 * 100 # 11.05263

# get the average number of species collected per collection event ####
# entire data set
number_collection_events <- condensed_data %>%
  count(day, month, year, recorded_by, decimal_latitude, decimal_longitude)
nrow(number_collection_events) # 2949
nrow(condensed_data) #6080

6080/2949 # on average 2.1 species found each collection event

# raleigh subsample
nrow(spatial_subset_data) #5408
raleigh_collection_events <- spatial_subset_data %>%
  count(day, month, year, recorded_by, decimal_latitude, decimal_longitude)
nrow(raleigh_collection_events) # 2625

5408/2625 # 2.1 species on average found each collection event

# not raleigh subsample
nrow(not_raleigh_subset) #672
not_raleigh_collection_events <- not_raleigh_subset %>%
  count(day, month, year, recorded_by, decimal_latitude, decimal_longitude)
nrow(not_raleigh_collection_events) #330

672/330 # on average 2.0 species on average found each collection event

# save figure ####
#ggsave("raleigh_VD_w_labels.png", width = 4, height = 4,
#       units = "in", dpi = 600, plot = raleigh_subsample_venndiagram,
#       path = figure_path, family = "Arial")


# get a table of how many times each species is collected ####
# doing raleigh only, both, and not raleigh only


spatial_subset_data2 <- spatial_subset_data %>%
  count(scientific_name) %>%
  rename(raleigh = n)
spatial_subset_data2


not_raleigh_data2 <- not_raleigh_subset %>%
  count(scientific_name) %>%
  rename(outside_raleigh = n)
not_raleigh_data2

full_spp_list_tb <- condensed_data2 %>%
  count(scientific_name) %>%
  select(-n)
full_spp_list_tb


density_tibble <- full_spp_list_tb %>%
  left_join(spatial_subset_data2) %>%
  left_join(not_raleigh_data2) %>%
  mutate(location = ifelse(!is.na(raleigh) & is.na(outside_raleigh), "Raleigh", "NA")) %>%
  mutate(location = ifelse(!is.na(raleigh) & !is.na(outside_raleigh), "Both", location)) %>%
  mutate(location = ifelse(is.na(raleigh) & !is.na(outside_raleigh), "Outside Raleigh", location)) %>%
  mutate(value = ifelse(!is.na(raleigh) & is.na(outside_raleigh), raleigh, "NA")) %>%
  mutate(value = ifelse(!is.na(raleigh) & !is.na(outside_raleigh), raleigh + outside_raleigh, value)) %>%
  mutate(value = ifelse(is.na(raleigh) & !is.na(outside_raleigh), outside_raleigh, value)) %>%
  mutate(value = as.numeric(value))
density_tibble


density_plot_location <- ggplot(density_tibble, aes(x = value, color = location, fill = location)) +
  geom_histogram(bins = 100) +
  facet_grid(location~., scales = "free") + #scales = "free_y"
  labs(y = "Species Count", x = "Num. of Unique Records") +
  scale_x_continuous(expand = c(0,0), breaks = seq(0,300,20)) +
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values=c("#00BFC4","grey40", "grey80" ),
                    breaks=c("Raleigh","Outside Raleigh","Both"))+
  scale_colour_manual(values=c("#00BFC4","grey40", "grey80"),
                      breaks=c("Raleigh","Outside Raleigh","Both"))+
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
density_plot_location

# save figure ####

#ggsave("raleigh_outsideRaleigh_density_plot.png", width = 6, height = 6,
#       units = "in", dpi = 600, plot = density_plot_location,
#       path = figure_path, family = "Arial")

# what species unique to outside raleigh ####
density_tibble %>%
  filter(location == "Outside Raleigh")
# A tibble: 12 x 5
#scientific_name           raleigh outside_raleigh location        value
#<chr>                       <int>           <int> <chr>           <dbl>
#1 Anthophora bomboides           NA               1 Outside Raleigh     1
#2 Bombus fervidus                NA               1 Outside Raleigh     1
#3 Coelioxys modesta              NA               2 Outside Raleigh     2
#4 Epeolus ilicis                 NA               1 Outside Raleigh     1
#5 Lasioglossum simplex           NA               1 Outside Raleigh     1
#6 Megachile frigida              NA               3 Outside Raleigh     3
#7 Megachile lippiae              NA               1 Outside Raleigh     1
#8 Megachile policaris            NA               1 Outside Raleigh     1
#9 Nomada vegana                  NA               2 Outside Raleigh     2
#10 Perdita bradleyi               NA               1 Outside Raleigh     1
#11 Perdita georgica               NA               1 Outside Raleigh     1
#12 Pseudopanurgus aestivalis      NA               1 Outside Raleigh     1

## seeing what traits these different species have ####
# load in the trait data
raw_data <- read_excel(paste(data_path, "Bee_trait_file.xlsx", sep="/"),
                       sheet = "bee_trait_file",
                       col_types = "text")
raw_data

# fix column names ####
trait_data <- raw_data %>%
  rename(native_notes = "Native Notes") %>%
  rename(native_source = "Native Source") %>%
  rename(nesting_notes = "nesting Notes") %>%
  rename(nesting_sources = "Nesting Sources") %>%
  rename(diet_notes = "Diet Notes") %>%
  rename(diet_sources = "Diet Source") %>%
  rename(social_notes = "Social Notes") %>%
  rename(social_sources = "Social Sources") %>%
  rename(parasite_host_notes = "Parasite Host Notes") %>%
  rename(parasite_host_sources = "Parasite Host Sources") %>%
  rename(scientific_name = "Scientific Name") %>%
  rename(family = Family) %>%
  rename(native_status = "Native Status") %>%
  rename(nesting_level = "Nesting level") %>%
  rename(nesting_RentBuild = "Nesting Rent or Build") %>%
  rename(nest_substrate = "Nest Substrate") %>%
  rename(diet_category = "Diet Category") %>%
  rename(diet_oligo_host = "Diet Oligo Host") %>%
  rename(social_category = "Social Category") %>%
  rename(parasite_host = "Parasite Host")
trait_data


# dummy code the trait data
trait_data2 <- trait_data %>%
  # native status dummy coding -- none have missing data so do not need to account for that
  mutate(native_status_expansion = ifelse(native_status == "ancient range expansion" &
                                            !is.na(native_status), 1, 0)) %>%
  mutate(native_status_introduced = ifelse(native_status == "introduced" &
                                             !is.na(native_status), 1, 0)) %>%
  mutate(native_status_native = ifelse(native_status == "presumed native" &
                                         !is.na(native_status), 1, 0)) %>%
  # nesting level dummy coding - and need to account for missing values
  mutate(nesting_level_above = ifelse((nesting_level == "above" |
                                         nesting_level == "above, below") &
                                        !is.na(nesting_level), 1, 0),
         nesting_level_above = ifelse(is.na(nesting_level), NA, nesting_level_above)) %>%
  mutate(nesting_level_below = ifelse((nesting_level == "below" |
                                         nesting_level == "above, below") &
                                        !is.na(nesting_level), 1, 0),
         nesting_level_below = ifelse(is.na(nesting_level), NA, nesting_level_below)) %>%
  # nesting rent build dummy coding - and need to account for missing values
  mutate(nestingRB_rent = ifelse((nesting_RentBuild == "rent" |
                                    nesting_RentBuild == "rent, build") &
                                   !is.na(nesting_RentBuild), 1, 0),
         nestingRB_rent = ifelse(is.na(nesting_RentBuild), NA, nestingRB_rent)) %>%
  mutate(nestingRB_build = ifelse((nesting_RentBuild == "build" |
                                     nesting_RentBuild == "rent, build") &
                                    !is.na(nesting_RentBuild), 1, 0),
         nestingRB_build = ifelse(is.na(nesting_RentBuild), NA, nestingRB_build)) %>%
  mutate(nestingRB_parasite = ifelse(nesting_RentBuild == "parasite" &
                                       !is.na(nesting_RentBuild), 1, 0),
         nestingRB_parasite = ifelse(is.na(nesting_RentBuild), NA, nestingRB_parasite)) %>%
  # nesting substrate dummy coding - and need to take into account the missing values
  mutate(nestSubstrate_cavity = ifelse((nest_substrate == "cavity" |
                                          nest_substrate == "cavity, soil" |
                                          nest_substrate == "soil, stem, wood, cavity" |
                                          nest_substrate == "wood, cavity") &
                                         !is.na(nest_substrate), 1, 0),
         nestSubstrate_cavity = ifelse(is.na(nest_substrate), NA, nestSubstrate_cavity)) %>%
  mutate(nestSubstrate_soil = ifelse((nest_substrate == "soil" |
                                        nest_substrate == "cavity, soil" |
                                        nest_substrate == "soil, stem, wood, cavity" |
                                        nest_substrate == "soil, stem, wood" |
                                        nest_substrate == "stem, soil" |
                                        nest_substrate == "stem, wood, soil") &
                                       !is.na(nest_substrate), 1, 0),
         nestSubstrate_soil = ifelse(is.na(nest_substrate), NA, nestSubstrate_soil)) %>%
  mutate(nestSubstrate_wood = ifelse((nest_substrate == "wood" |
                                        nest_substrate == "wood, cavity" |
                                        nest_substrate == "soil, stem, wood, cavity" |
                                        nest_substrate == "soil, stem, wood" |
                                        nest_substrate == "stem, wood" |
                                        nest_substrate == "stem, wood, soil" |
                                        nest_substrate == "stem-wood") &
                                       !is.na(nest_substrate), 1, 0),
         nestSubstrate_wood = ifelse(is.na(nest_substrate), NA, nestSubstrate_wood)) %>%
  mutate(nestSubstrate_stem = ifelse((nest_substrate == "stem" |
                                        nest_substrate == "soil, stem, wood" |
                                        nest_substrate == "soil, stem, wood, cavity" |
                                        nest_substrate == "stem-wood" |
                                        nest_substrate == "stem, wood" |
                                        nest_substrate == "stem, wood, soil") &
                                       !is.na(nest_substrate), 1, 0),
         nestSubstrate_stem = ifelse(is.na(nest_substrate), NA, nestSubstrate_stem)) %>%
  mutate(nestSubstrate_exposed = ifelse(nest_substrate == "exposed" &
                                          !is.na(nest_substrate), 1, 0),
         nestSubstrate_exposed = ifelse(is.na(nest_substrate), NA, nestSubstrate_exposed)) %>%
  # diet category dummy coding - and taking into account missing values
  mutate(diet_category_generalist = ifelse(diet_category == "generalist" &
                                             !is.na(diet_category), 1, 0),
         diet_category_generalist = ifelse(is.na(diet_category), NA, diet_category_generalist)) %>%
  mutate(diet_category_parasite = ifelse(diet_category == "parasite" &
                                           !is.na(diet_category), 1, 0),
         diet_category_parasite = ifelse(is.na(diet_category), NA, diet_category_parasite)) %>%
  mutate(diet_category_specialist = ifelse(diet_category == "specialist" &
                                             !is.na(diet_category), 1, 0),
         diet_category_specialist = ifelse(is.na(diet_category), NA, diet_category_specialist)) %>%
  # social category dummy coding
  mutate(social_category_parasite = ifelse(social_category == "parasite" &
                                             !is.na(social_category), 1, 0),
         social_category_parasite = ifelse(is.na(social_category), NA, social_category_parasite)) %>%
  mutate(social_category_parasite_social = ifelse(social_category == "parasite (social)" &
                                                    !is.na(social_category), 1, 0),
         social_category_parasite_social = ifelse(is.na(social_category), NA, social_category_parasite_social)) %>%
  mutate(social_category_polymorphic = ifelse(social_category == "polymorphic" &
                                                !is.na(social_category), 1, 0),
         social_category_polymorphic = ifelse(is.na(social_category), NA, social_category_polymorphic)) %>%
  mutate(social_category_social = ifelse((social_category == "social" |
                                            social_category == "social, solitary") &
                                           !is.na(social_category), 1, 0),
         social_category_social = ifelse(is.na(social_category), NA, social_category_social)) %>%
  mutate(social_category_solitary = ifelse((social_category == "solitary" |
                                              social_category == "social, solitary") &
                                             !is.na(social_category), 1, 0),
         social_category_solitary = ifelse(is.na(social_category), NA, social_category_solitary))
trait_data2


trait_names <- c("native_status_expansion", "native_status_introduced", "native_status_native", 
                 "nesting_level_above", "nesting_level_below", "nestingRB_rent", "nestingRB_build", 
                 "nestingRB_parasite", "nestSubstrate_cavity", "nestSubstrate_soil", 
                 "nestSubstrate_wood", "nestSubstrate_stem", "nestSubstrate_exposed", 
                 "diet_category_generalist", "diet_category_parasite", "diet_category_specialist", 
                 "social_category_parasite", "social_category_parasite_social", "social_category_polymorphic", 
                 "social_category_social", "social_category_solitary")

native_status_names <- c("native_status_expansion", "native_status_introduced", "native_status_native")

nesting_level_names <- c("nesting_level_above", "nesting_level_below")

nesting_RB_names <- c("nestingRB_rent", "nestingRB_build", "nestingRB_parasite")

nesting_substrate_names <- c("nestSubstrate_cavity", "nestSubstrate_soil", "nestSubstrate_wood", "nestSubstrate_stem", "nestSubstrate_exposed")

diet_category_names <- c("diet_category_generalist", "diet_category_parasite", "diet_category_specialist")

social_category_names <- c("social_category_parasite", "social_category_parasite_social", "social_category_polymorphic", 
                           "social_category_social", "social_category_solitary")

# merge trait data and where species were collected
density_tibble2 <- density_tibble %>% 
  left_join(trait_data2)
density_tibble2

# to get the number of species that have data in each of the collection locations

native_status_n <- density_tibble2 %>%
  group_by(location) %>%
  count(native_status) %>%
  summarise(sum_species = sum(n)) %>%
  mutate(trait_category = "native_status")
native_status_n

nesting_level_n <- density_tibble2 %>%
  group_by(location) %>%
  count(nesting_level) %>%
  filter(!is.na(nesting_level)) %>%
  summarise(sum_species = sum(n)) %>%
  mutate(trait_category = "nesting_level")
nesting_level_n

nesting_RB_n <- density_tibble2 %>%
  group_by(location) %>%
  count(nesting_RentBuild) %>%
  filter(!is.na(nesting_RentBuild)) %>%
  summarise(sum_species = sum(n)) %>%
  mutate(trait_category = "nesting_RB")
nesting_RB_n

nesting_substrate_n <- density_tibble2 %>%
  group_by(location) %>%
  count(nest_substrate) %>%
  filter(!is.na(nest_substrate)) %>%
  summarise(sum_species = sum(n)) %>%
  mutate(trait_category = "nest_substrate")
nesting_substrate_n

diet_category_n <- density_tibble2 %>%
  group_by(location) %>%
  count(diet_category) %>%
  filter(!is.na(diet_category)) %>%
  summarise(sum_species = sum(n)) %>%
  mutate(trait_category = "diet_category")
diet_category_n

social_category_n <- density_tibble2 %>%
  group_by(location) %>%
  count(social_category) %>%
  filter(!is.na(social_category)) %>%
  summarise(sum_species = sum(n)) %>%
  mutate(trait_category = "social_category")
social_category_n


# join all the sample sizes together
joined_samples <- native_status_n %>%
  full_join(nesting_level_n) %>%
  full_join(nesting_RB_n) %>%
  full_join(nesting_substrate_n) %>%
  full_join(diet_category_n) %>%
  full_join(social_category_n)
joined_samples


heatmap_data <- density_tibble2 %>%
  pivot_longer(cols = trait_names, names_to = "categorical_trait", values_to = "value2") %>%
  # only keep certain columns
  select(scientific_name, location, categorical_trait, value2) %>%
  # filter so only the traits that are represented are present
  filter(value2 == 1) %>%
  # to get a count of how many of each categorical trait by timeperiod
  count(location, categorical_trait) %>%
  rename(raw_count = n) %>%
  # set up what trait category these go with
  mutate(trait_category = ifelse(categorical_trait %in% native_status_names, "native_status", NA),
         trait_category = ifelse(categorical_trait %in% nesting_level_names, "nesting_level", trait_category),
         trait_category = ifelse(categorical_trait %in% nesting_RB_names, "nesting_RB", trait_category),
         trait_category = ifelse(categorical_trait %in% nesting_substrate_names, "nest_substrate", trait_category),
         trait_category = ifelse(categorical_trait %in% diet_category_names, "diet_category", trait_category),
         trait_category = ifelse(categorical_trait %in% social_category_names, "social_category", trait_category)) %>%
  # join with the dataset that gives the number to dive by
  left_join(joined_samples) %>%
  # get the proportion
  mutate(prop = raw_count/sum_species) %>%
  # change this to a percent
  mutate(percentage = prop*100) %>%
  # create an order for plotting
  mutate(order = ifelse(location == "Raleigh", 1, NA),
         order = ifelse(location == "Both", 2, order),
         order = ifelse(location == "Outside Raleigh", 3, order)) %>%
  # set it up so that the label color changes based on the proportion
  mutate(label_color = ifelse(prop >= 0.55, "white", "black"))
heatmap_data



heatmap_data %>%
  ggplot(aes(x = reorder(location, order), y = categorical_trait)) +
  geom_tile(aes(fill = prop)) +
  scale_fill_viridis_c(begin = 1, end = 0) +  #option = "magma"
  #scale_fill_gradient2(low = "darkslategray1", mid = "yellow", high = "turquoise4")
  xlab("Location") +
  ylab("Categorical trait") +
  geom_hline(yintercept = 3.5) + 
  geom_hline(yintercept = 6.5) + 
  geom_hline(yintercept = 8.5) + 
  geom_hline(yintercept = 11.5) + 
  geom_hline(yintercept = 16.5) 


heatmap_plot <- heatmap_data %>%
  ggplot(aes(x = reorder(location, order), y = categorical_trait)) +
  geom_tile(aes(fill = prop)) +
  geom_text(aes(label = round(prop, 2), color = label_color), size = 3) +
  scale_color_manual(breaks = c("white", "black"),
                     values = c("white", "black")) +
  guides(color = "none") +
  scale_fill_viridis_c(begin = 1, end = 0, name = "Proportion") +  #option = "magma"
  #scale_fill_gradient2(low = "darkslategray1", mid = "yellow", high = "turquoise4")
  xlab("Location") +
  ylab("Categorical trait") +
  #geom_text(aes(label = round(prop, 2)), size = 3) +
  geom_hline(yintercept = 3.5) + 
  geom_hline(yintercept = 6.5) + 
  geom_hline(yintercept = 8.5) + 
  geom_hline(yintercept = 11.5) + 
  geom_hline(yintercept = 16.5) +
  # to update the labels on the y axis
  scale_y_discrete(
    labels = c(
      "native_status_expansion" = "expansion",
      "native_status_introduced" = "introduced",
      "native_status_native" = "native",
      "nesting_level_above" = "above",
      "nesting_level_below" = "below",
      "nestingRB_rent" = "rent",
      "nestingRB_build" = "build",
      "nestingRB_parasite" = "parasite",
      "nestSubstrate_cavity" = "cavity",
      "nestSubstrate_soil" = "soil",
      "nestSubstrate_wood" = "wood",
      "nestSubstrate_stem" = "stem",
      "nestSubstrate_exposed" = "exposed",
      "diet_category_generalist" = "generalist",
      "diet_category_parasite" = "parasite",
      "diet_category_specialist" = "specialist",
      "social_category_parasite" = "parasite",
      "social_category_parasite_social" = "social parasite",
      "social_category_polymorphic" = "polymorphic",
      "social_category_social" = "social",
      "social_category_solitary" = "solitary")) +
  scale_x_discrete(
    labels = c(
      "Raleigh" = "Raleigh",
      "Both" = "Both",
      "Outside Raleigh" = "Outside\nRaleigh")) +
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
        legend.text = element_text(size = 10),
        #legend.title = element_blank(),
        axis.title.x = element_text(face="bold", size=14, color="black"), #size of x-axis title
        axis.title.y = element_text(face="bold", size=14, color="black"), #size of y-axis title
        axis.text.x = element_text(size=12, color="black"), #size of x-axis text
        axis.text.y = element_text(size=12, color="black"))#size of y-axis text 
heatmap_plot

#ggsave("heatmap_plot_Raleigh_Wake.png", width = 5, height = 8,
#       units = "in", dpi = 600, plot = heatmap_plot,
#       path = figure_path, family = "Arial")


# create venn diagrams to show sample size ####
# different venn diagram for each trait category
#- to show how many species have that trait information in each time frame instead of using a table
# - using the same colors as the venn diagram of where species were collected
joined_samples


# -- native status
#1 Both                    133 native_status  
#2 Outside Raleigh          12 native_status  
#3 Raleigh                 183 native_status

dev.off()
native_status_VD_n_RvW <- draw.pairwise.venn(area1 = 183+133, area2 = 12+133, 
                                         cross.area = 133,
                                         #category = c("Raleigh", "\nOutside\nRaleigh"),
                                         fill = c("#00BFC4", "grey40"),
                                         cex = 1, cat.cex = 1, ext.txt = TRUE,
                                         ext.percent = c(.3,.3,.3), #cat.dist = .15,
                                         ext.dist = 0.05,
                                         cat.fontface = "bold") #font
dev.off()


# -- nesting level
#4 Both                    130 nesting_level  
#5 Outside Raleigh          12 nesting_level  
#6 Raleigh                 178 nesting_level 

dev.off()
nesting_level_VD_n_RvW <- draw.pairwise.venn(area1 = 178+130, area2 = 12+130,
                                         cross.area = 130,
                                         #category = c("Raleigh", "\nOutside\nRaleigh"),
                                         fill = c("#00BFC4", "grey40"),
                                         cex = 1, cat.cex = 1, ext.txt = TRUE,
                                         ext.percent = c(.3,.3,.3), #cat.dist = .15,
                                         ext.dist = 0.05, cat.fontface = "bold") #font
dev.off()

# -- nesting rent or build
#7 Both                    128 nesting_RB     
#8 Outside Raleigh          12 nesting_RB     
#9 Raleigh                 178 nesting_RB     
   

dev.off()
nest_RB_VD_n_RvW <- draw.pairwise.venn(area1 = 178+128, area2 = 12+128, 
                                   cross.area = 128,
                                   #category = c("Raleigh", "\nOutside\nRaleigh"),
                                   fill = c("#00BFC4", "grey40"),
                                   cex = 1, cat.cex = 1, ext.txt = TRUE,
                                   ext.percent = c(.3,.3,.3), #cat.dist = .15,
                                   ext.dist = 0.05,
                                   cat.fontface = "bold") #font
dev.off()

# -- nest substrate
#10 Both                    130 nest_substrate 
#11 Outside Raleigh          11 nest_substrate 
#12 Raleigh                 176 nest_substrate 


dev.off()
nest_substrate_VD_n_RvW <- draw.pairwise.venn(area1 = 176+130, area2 = 11+130, 
                                          cross.area = 130,
                                          #category = c("Raleigh", "\nOutside\nRaleigh"),
                                          fill = c("#00BFC4", "grey40"),
                                          cex = 1, cat.cex = 1, ext.txt = TRUE,
                                          ext.percent = c(.3,.3,.3), #cat.dist = .15,
                                          ext.dist = 0.05,
                                          cat.fontface = "bold") #font
dev.off()

# -- diet category
#13 Both                    122 diet_category  
#14 Outside Raleigh          10 diet_category  
#15 Raleigh                 153 diet_category  
  

dev.off()
diet_category_VD_n_RvW <- draw.pairwise.venn(area1 = 153+122, area2 = 10+122, 
                                         cross.area = 122,
                                         #category = c("Raleigh", "\nOutside\nRaleigh"),
                                         fill = c("#00BFC4", "grey40"),
                                         cex = 1, cat.cex = 1, ext.txt = TRUE,
                                         ext.percent = c(.3,.3,.3), #cat.dist = .15,
                                         ext.dist = 0.05,
                                         cat.fontface = "bold") #font
dev.off()

# -- social category
#16 Both                    132 social_category
#17 Outside Raleigh          12 social_category
#18 Raleigh                 181 social_category

dev.off()
social_category_VD_n_RvW <- draw.pairwise.venn(area1 = 181+132, area2 = 12+132, 
                                           cross.area = 132,
                                           #category = c("Raleigh", "\nOutside\nRaleigh"),
                                           fill = c("#00BFC4", "grey40"),
                                           cex = 1, cat.cex = 1, ext.txt = TRUE,
                                           ext.percent = c(.3,.3,.3), #cat.dist = .15,
                                           ext.dist = 0.05,
                                           cat.fontface = "bold") #font
dev.off()

# save the venn diagrams ####
dev.off()
native_status_VD_n_RvW
ggsave("native_status_VD_n_RvW.png", width = 1, height = 1,
       units = "in", dpi = 600, plot = native_status_VD_n_RvW,
       path = figure_path, family = "Arial")

dev.off()
nesting_level_VD_n_RvW
ggsave("nesting_level_VD_n_RvW.png", width = 1, height = 0.75,
       units = "in", dpi = 600, plot = nesting_level_VD_n_RvW,
       path = figure_path, family = "Arial")

dev.off()
nest_RB_VD_n_RvW
ggsave("nest_RB_VD_n_RvW.png", width = 1, height = 1,
       units = "in", dpi = 600, plot = nest_RB_VD_n_RvW,
       path = figure_path, family = "Arial")

dev.off()
nest_substrate_VD_n_RvW
ggsave("nest_substrate_VD_n_RvW.png", width = 1, height = 1,
       units = "in", dpi = 600, plot = nest_substrate_VD_n_RvW,
       path = figure_path, family = "Arial")

dev.off()
diet_category_VD_n_RvW
ggsave("diet_category_VD_n_RvW.png", width = 1, height = 1,
       units = "in", dpi = 600, plot = diet_category_VD_n_RvW,
       path = figure_path, family = "Arial")

dev.off()
social_category_VD_n_RvW
ggsave("social_category_VD_n_RvW.png", width = 1, height = 1,
       units = "in", dpi = 600, plot = social_category_VD_n_RvW,
       path = figure_path, family = "Arial")

