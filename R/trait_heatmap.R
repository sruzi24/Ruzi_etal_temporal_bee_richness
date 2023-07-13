# to make a categorical trait heatmap


# load libraries
library(tidyverse)
library(here)
library(VennDiagram)
library(readxl)

# set relative pathways ####
data_path <- here::here("data")
figure_path <- here::here("figs")
Rsource_path <- here::here("R/Resources")

# read in data ####
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


# read in occurence records ####
# to get what time periods they are collected in

condensed_data <- read_csv(paste(data_path, "Dataset1.csv", sep="/"),
                           col_types = 
                             cols( 
                               .default = col_character()))
condensed_data

# when collected
historic_sampling <- c(seq(1900,1969,1))
mid_low_sampling <- c(seq(1970,1999,1))
more_contemporary <- c(seq(2000,2018,1))


historic_sampling_spp <- condensed_data %>%
  filter(year %in% historic_sampling) %>%
  count(scientific_name) %>%
  rename(year1900to1969 = n) 

mid_low_spp <- condensed_data %>%
  filter(year %in% mid_low_sampling) %>%
  count(scientific_name) %>%
  rename(year1970to1999 = n) 

more_contemporary_spp <- condensed_data %>%
  filter(year %in% more_contemporary) %>%
  count(scientific_name) %>%
  rename(year2000to2018 = n) 


family_spp_tibble <- condensed_data %>%
  count(family, scientific_name) %>%
  select(-n)

species_list_by_time_frame <- family_spp_tibble %>% 
  left_join(historic_sampling_spp) %>%
  left_join(mid_low_spp) %>%
  left_join(more_contemporary_spp)
species_list_by_time_frame

time_frame_subset <- species_list_by_time_frame %>%
  mutate(timeframe = ifelse(!is.na(year1900to1969) &
                              is.na(year2000to2018), "historic_only", NA)) %>%
  mutate(timeframe = ifelse(is.na(year1900to1969) &
                              !is.na(year2000to2018), "recent_only", timeframe)) %>%
  mutate(timeframe = ifelse(!is.na(year1900to1969) &
                              !is.na(year2000to2018), "both", timeframe)) %>%
  # remove unecessary columns
  select(-c(year1900to1969, year1970to1999, year2000to2018))

cat_data_timeframe_subset <- time_frame_subset %>%
  # remove the species that are not in the focal timeframes
  filter(!is.na(timeframe)) %>%
  # join in the trait data
  left_join(trait_data)
cat_data_timeframe_subset

# update variable names to make them easier to read in a figure ####

# need to have one column for the value and one column for the name of each of the variables and the factor within the variable
# so need to dummy code everything to make it easier since some species have more than one trait 



# dummy code the dataset ####
# 1 means that the species has that characteristic
# 0 means that the species does not have that characterisitc
# NA means that we do not have information for that species

dummy_categorical_data <- cat_data_timeframe_subset %>%
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

dummy_categorical_data %>%
  #filter(nest_substrate == "soil, stem, wood, cavity") %>%
  #count(nesting_level, nesting_level_above, nesting_level_below)
  #count(nesting_RentBuild, nestingRB_build, nestingRB_parasite, nestingRB_rent)
  #count(nest_substrate, nestSubstrate_cavity, nestSubstrate_exposed, nestSubstrate_soil, nestSubstrate_stem, nestSubstrate_wood)
  #count(diet_category, diet_category_generalist, diet_category_parasite, diet_category_specialist)
  count(social_category, social_category_parasite, social_category_parasite_social, social_category_polymorphic,
        social_category_social, social_category_solitary)

# how many species have information for each trait category ###
dummy_categorical_data %>%  filter(!is.na(native_status)) # 326
dummy_categorical_data %>%  filter(!is.na(nesting_level)) # 318
dummy_categorical_data %>%  filter(!is.na(nesting_RentBuild)) # 316
dummy_categorical_data %>%  filter(!is.na(nest_substrate)) # 315
dummy_categorical_data %>%  filter(!is.na(diet_category)) # 283
dummy_categorical_data %>%  filter(!is.na(social_category)) # 323




# to get the number of species that has each trait ####

names(dummy_categorical_data)


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


# to get the number of species that have data in each of the timeframes

historic_native_status <- dummy_categorical_data %>%  
  filter(timeframe == "historic_only") %>%
  filter(!is.na(native_status))

native_status_n <- dummy_categorical_data %>%
  group_by(timeframe) %>%
  count(native_status) %>%
  summarise(sum_species = sum(n)) %>%
  mutate(trait_category = "native_status")
native_status_n

nesting_level_n <- dummy_categorical_data %>%
  group_by(timeframe) %>%
  count(nesting_level) %>%
  filter(!is.na(nesting_level)) %>%
  summarise(sum_species = sum(n)) %>%
  mutate(trait_category = "nesting_level")
nesting_level_n

nesting_RB_n <- dummy_categorical_data %>%
  group_by(timeframe) %>%
  count(nesting_RentBuild) %>%
  filter(!is.na(nesting_RentBuild)) %>%
  summarise(sum_species = sum(n)) %>%
  mutate(trait_category = "nesting_RB")
nesting_RB_n

nesting_substrate_n <- dummy_categorical_data %>%
  group_by(timeframe) %>%
  count(nest_substrate) %>%
  filter(!is.na(nest_substrate)) %>%
  summarise(sum_species = sum(n)) %>%
  mutate(trait_category = "nest_substrate")
nesting_substrate_n

diet_category_n <- dummy_categorical_data %>%
  group_by(timeframe) %>%
  count(diet_category) %>%
  filter(!is.na(diet_category)) %>%
  summarise(sum_species = sum(n)) %>%
  mutate(trait_category = "diet_category")
diet_category_n

social_category_n <- dummy_categorical_data %>%
  group_by(timeframe) %>%
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

heatmap_data <- dummy_categorical_data %>%
  pivot_longer(cols = trait_names, names_to = "categorical_trait", values_to = "value") %>%
  # only keep certain columns
  select(scientific_name, family, timeframe, categorical_trait, value) %>%
  # filter so only the traits that are represented are present
  filter(value == 1) %>%
  # to get a count of how many of each categorical trait by timeperiod
  count(timeframe, categorical_trait) %>%
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
  mutate(order = ifelse(timeframe == "historic_only", 1, NA),
         order = ifelse(timeframe == "both", 2, order),
         order = ifelse(timeframe == "recent_only", 3, order)) %>%
  # set up an alternative order for plotting
  mutate(order2 = ifelse(timeframe == "historic_only", 2, NA),
         order2 = ifelse(timeframe == "both", 1, order2),
         order2 = ifelse(timeframe == "recent_only", 3, order2)) %>%
  # set it up so that the label color changes based on the proportion
  mutate(label_color = ifelse(prop >= 0.55, "white", "black"))
heatmap_data



heatmap_data %>%
  ggplot(aes(x = reorder(timeframe, order), y = categorical_trait)) +
  geom_tile(aes(fill = prop)) +
  scale_fill_viridis_c(begin = 1, end = 0) +  #option = "magma"
  #scale_fill_gradient2(low = "darkslategray1", mid = "yellow", high = "turquoise4")
  xlab("Time frame") +
  ylab("Categorical trait") +
  geom_hline(yintercept = 3.5) + 
  geom_hline(yintercept = 6.5) + 
  geom_hline(yintercept = 8.5) + 
  geom_hline(yintercept = 11.5) + 
  geom_hline(yintercept = 16.5) 

heatmap_plot <- heatmap_data %>%
  ggplot(aes(x = reorder(timeframe, order2), y = categorical_trait)) +
  geom_tile(aes(fill = prop)) +
  scale_fill_viridis_c(begin = 1, end = 0, name = "Proportion") +  #option = "magma"
  #scale_fill_gradient2(low = "darkslategray1", mid = "yellow", high = "turquoise4")
  xlab("Time frame") +
  ylab("Categorical trait") +
  geom_text(aes(label = round(prop, 2), color = label_color), size = 3) +
  scale_color_manual(breaks = c("white", "black"),
                     values = c("white", "black")) +
  guides(color = "none") +
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
      "historic_only" = "historic\nonly",
      "both" = "both",
      "recent_only" = "recent\nonly")) +
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

#ggsave("heatmap_plot.png", width = 5, height = 8,
#       units = "in", dpi = 600, plot = heatmap_plot,
#       path = figure_path, family = "Arial")


# create venn diagrams to show sample size ####
# different venn diagram for each trait category
#- to show how many species have that trait information in each time frame instead of using a table
# - using the same colors as the venn diagram of when species were collected
joined_samples
 

# -- native status
#1 both                   92 native_status  
#2 historic_only         213 native_status  
#3 recent_only            21 native_status 

dev.off()
native_status_VD_n <- draw.pairwise.venn(area1 = 213+92, area2 = 21+92, 
                 cross.area = 92,
                 #category = c("1900-\n1969", "2000-\n2018"),
                 fill = c("#D55E00", "#0072B2"),
                 cex = 1, cat.cex = 1, ext.txt = TRUE,
                 ext.percent = c(.3,.3,.3), #cat.dist = .15,
                 ext.dist = 0.05,
                 cat.fontface = "bold") #font
dev.off()


# -- nesting level
#4 both                   91 nesting_level  
#5 historic_only         206 nesting_level  
#6 recent_only            21 nesting_level  

dev.off()
nesting_level_VD_n <- draw.pairwise.venn(area1 = 206+91, area2 = 21+91, 
                                         cross.area = 91,
                                         #category = c("1900-\n1969", "2000-\n2018"),
                                         fill = c("#D55E00", "#0072B2"),
                                         cex = 1, cat.cex = 1, ext.txt = TRUE,
                                         ext.percent = c(.3,.3,.3), #cat.dist = .15,
                                         ext.dist = 0.05,
                                         cat.fontface = "bold")
dev.off()

# -- nesting rent or build
#7 both                   89 nesting_RB     
#8 historic_only         206 nesting_RB     
#9 recent_only            21 nesting_RB   

dev.off()
nest_RB_VD_n <- draw.pairwise.venn(area1 = 206+89, area2 = 21+89, 
                                         cross.area = 89,
                                         #category = c("1900-\n1969", "2000-\n2018"),
                                         fill = c("#D55E00", "#0072B2"),
                                   cex = 1, cat.cex = 1, ext.txt = TRUE,
                                   ext.percent = c(.3,.3,.3), #cat.dist = .15,
                                   ext.dist = 0.05,
                                   cat.fontface = "bold")
dev.off()

# -- nest substrate
#10 both                   91 nest_substrate 
#11 historic_only         203 nest_substrate 
#12 recent_only            21 nest_substrate

dev.off()
nest_substrate_VD_n <- draw.pairwise.venn(area1 = 203+91, area2 = 21+91, 
                                         cross.area = 91,
                                         #category = c("1900-\n1969", "2000-\n2018"),
                                         fill = c("#D55E00", "#0072B2"),
                                         cex = 1, cat.cex = 1, ext.txt = TRUE,
                                         ext.percent = c(.3,.3,.3), #cat.dist = .15,
                                         ext.dist = 0.05,
                                         cat.fontface = "bold")
dev.off()

# -- diet category
#13 both                   87 diet_category  
#14 historic_only         178 diet_category  
#15 recent_only            18 diet_category  

dev.off()
diet_category_VD_n <- draw.pairwise.venn(area1 = 178+87, area2 = 18+87, 
                                         cross.area = 87,
                                         #category = c("1900-\n1969", "2000-\n2018"),
                                         fill = c("#D55E00", "#0072B2"),
                                         cex = 1, cat.cex = 1, ext.txt = TRUE,
                                         ext.percent = c(.3,.3,.3), #cat.dist = .15,
                                         ext.dist = 0.05,
                                         cat.fontface = "bold")
dev.off()

# -- social category
#16 both                   91 social_category
#17 historic_only         212 social_category
#18 recent_only            20 social_category

dev.off()
social_category_VD_n <- draw.pairwise.venn(area1 = 212+91, area2 = 20+91, 
                                         cross.area = 91,
                                         #category = c("1900-\n1969", "2000-\n2018"),
                                         fill = c("#D55E00", "#0072B2"),
                                         cex = 1, cat.cex = 1, ext.txt = TRUE,
                                         ext.percent = c(.3,.3,.3), #cat.dist = .15,
                                         ext.dist = 0.05,
                                         cat.fontface = "bold")
dev.off()

# save the venn diagrams ####
dev.off()
native_status_VD_n
ggsave("native_status_VD_n.png", width = 1, height = 1,
       units = "in", dpi = 600, plot = native_status_VD_n,
       path = figure_path, family = "Arial")

dev.off()
nesting_level_VD_n
ggsave("nesting_level_VD_n.png", width = 1, height = 0.75,
       units = "in", dpi = 600, plot = nesting_level_VD_n,
       path = figure_path, family = "Arial")

dev.off()
nest_RB_VD_n
ggsave("nest_RB_VD_n.png", width = 1, height = 1,
       units = "in", dpi = 600, plot = nest_RB_VD_n,
       path = figure_path, family = "Arial")

dev.off()
nest_substrate_VD_n
ggsave("nest_substrate_VD_n.png", width = 1, height = 1,
       units = "in", dpi = 600, plot = nest_substrate_VD_n,
       path = figure_path, family = "Arial")

dev.off()
diet_category_VD_n
ggsave("diet_category_VD_n.png", width = 1, height = 1,
       units = "in", dpi = 600, plot = diet_category_VD_n,
       path = figure_path, family = "Arial")

dev.off()
social_category_VD_n
ggsave("social_category_VD_n.png", width = 1, height = 1,
       units = "in", dpi = 600, plot = social_category_VD_n,
       path = figure_path, family = "Arial")
