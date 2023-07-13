# rarefaction and extrapolation curves with data set up similarly to 
# how entered for SPECRICH2 annual estimates

# load libraries ####
library(here)
library(tidyverse)
library(iNEXT)

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
                               .default = col_character(),
                               decimal_latitude = col_double(),
                               decimal_longitude = col_double()))
condensed_data

num_species_per_year <- condensed_data %>%
  count(scientific_name, year) %>%
  count(year) %>%
  rename(num_species = n)
num_species_per_year

num_unique_specimens <- condensed_data %>%
  count(year) %>%
  rename(num_specimens = n)
num_unique_specimens

# get a list of all the years we will need estimates for ####
# these are years that have at least 3 or more collection dates
collection_dates_per_year <- condensed_data %>%
  # condense down to collection dates
  count(day, month, year) %>%
  # get number of collection dates per year
  count(year) %>%
  # get only years with at least 3 collection dates
  filter(n >= 3)
collection_dates_per_year
collection_dates_per_year %>% print(n = 100)

year_list <- as.vector(collection_dates_per_year$year)

# get a tibble of all the 331 species ####
full_spp_list_tb <- condensed_data %>%
  count(scientific_name) %>%
  select(-n)
full_spp_list_tb

# set up the data to use with iNEXT and have it comparable to what was used for SPECRICH2 ####
condensed_data2 <- condensed_data %>%
  # remove the years we don't need information from
  filter(year %in% year_list) %>%
  select(-n)
condensed_data2

# initiate an empty list of the same length as the number of years
input_list <- vector(mode = "list", length = length(year_list))
names(input_list) <- year_list
input_list

for(i in 1:length(year_list)){
  temp_year = year_list[i]
  #print(temp_year)
  temp_tibble <- condensed_data %>%
    filter(year == temp_year) %>%
    # need to merge the day, month, year columns
    unite(event_date, day, c(day, month, year)) %>%
    count(scientific_name, event_date) %>%
    # make a new column of presence/non-presence
    mutate(n2 = if_else(!is.na(n), "1", "0")) %>%
    mutate(n2 = as.numeric(n2)) %>%
    # remove n so that won't get multiple species rows when pivot_wider
    select(-n) %>%
    pivot_wider(names_from = event_date, values_from = n2) %>%
    # each matrix needs to have all 331 species though
    full_join(full_spp_list_tb)
  # now need to fill all the NAs with 0's
  temp_tibble[is.na(temp_tibble)] <- 0
  # to work with iNEXT need to remove the scientific_name column
  # and make it into a matrix when putting it into the list
  temp_tibble2 <- temp_tibble %>%
    select(-scientific_name)
  #print(temp_tibble2)
  # neet to put the tibble created into the list and make sure it is a matrix
  input_list[[i]] <- as.matrix(temp_tibble2)
}

input_list

# generate iNEXT results using all the data ####
# will need to subset for graphing though

# commenting the following out since will be saving the output to an Rdata file
#all_data_iNEXT_output <- iNEXT(input_list, q = c(0), datatype = "incidence_raw")


# save output for now so do not need to rerun that and wait for it
## -- save the rarefaction output
#save(all_data_iNEXT_output, 
#     file = paste(Rsource_path, "all_data_iNEXT_output_setup_like_SPECRICH2.rda", sep = "/"))


load(paste(Rsource_path,"all_data_iNEXT_output_setup_like_SPECRICH2.rda", sep = "/"))

all_data_iNEXT_output


my_theme <- theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
                  panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
                  panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
                  panel.border=element_rect(colour="black", fill=NA),
                  #panel.border=element_blank(), #gets rid of square going around the entire graph
                  #axis.line = element_line(colour = 'black', size = 0.5),#sets the axis line size
                  axis.ticks=element_line(colour = 'black', size = 0.5), #sets the tick lines
                  #legend.position=c(0.10,0.25), # moves the location of the legend
                  legend.position="none",
                  #legend.title=element_blank(),
                  #legend.text=element_text(face="bold"),
                  #legend.background=element_blank(), # removes the overall border
                  #legend.background=element_rect(fill="white", colour="black"), #puts a black box around the legend
                  legend.key=element_blank(), #remove the border around each item
                  axis.title.x = element_text(face="bold", size=14, color="black"), #size of x-axis title
                  axis.title.y = element_text(face="bold", size=14, color="black"), #size of y-axis title
                  axis.text.x = element_text(size=12, color="black"), #size of x-axis text
                  axis.text.y = element_text(size=12, color="black"))#size of y-axis text

# make some general plots and save them since they are too large to view in R ####
iNEXT_SPECRICH2_sample_by_diversity <- ggiNEXT(all_data_iNEXT_output, type=1, se=TRUE, grey=TRUE)+
  facet_wrap(.~site, scales = "free_x", ncol = 4)+
  my_theme

#ggsave("iNEXT_SPECRICH2_sample_by_diversity.png", width = 8, height = 40,
#       units = "in", dpi = 600, plot = iNEXT_SPECRICH2_sample_by_diversity,
#       path = figure_path)


iNEXT_SPECRICH2_sample_units_by_coverage <- ggiNEXT(all_data_iNEXT_output, type=2, se=TRUE, grey=TRUE)+
  facet_wrap(.~site, scales = "free_x", ncol = 4)+
  my_theme

#ggsave("iNEXT_SPECRICH2_sample_units_by_coverage.png", width = 8, height = 40,
#       units = "in", dpi = 600, plot = iNEXT_SPECRICH2_sample_units_by_coverage,
#       path = figure_path)


iNEXT_SPECRICH2_sample_coverage_by_diversity <- ggiNEXT(all_data_iNEXT_output, type=3, se=TRUE, grey=TRUE)+
  facet_wrap(.~site, scales = "free_x", ncol = 4)+
  my_theme

#ggsave("iNEXT_SPECRICH2_sample_coverage_by_diversity.png", width = 8, height = 40,
#       units = "in", dpi = 600, plot = iNEXT_SPECRICH2_sample_coverage_by_diversity,
#       path = figure_path)


specrich2_data_setup_trial_asmptote_estimates <- all_data_iNEXT_output$AsyEst

ggplot(specrich2_data_setup_trial_asmptote_estimates, aes(x = Site, y = Estimator, color = Diversity))+
  geom_point(stat = "identity") 

# to get the a standard sample size or sample coverage to compare across ####
all_data_iNEXT_output$DataInfo # too long to print (will need to use min and max I think to get the numbers interested in)

all_data_iNEXT_output_data_info <- all_data_iNEXT_output$DataInfo

#save the above table to a csv file so can make a table for the supplement

#write.csv(all_data_iNEXT_output_data_info, file = paste(data_path, "SPECRICH2_all_data_iNEXT_output_data_info.csv", sep="/"),
#          row.names = FALSE, na = "")

# when extrapolating for q = 0 (i.e. species richness) Chao et al. 2014
# recommends not extrapolating past 2 times the sample size (i.e. r = 2)
# --- so going to follow Box 1 in Chao et al.
# a) sample-sized-based rarefaction/extropolation
#     Step 1. Compute the maximum reference size, na = max {n1,n2,...,nk}
max(all_data_iNEXT_output_data_info$T) # 80
#     For us this is na = 80
#     Step 2. Compute the minimum r times reference sample sizes, nb = min{rn1,rn2, ..., rnk}
min(all_data_iNEXT_output_data_info$T) # 3
3 * 2 # 6
#     For us this is r = 2, nb  = 6
#     Step 3. The suggested base sample size is the maximum of na and nb, nbase = max{na,nb}
#     For us this is nbase = max{80,6} = 80

# b) Coverage-based rarefaction/extrapolation
#     Step 1. Compute the maximum coverage of reference sample sizes, Ca = max{C(n1),C(n2)...,C(nk)}
max(all_data_iNEXT_output_data_info$SC) # 1
#     For us this is Ca = 1
#     Step 2. Compute the minimum coverage of r times reference sample sizes, Cb = min{C(n1),C(n2)...,C(nk)}

sample_size_6_coverage <- estimateD(input_list, datatype = "incidence_raw", base = "size",
                                    level = 6, conf = 0.95)

trial_estimate <- sample_size_6_coverage %>%
  filter(order == "0")
min(trial_estimate$SC) #0.054
#     For us this is Cb = 
#     Step 3. The suggested base coverage is the maximum of Ca and Cb, Cbase = max{Ca,Cb}
#     For us this is Cbase = max{1, 0.054} = 1

## going to look at the median or mode sample coverage instead of the actual data
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


median(all_data_iNEXT_output_data_info$T) # 16
getmode(all_data_iNEXT_output_data_info$T) # 8
mean(all_data_iNEXT_output_data_info$T) # 20.17895 --> 20

median(all_data_iNEXT_output_data_info$SC) # 0.5529
getmode(all_data_iNEXT_output_data_info$SC) # 0.25
mean(all_data_iNEXT_output_data_info$SC) # 0.5301442 --> 0.53

# going to try with the mean sample size value instead
sample_size_20_coverage <- estimateD(input_list, datatype = "incidence_raw", base = "size",
                                     level = 20, conf = 0.95)

trial_estimate_2 <- sample_size_20_coverage %>%
  filter(order == "0")
min(trial_estimate_2$SC) # 0.176

# going to go with the mean sample coverage

#sample_coverage_0.53 <- estimateD(input_list, datatype = "incidence_raw", base = "coverage",
#                                     level = 0.53, conf = 0.95)

# save output for now so do not need to rerun that and wait for it
## -- save the estimateD output
#save(sample_coverage_0.53, 
#     file = paste(Rsource_path, "sample_coverage_0.53_estimateD_iNEXT_output_setup_like_SPECRICH2.rda", sep = "/"))

load(paste(Rsource_path,"sample_coverage_0.53_estimateD_iNEXT_output_setup_like_SPECRICH2.rda", sep = "/"))

sample_coverage_0.53

trial_estimate_3 <- sample_coverage_0.53 %>%
  filter(order == "0")

#save the above table to a csv file so can make a table for the supplement
#write.csv(trial_estimate_3, file = paste(data_path, "SPECRICH2_iNEXT_estimateD_sample_coverage_0_53_output_data_info.csv", sep="/"),
#          row.names = FALSE, na = "")

max(trial_estimate_3$t) # 92

# - correlation of the 0.53 estimates with year ####
correlation_data <- sample_coverage_0.53 %>%
  filter(order == "0") %>%
  mutate(year = as.numeric(str_extract(site, regex("
                             \\d{4} # 4 digits
                             ", comments = TRUE)))) #%>%

#mutate(year = as.numeric(site))
correlation_data

y_rarefaction_SPECRICH2 <- correlation_data$qD
x_year <- correlation_data$year

# spearman
cor.test(y_rarefaction_SPECRICH2, x_year, method = "spearman")
#Spearman's rank correlation rho
#
#data:  y_rarefaction_SPECRICH2 and x_year
#S = 173808, p-value = 0.03512
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho 
#-0.2164627 
#
#Warning message:
#In cor.test.default(y_rarefaction_SPECRICH2, x_year, method = "spearman") :
#  Cannot compute exact p-value with ties