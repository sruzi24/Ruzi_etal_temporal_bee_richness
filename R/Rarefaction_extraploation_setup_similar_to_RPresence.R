# rarefaction and extrapolation curves with data set up similarly to 
# how entered for RPresence

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

# read in the file used for RPresence ####

presence_raw_data <- read_csv(paste(data_path, "2023_Rpresence_bee_detection_histories.csv", sep="/"),
                              col_types = 
                                cols(
                                  .default = col_double(),
                                  scientific_name = col_character(),
                                  family = col_character()))
presence_raw_data

# set the focal periods and data to work with iNEXT####
# need to fill in all the NAs with zero
presence_raw_data[ is.na( presence_raw_data ) ] <- 0
presence_raw_data

presence_raw_data2 <- presence_raw_data %>%
  select(-family)

name_list <- names(presence_raw_data2)[2:ncol(presence_raw_data2)]
name_list

focal_periods <- c(seq(1909, 1913, 1),
                   seq(1924, 1928, 1),
                   seq(1939, 1943, 1),
                   seq(1954, 1958, 1),
                   seq(1969, 1973, 1),
                   seq(1984, 1988, 1),
                   seq(1999, 2003, 1),
                   seq(2014, 2018, 1))
focal_periods

iNext_raw_data <- presence_raw_data2 %>%
  pivot_longer(name_list, names_to = "year") %>%
  filter(year %in% focal_periods)
iNext_raw_data

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

iNEXT_period1 <- iNext_raw_data %>%
  filter(year %in% period1) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  select(-scientific_name)
iNEXT_period1
ncol(iNEXT_period1) #5 samples

iNEXT_period2 <- iNext_raw_data %>%
  filter(year %in% period2) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  select(-scientific_name)
iNEXT_period2
ncol(iNEXT_period2) #5 samples

iNEXT_period3 <- iNext_raw_data %>%
  filter(year %in% period3) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  select(-scientific_name)
iNEXT_period3
ncol(iNEXT_period3) #5 samples

iNEXT_period4 <- iNext_raw_data %>%
  filter(year %in% period4) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  select(-scientific_name)
iNEXT_period4
ncol(iNEXT_period4) #5 samples

iNEXT_period5 <- iNext_raw_data %>%
  filter(year %in% period5) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  select(-scientific_name)
iNEXT_period5
ncol(iNEXT_period5) #5 samples

iNEXT_period6 <- iNext_raw_data %>%
  filter(year %in% period6) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  select(-scientific_name)
iNEXT_period6
ncol(iNEXT_period6) #4 samples

iNEXT_period7 <- iNext_raw_data %>%
  filter(year %in% period7) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  select(-scientific_name)
iNEXT_period7
ncol(iNEXT_period7) #1 sample

iNEXT_period8 <- iNext_raw_data %>%
  filter(year %in% period8) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  select(-scientific_name)
iNEXT_period8
ncol(iNEXT_period8) #5 samples

iNEXT_indicence_raw_data <- list(season1 = as.matrix(iNEXT_period1),
                                 season2 = as.matrix(iNEXT_period2),
                                 season3 = as.matrix(iNEXT_period3),
                                 season4 = as.matrix(iNEXT_period4),
                                 season5 = as.matrix(iNEXT_period5),
                                 season6 = as.matrix(iNEXT_period6), 
                                 #period7 = as.matrix(iNEXT_period7), # only has 1 sample so needs to be removed
                                 season8 = as.matrix(iNEXT_period8)) 

iNEXT_indicence_raw_data


# run iNEXT ####

#inext_rpresence_setup <- iNEXT(iNEXT_indicence_raw_data, q=c(0), datatype = "incidence_raw")
#inext_rpresence_setup

## -- save the output
#save(inext_rpresence_setup, 
#     file = paste(Rsource_path, "iNEXT_output_setup_like_PRESENCE.rda", sep = "/"))

load(paste(Rsource_path,"iNEXT_output_setup_like_PRESENCE.rda", sep = "/"))

inext_rpresence_setup

ggiNEXT(inext_rpresence_setup, type=1, se=TRUE, grey=FALSE)
ggiNEXT(inext_rpresence_setup, type=2, se=TRUE, grey=FALSE)
ggiNEXT(inext_rpresence_setup, type=3, se=TRUE, grey=FALSE)


# following Box 1 in Chao et al. 2014 ####
inext_rpresence_setup$DataInfo
#     site T   U S.obs     SC Q1 Q2 Q3 Q4 Q5 Q6 Q7 Q8 Q9 Q10
#1 season1 5  32    26 0.4008 21  4  1  0  0  0  0  0  0   0
#2 season2 5 270   139 0.7865 70 30 22 11  6  0  0  0  0   0
#3 season3 5 210   107 0.8303 47 30 18 11  1  0  0  0  0   0
#4 season4 5 281   132 0.8223 61 27 19 16  9  0  0  0  0   0
#5 season5 5  18    13 0.5714  9  3  1  0  0  0  0  0  0   0
#6 season6 4  26    21 0.4150 17  3  1  0  0  0  0  0  0   0
#7 season8 5 241   106 0.8976 37 37 11  8 13  0  0  0  0   0

# so the lowest SC (sample coverage) is period 1 at 40%,
# and then period 6 at 41.5%

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
#     For us this is Ca = max{0.4008, 0.7865, 0.8303, 0.8223, 0.5714, 0.4150, 0.8976} = 0.8976
#     Step 2. Compute the minimum coverage of r times reference sample sizes, Cb = min{C(n1),C(n2)...,C(nk)}
#     For us this is Cb = min{0.544, 0.881, 0.926, 0.902, 0.730, 0.625, 0.970} = 0.544
#     Step 3. The suggested base coverage is the maximum of Ca and Cb, Cbase = max{Ca,Cb}
#     For us this is Cbase = max{0.8976, 0.544} = 0.8976

# to get the r times the reference sample sizes
sample_size_8_coverage <- estimateD(iNEXT_indicence_raw_data, datatype = "incidence_raw", base = "size",
                                    level = 8, conf = 0.95)
sample_size_8_coverage %>%
  filter(order == "0")
#     site t       method order    SC      qD  qD.LCL  qD.UCL
#1 season1 8 extrapolated     0 0.544  36.533  24.850  48.216
#2 season2 8 extrapolated     0 0.881 167.843 152.807 182.880
#3 season3 8 extrapolated     0 0.926 123.623 111.576 135.669
#4 season4 8 extrapolated     0 0.902 156.865 140.726 173.005
#5 season5 8 extrapolated     0 0.730  16.999  11.111  22.887
#6 season6 8 extrapolated     0 0.625  33.973  21.257  46.689
#7 season8 8 extrapolated     0 0.970 116.415 105.762 127.068

# generate and save the recommended sample coverage output ####
#sample_coverage_base_0.8976 <- estimateD(iNEXT_indicence_raw_data, datatype = "incidence_raw", base = "coverage",
#                                                level = 0.8976, conf = 0.95)
# gives some warnings because of over extrapolation


# save output for now so do not need to rerun that and wait for it
## -- save the estimateD output
#save(sample_coverage_base_0.8976, 
#     file = paste(Rsource_path, "sample_coverage_0.8976_iNEXT_output_setup_like_PRESENCE.rda", sep = "/"))

# plot data ####
load(paste(Rsource_path,"sample_coverage_0.8976_iNEXT_output_setup_like_PRESENCE.rda", sep = "/"))

sample_coverage_base_0.8976 %>%
  filter(order == "0")
#     site  t       method order    SC      qD  qD.LCL  qD.UCL
#1 season1 24 extrapolated     0 0.894  62.270  31.947  92.593
#2 season2  9 extrapolated     0 0.902 174.283 155.665 192.901
#3 season3  7 extrapolated     0 0.903 119.528 107.730 131.325
#4 season4  8 extrapolated     0 0.902 156.865 140.727 173.004
#5 season5 14 extrapolated     0 0.893  21.103  11.167  31.038
#6 season6 20 extrapolated     0 0.901  51.031  21.901  80.160
#7 season8  3 interpolated     0 0.770  87.500  81.844  93.156

# for easier plotting need to generate values out to the max t which is 24

#max_t <- iNEXT(iNEXT_indicence_raw_data, q=c(0), datatype = "incidence_raw",
#               endpoint = 24)

# save output for now so do not need to rerun that and wait for it
## -- save the output
#save(max_t, 
#     file = paste(Rsource_path, "sample_coverage_iNEXT_output_setup_like_PRESENCE_extended.rda", sep = "/"))

load(paste(Rsource_path,"sample_coverage_iNEXT_output_setup_like_PRESENCE_extended.rda", sep = "/"))

PRESENCE_rarefaction_sample_coverage_by_diversity_0.8976 <- ggiNEXT(max_t, type=3, se=TRUE, grey=FALSE)+
  #facet_wrap(.~site, scales = "free_y" )+
  geom_vline(xintercept = 0.8976, linetype="dashed", 
             color = "black") +
  geom_vline(xintercept = 0.544, linetype="dotted", 
             color = "black") +
  ylab("Species Richness (q = 0)") +
  annotate("text", x = 0.80, y = 240, label = "0.8976") +
  annotate("text", x = 0.45, y = 240, label = "0.544") +
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
PRESENCE_rarefaction_sample_coverage_by_diversity_0.8976

# save figure ####
#ggsave("PRESENCE_rarefaction_sample_coverage_by_diversity_0.8976.png", width = 6.13, height = 4,
#       units = "in", dpi = 600, plot = PRESENCE_rarefaction_sample_coverage_by_diversity_0.8976,
#       path = figure_path)

# run some correlations between time and inext ####

ggplot(sample_coverage_base_0.8976, aes(x = site, y = qD, color = order, shape = method))+
  geom_point(stat = "identity")
sample_coverage_base_0.8976


# - correlation of the 0.8976 estimates with time period
sample_coverage_base_0.8976_2 <- sample_coverage_base_0.8976 %>%
  mutate(period = str_extract(site, regex("
                             \\d # a digit
                             ", comments = TRUE))) %>%
  mutate(period = as.numeric(period)) %>%
  filter(order == "0")

y_rarefaction_PRESENCE <- sample_coverage_base_0.8976_2$qD
x_period <- sample_coverage_base_0.8976_2$period

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
