# Specrich2 data

# load libraries ####
library(here)
library(tidyverse)
library(naniar)

# set relative pathways ####
data_path <- here::here("data")
figure_path <- here::here("figs")
Rsource_path <- here::here("R/Resources")

# any functions needed ####
`%nin%` = Negate(`%in%`) # will make it so that can filter by things that are not in 

# load in the SPECRICH2 estimates ####
specrich2_annual <- read_csv(paste(data_path, "SPECRICH2_annual_input_and_output_surveyCollectionDate.csv", sep="/"),
                             col_types = 
                               cols(
                                 year = col_double(),
                                 num_collection_events = col_double(),
                                 bootstrap_iterations = col_double(),
                                 seed_number = col_double(),
                                 `R(1)` = col_double(),
                                 `N(1)` = col_double(),
                                 `SE(N(1))` = col_double(),
                                 `chi-square` = col_double(),
                                 df = col_double(),
                                 prob = col_double(),
                                 output_file_saved = col_character(),
                                 notes = col_character()))
specrich2_annual

specrich2_spring <- read_csv(paste(data_path, "SPECRICH2_output_spring_surveyAsDMYYYY.csv", sep="/"),
                             col_types = 
                               cols(
                                 year = col_double(),
                                 num_total_collection_events = col_double(),
                                 bootstrap_iterations = col_double(),
                                 seed_number = col_double(),
                                 `R(1)` = col_double(),
                                 `N(1)` = col_double(),
                                 `SE(N(1))` = col_double(),
                                 `chi-square` = col_double(),
                                 df = col_double(),
                                 prob = col_double(),
                                 output_file_saved = col_character(),
                                 notes = col_character()))

specrich2_spring

specrich2_summer <- read_csv(paste(data_path, "SPECRICH2_output_summer_surveyAsDMYYYY.csv", sep="/"),
                             col_types = 
                               cols(
                                 year = col_double(),
                                 num_total_collection_events = col_double(),
                                 bootstrap_iterations = col_double(),
                                 seed_number = col_double(),
                                 `R(1)` = col_double(),
                                 `N(1)` = col_double(),
                                 `SE(N(1))` = col_double(),
                                 `chi-square` = col_double(),
                                 df = col_double(),
                                 prob = col_double(),
                                 output_file_saved = col_character(),
                                 notes = col_character()))

specrich2_summer

specrich2_fall <- read_csv(paste(data_path, "SPECRICH2_output_fall_surveyAsDMYYYY.csv", sep="/"),
                           col_types = 
                             cols(
                               year = col_double(),
                               num_total_collection_events = col_double(),
                               bootstrap_iterations = col_double(),
                               seed_number = col_double(),
                               `R(1)` = col_double(),
                               `N(1)` = col_double(),
                               `SE(N(1))` = col_double(),
                               `chi-square` = col_double(),
                               df = col_double(),
                               prob = col_double(),
                               output_file_saved = col_character(),
                               notes = col_character()))

specrich2_fall

# clean and merge estimates ####
specrich2_annual2 <- specrich2_annual %>%
  # remove years that we could not run SPECRICH2 with
  filter(num_collection_events >= 3) %>%
  #rename a column so that can match up with other datasets easier
  rename(num_total_collection_events = num_collection_events) %>%
  # fix some of the column names
  rename(richness_estimate = 'N(1)') %>%
  rename(richness_observed = 'R(1)') %>%
  rename(se_richness_estimate = 'SE(N(1))') %>%
  # make a column that determines if the GOF rejects null or not
  mutate(reject_Mh = if_else(prob <= 0.05, "p =< 0.05", "p > 0.05")) %>%
  #make a column that says what portion of the year data went into this analysis
  mutate(seasonality = "Annual")
specrich2_annual2

specrich2_spring2 <- specrich2_spring %>%
  # remove years that we could not run SPECRICH2 with
  filter(num_total_collection_events >= 3) %>%
  # fix some of the column names
  rename(richness_estimate = 'N(1)') %>%
  rename(richness_observed = 'R(1)') %>%
  rename(se_richness_estimate = 'SE(N(1))') %>%
  # make a column that determines if the GOF rejects null or not
  mutate(reject_Mh = if_else(prob <= 0.05, "p =< 0.05", "p > 0.05")) %>%
  #make a column that says what portion of the year data went into this analysis
  mutate(seasonality = "Spring")
specrich2_spring2



specrich2_summer2 <- specrich2_summer  %>%
  # remove years that we could not run SPECRICH2 with
  filter(num_total_collection_events >= 3) %>%
  # fix some of the column names
  rename(richness_estimate = 'N(1)') %>%
  rename(richness_observed = 'R(1)') %>%
  rename(se_richness_estimate = 'SE(N(1))') %>%
  # make a column that determines if the GOF rejects null or not
  mutate(reject_Mh = if_else(prob <= 0.05, "p =< 0.05", "p > 0.05")) %>%
  #make a column that says what portion of the year data went into this analysis
  mutate(seasonality = "Summer")
specrich2_summer2

specrich2_fall2 <- specrich2_fall  %>%
  # remove years that we could not run SPECRICH2 with
  filter(num_total_collection_events >= 3) %>%
  # fix some of the column names
  rename(richness_estimate = 'N(1)') %>%
  rename(richness_observed = 'R(1)') %>%
  rename(se_richness_estimate = 'SE(N(1))') %>%
  # make a column that determines if the GOF rejects null or not
  mutate(reject_Mh = if_else(prob <= 0.05, "p =< 0.05", "p > 0.05")) %>%
  #make a column that says what portion of the year data went into this analysis
  mutate(seasonality = "Fall")
specrich2_fall2

# merge all the files together

specrich2_seasonality_graphing <- specrich2_annual2 %>%
  full_join(specrich2_spring2) %>%
  full_join(specrich2_summer2) %>%
  full_join(specrich2_fall2)
specrich2_seasonality_graphing

# checking row lengths to ensure merging was correct
nrow(specrich2_seasonality_graphing) # 272
nrow(specrich2_annual2) + nrow(specrich2_spring2) + nrow(specrich2_summer2) + nrow(specrich2_fall2) # 272

# annual SPECRICH2 correlation with time ####
#spearman correlation
names(specrich2_annual2)
cor.test(specrich2_annual2$richness_estimate, specrich2_annual2$year, method = "spearman")
#Spearman's rank correlation rho
#
#data:  specrich2_annual2$richness_estimate and specrich2_annual2$year
#S = 157508, p-value = 0.3235
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#       rho 
#-0.1023795 
#
#Warning message:
#In cor.test.default(specrich2_annual2$richness_estimate, specrich2_annual2$year,  :
#  Cannot compute exact p-value with ties
  
# graph the data ####
specrich2_seasonality_graphing$seasonality2 <- factor(specrich2_seasonality_graphing$seasonality,
                                                      levels = c("Annual", "Spring", "Summer", "Fall"))



specrich2_seasonality_graph_color_blind_wrapped <- ggplot(data = specrich2_seasonality_graphing,
                                                          aes(x = year, y = richness_estimate, 
                                                              color = seasonality2,
                                                              shape = seasonality2)) +
  geom_point(size =2) +
  facet_wrap(~seasonality2, ncol = 1) + # scales = "free_y"
  #geom_point(size = 2, color = "black", shape = 15) +
  #geom_point(aes(x = year, y = richness_observed), color = "blue", shape = 8) +
  #geom_point(data = occupancy_estimates, aes(x = pp_plotting, y = spp_rich), color = "red",
  # size = 2) +
  #geom_line()+
  ylab("Species Richness") +
  xlab("Year") +
  scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  scale_y_continuous(expand = c(0,0), breaks=seq(0, 300, 25)) +
  labs(color = "Data",
       shape = "Data") +
  #annotate("text", x=1990, y=60,
  #         label=paste('Spearman\'s rho =', round(initial_spearman_cor_value,3)),
  #         size = 5)+
  #scale_color_manual(values = c("grey30", "grey80"),
  #                   breaks = c("p =< 0.05", "p > 0.05")) +
  #geom_errorbar(data = occupancy_estimates, aes(x = pp_plotting, ymin=spp_rich_lower_0.95,
  #                  ymax=spp_rich_upper_0.95), width=.2, color = "blue")+
  geom_errorbar(data = specrich2_seasonality_graphing, aes(ymin=richness_estimate - se_richness_estimate,
                                                           ymax=richness_estimate + se_richness_estimate, color = seasonality2), width=.2)+ #, color = "grey60"
  #geom_segment(data = occupancy_estimates, aes(x = pp_plotting, xend = pp_plotting, y = spp_rich_lower_0.95,
  #                                             yend = spp_rich_upper_0.95),
  #             color = "red") +
  #geom_segment(data = occupancy_estimates, aes(x = pp_plotting - 2.5, xend = pp_plotting + 2.5, y = spp_rich,
  #                                             yend = spp_rich),
  #             color = "red") +
  scale_color_manual(breaks = c("Annual", "Spring", "Summer", "Fall"),
                     values = c("black", "#CC79A7", "#56B409", "#009E73")) +
  scale_shape_manual(breaks = c("Annual", "Spring", "Summer", "Fall"),
                     values = c(15, 16, 17, 18)) +
  theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
        #panel.grid.mzeajor=element_blank(), #gets rid of grey and lines in the middle
        panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
        panel.border=element_blank(), #gets rid of square going around the entire graph
        axis.line = element_line(colour = 'black', size = 0.5),#sets the axis line size
        axis.ticks=element_line(colour = 'black', size = 0.5), #sets the tick lines
        legend.background=element_blank(), # removes the overall border
        legend.key=element_blank(), #remove the border around each item
        legend.position = "top",
        #legend.title = element_blank(),
        axis.title.x = element_text(face="bold", size=14, color="black"), #size of x-axis title
        axis.title.y = element_text(face="bold", size=14, color="black"), #si of y-axis title
        axis.text.x = element_text(size=10, color="black"), #size of x-axis text
        axis.text.y = element_text(size=10, color="black"))#size of y-axis text
specrich2_seasonality_graph_color_blind_wrapped

GOF_seasonality2_wrapped <- ggplot(data = specrich2_seasonality_graphing, aes(x = year, y = richness_estimate, 
                                                                              color = seasonality2,
                                                                              shape = seasonality2,
                                                                              alpha = reject_Mh)) +
  geom_point(size =2) +
  facet_wrap(~seasonality2, ncol = 1) + # scales = "free_y"
  #geom_point(size = 2, color = "black", shape = 15) +
  #geom_point(aes(x = year, y = richness_observed), color = "blue", shape = 8) +
  #geom_point(data = occupancy_estimates, aes(x = pp_plotting, y = spp_rich), color = "red",
  # size = 2) +
  #geom_line()+
  ylab("Species Richness") +
  xlab("Year") +
  scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  scale_y_continuous(expand = c(0,0), breaks=seq(0, 300, 25)) +
  labs(alpha = "M(h) GOF Test",
       color = "Data",
       shape = "Data") +
  #annotate("text", x=1990, y=60,
  #         label=paste('Spearman\'s rho =', round(initial_spearman_cor_value,3)),
  #         size = 5)+
  #scale_color_manual(values = c("grey30", "grey80"),
  #                   breaks = c("p =< 0.05", "p > 0.05")) +
  #geom_errorbar(data = occupancy_estimates, aes(x = pp_plotting, ymin=spp_rich_lower_0.95,
  #                  ymax=spp_rich_upper_0.95), width=.2, color = "blue")+
  geom_errorbar(data = specrich2_seasonality_graphing, aes(ymin=richness_estimate - se_richness_estimate,
                                                           ymax=richness_estimate + se_richness_estimate, color = seasonality), width=.2)+ #, color = "grey60"
  #geom_segment(data = occupancy_estimates, aes(x = pp_plotting, xend = pp_plotting, y = spp_rich_lower_0.95,
  #                                             yend = spp_rich_upper_0.95),
  #             color = "red") +
  #geom_segment(data = occupancy_estimates, aes(x = pp_plotting - 2.5, xend = pp_plotting + 2.5, y = spp_rich,
  #                                             yend = spp_rich),
  #             color = "red") +
  scale_color_manual(breaks = c("Annual", "Spring", "Summer", "Fall"),
                     values = c("black", "#CC79A7", "#56B409", "#009E73")) +
  scale_shape_manual(breaks = c("Annual", "Spring", "Summer", "Fall"), 
                     values = c(15, 16, 17, 18)) +
  scale_alpha_manual(values = c(1, .3),
                     breaks = c("p =< 0.05", "p > 0.05")) +
  guides(color = "none", shape = "none") +
  theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
        #panel.grid.mzeajor=element_blank(), #gets rid of grey and lines in the middle
        panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
        panel.border=element_blank(), #gets rid of square going around the entire graph
        axis.line = element_line(colour = 'black', size = 0.5),#sets the axis line size
        axis.ticks=element_line(colour = 'black', size = 0.5), #sets the tick lines
        legend.background=element_blank(), # removes the overall border
        legend.key=element_blank(), #remove the border around each item
        legend.position = "top",
        #legend.title = element_blank(),
        axis.title.x = element_text(face="bold", size=14, color="black"), #size of x-axis title
        axis.title.y = element_text(face="bold", size=14, color="black"), #si of y-axis title
        axis.text.x = element_text(size=10, color="black"), #size of x-axis text
        axis.text.y = element_text(size=10, color="black"))#size of y-axis text
GOF_seasonality2_wrapped

GOF_seasonality_num_samples_wrapped <- ggplot(data = specrich2_seasonality_graphing, aes(x = year, y = num_total_collection_events, 
                                                                                         color = seasonality2,
                                                                                         shape = seasonality2,
                                                                                         alpha = reject_Mh)) +
  geom_point(size =2) +
  facet_wrap(~seasonality2, ncol = 1) + # scales = "free_y"
  #geom_point(size = 2, color = "black", shape = 15) +
  #geom_point(aes(x = year, y = richness_observed), color = "blue", shape = 8) +
  #geom_point(data = occupancy_estimates, aes(x = pp_plotting, y = spp_rich), color = "red",
  # size = 2) +
  #geom_line()+
  ylab("Num. of Collection Dates") +
  xlab("Year") +
  scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  scale_y_continuous(expand = c(0,0), breaks=seq(0, 80, 10)) +
  labs(alpha = "M(h) GOF Test",
       color = "Data",
       shape = "Data") +
  #annotate("text", x=1990, y=60,
  #         label=paste('Spearman\'s rho =', round(initial_spearman_cor_value,3)),
  #         size = 5)+
  #scale_color_manual(values = c("grey30", "grey80"),
  #                   breaks = c("p =< 0.05", "p > 0.05")) +
  #geom_errorbar(data = occupancy_estimates, aes(x = pp_plotting, ymin=spp_rich_lower_0.95,
  #                  ymax=spp_rich_upper_0.95), width=.2, color = "blue")+
  #geom_errorbar(data = specrich2_seasonality_graphing, aes(ymin=richness_estimate - se_richness_estimate,
  #                                                         ymax=richness_estimate + se_richness_estimate, color = seasonality), width=.2)+ #, color = "grey60"
  #geom_segment(data = occupancy_estimates, aes(x = pp_plotting, xend = pp_plotting, y = spp_rich_lower_0.95,
  #                                             yend = spp_rich_upper_0.95),
#             color = "red") +
#geom_segment(data = occupancy_estimates, aes(x = pp_plotting - 2.5, xend = pp_plotting + 2.5, y = spp_rich,
#                                             yend = spp_rich),
#             color = "red") +
scale_color_manual(breaks = c("Annual", "Spring", "Summer", "Fall"),
                   values = c("black", "#CC79A7", "#56B409", "#009E73")) +
  scale_shape_manual(breaks = c("Annual", "Spring", "Summer", "Fall"), 
                     values = c(15, 16, 17, 18)) +
  scale_alpha_manual(values = c(1, .3),
                     breaks = c("p =< 0.05", "p > 0.05")) +
  guides(color = "none", shape = "none") +
  theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
        #panel.grid.mzeajor=element_blank(), #gets rid of grey and lines in the middle
        panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
        panel.border=element_blank(), #gets rid of square going around the entire graph
        axis.line = element_line(colour = 'black', size = 0.5),#sets the axis line size
        axis.ticks=element_line(colour = 'black', size = 0.5), #sets the tick lines
        legend.background=element_blank(), # removes the overall border
        legend.key=element_blank(), #remove the border around each item
        legend.position = "top",
        #legend.title = element_blank(),
        axis.title.x = element_text(face="bold", size=14, color="black"), #size of x-axis title
        axis.title.y = element_text(face="bold", size=14, color="black"), #si of y-axis title
        axis.text.x = element_text(size=12, color="black"), #size of x-axis text
        axis.text.y = element_text(size=12, color="black"))#size of y-axis text
GOF_seasonality_num_samples_wrapped

# saving the figures ####


#specrich2_seasonality_graph_color_blind_wrapped
#
#ggsave("appendix_specrich2_seasonality.png", width = 6, height = 8,
#       units = "in", dpi = 600, plot = specrich2_seasonality_graph_color_blind_wrapped,
#       path = figure_path, family = "Arial")
#
#GOF_seasonality2_wrapped
#
#ggsave("appendix_specrich2_seasonality_gof.png", width = 6, height = 8,
#       units = "in", dpi = 600, plot = GOF_seasonality2_wrapped,
#       path = figure_path, family = "Arial")
#
#
#GOF_seasonality_num_samples_wrapped
#
#ggsave("appendix_specrich2_seasonality_coll_dates.png", width = 6, height = 8,
#       units = "in", dpi = 600, plot = GOF_seasonality_num_samples_wrapped,
#       path = figure_path, family = "Arial")

# to make tables in the appendix ####
# - number of years that the gof test was rejected


specrich2_seasonality_graphing %>%
  count(seasonality2, num_total_collection_events) %>%
  arrange(num_total_collection_events) # making sure that there are no 1s and 2s which were not run in SPECRICH2
# lowest number of collection events (DDMMYYYY) is 3

specrich2_seasonality_graphing %>%
  count(seasonality2, reject_Mh)
# A tibble: 8 x 3
#seasonality2 reject_Mh     n
#<fct>        <chr>     <int>
#1 Annual       p =< 0.05    38
#2 Annual       p > 0.05     58
#3 Spring       p =< 0.05    31
#4 Spring       p > 0.05     33
#5 Summer       p =< 0.05    17
#6 Summer       p > 0.05     45
#7 Fall         p =< 0.05    10
#8 Fall         p > 0.05     40

# percent that reject Mh
#annual
38/(38+58)*100 # 39.58333

#spring
31/(31+33)*100 # 48.4375

#summer
17/(17+45)*100 # 27.41935

#fall
10/(10+40)*100 # 20

## conduct a test of whether reject M(h) based on number of collection events ####
# see if the number of collection dates differs between the tests that reject M(h) versus ones that don't
# using a mann-whitney u test

# independent 2-group Mann-Whitney U Test
#wilcox.test(y~A)
# where y is numeric and A is A binary factor

# annual
class(specrich2_annual2$reject_Mh)
specrich2_annual2$reject_Mh_factor <- as.factor(specrich2_annual2$reject_Mh)
str(specrich2_annual2$reject_Mh_factor)

wilcox.test(specrich2_annual2$num_total_collection_events~specrich2_annual2$reject_Mh_factor)
#Wilcoxon rank sum test with continuity correction
#
#data:  specrich2_annual2$num_total_collection_events by specrich2_annual2$reject_Mh_factor
#W = 2022, p-value = 5.372e-12
#alternative hypothesis: true location shift is not equal to 0


# spring
class(specrich2_spring2$reject_Mh)
specrich2_spring2$reject_Mh_factor <- as.factor(specrich2_spring2$reject_Mh)
str(specrich2_spring2$reject_Mh_factor)

wilcox.test(specrich2_spring2$num_total_collection_events~specrich2_spring2$reject_Mh_factor)
#Wilcoxon rank sum test with continuity correction
#
#data:  specrich2_spring2$num_total_collection_events by specrich2_spring2$reject_Mh_factor
#W = 864.5, p-value = 2.11e-06
#alternative hypothesis: true location shift is not equal to 0
#
#Warning message:
#  In wilcox.test.default(x = c(35, 37, 38, 23, 21, 10, 15, 15, 13,  :
#                                 cannot compute exact p-value with ties



# summer
class(specrich2_summer2$reject_Mh)
specrich2_summer2$reject_Mh_factor <- as.factor(specrich2_summer2$reject_Mh)
str(specrich2_summer2$reject_Mh_factor)

wilcox.test(specrich2_summer2$num_total_collection_events~specrich2_summer2$reject_Mh_factor)

#Wilcoxon rank sum test with continuity correction
#
#data:  specrich2_summer2$num_total_collection_events by specrich2_summer2$reject_Mh_factor
#W = 612, p-value = 0.0002804
#alternative hypothesis: true location shift is not equal to 0
#
#Warning message:
#  In wilcox.test.default(x = c(28, 26, 8, 9, 19, 3, 5, 7, 9, 6, 12,  :
#                                 cannot compute exact p-value with ties


# fall
class(specrich2_fall2$reject_Mh)
specrich2_fall2$reject_Mh_factor <- as.factor(specrich2_fall2$reject_Mh)
str(specrich2_fall2$reject_Mh_factor)

wilcox.test(specrich2_fall2$num_total_collection_events~specrich2_fall2$reject_Mh_factor)
#Wilcoxon rank sum test with continuity correction
#
#data:  specrich2_fall2$num_total_collection_events by specrich2_fall2$reject_Mh_factor
#W = 253.5, p-value = 0.1946
#alternative hypothesis: true location shift is not equal to 0
#
#Warning message:
#  In wilcox.test.default(x = c(5, 19, 5, 10, 7, 6, 6, 8, 11, 5), y = c(9,  :
#                                     cannot compute exact p-value with ties