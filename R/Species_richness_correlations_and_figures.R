# graphing the species richness trends and conducting correlations

# load libraries ####
library(here)
library(tidyverse)
library(naniar)
library(iNEXT)

# set relative pathways ####
data_path <- here::here("data")
figure_path <- here::here("figs")
Rsource_path <- here::here("R/Resources")

# any functions potentially needed ####
`%nin%` = Negate(`%in%`) # will make it so that can filter by things that are not in 
se <- function(x) sqrt(var(x)/length(x))

# load in annual specrich2 estimates ####
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

# remove certain years from the dataset before graphing
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

# load in the RPresence data ####
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

# load in the specimen data to get number of specimens per year ####
condensed_data <- read_csv(paste(data_path, "Dataset1.csv", sep="/"),
                           col_types = 
                             cols( 
                               .default = col_character()))
condensed_data

num_species_per_year <- condensed_data %>%
  count(scientific_name, year) %>%
  count(year) %>%
  rename(num_species = n) %>%
  mutate(year = as.numeric(year))
num_species_per_year

num_unique_specimens <- condensed_data %>%
  count(year) %>%
  rename(num_specimens = n)
num_unique_specimens

# load in the iNEXT results based on data set up like RPresence input ####

load(paste(Rsource_path,"sample_coverage_0.8976_iNEXT_output_setup_like_PRESENCE.rda", sep = "/"))

sample_coverage_base_0.8976 %>%
  filter(order == "0")

inext_plotting <- sample_coverage_base_0.8976 %>%
  filter(order == "0")
inext_plotting$pp_plotting <- c(1911,1926,1941,1956,1971,1986,2016)

load(paste(Rsource_path,"sample_coverage_iNEXT_output_setup_like_PRESENCE_extended.rda", sep = "/"))


# plot trend figure with all four methods ####

all_4_estimate_methods <- ggplot(data = specrich2_annual2, aes(x = year, y = richness_estimate)) +
  geom_point(data = occupancy_estimates, aes(pp_plotting, y = spp_rich), size = 2, color = "red") +
  geom_point(data = inext_plotting, aes(x = pp_plotting, y = qD), color = "purple", shape = 17, size = 2) +
  geom_point(data = num_species_per_year, aes(x = year, y = num_species), color = "blue", shape = 8) +
  geom_point(data = specrich2_annual2, aes(x = year, y = richness_estimate), size = 2, color = "black", shape = 15) +
  ylab("Species Richness") +
  xlab("Year") +
  scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  scale_y_continuous(expand = c(0,0), breaks=seq(0, 350, 25)) +
  geom_errorbar(data = specrich2_annual2, aes(ymin=richness_estimate - se_richness_estimate,
                                              ymax=richness_estimate + se_richness_estimate), width=.2, color = "grey60") +
  geom_segment(data = occupancy_estimates, aes(x = pp_plotting, xend = pp_plotting, y = spp_rich_lower_0.95,
                                               yend = spp_rich_upper_0.95),
               color = "red") +
  geom_segment(data = occupancy_estimates, aes(x = pp_plotting - 2.5, xend = pp_plotting + 2.5, y = spp_rich,
                                               yend = spp_rich),
               color = "red") +
  geom_segment(data = inext_plotting, aes(x = pp_plotting - 2.5, xend = pp_plotting + 2.5, y = qD,
                                          yend = qD),
               color = "purple") +
  geom_segment(data = inext_plotting, aes(x = pp_plotting, xend = pp_plotting, y = qD.LCL,
                                          yend = qD.UCL),
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
all_4_estimate_methods


# to determine which time period has the greatest species richness ####
# will use RPresence and iNEXT data set up like for PRESEENCE
# with 95% CI

occupancy_estimates$pp_code <- c(1:8)
occupancy_estimates$analysis_method <- "PRESENCE"
inext_plotting$pp_code <- c(1:6,8)

CI_plotting <- inext_plotting %>%
  mutate(analysis_method = "iNEXT") %>%
  # will need to rename some columns
  rename(spp_rich = qD) %>%
  rename(spp_rich_lower_0.95 = qD.LCL) %>%
  rename(spp_rich_upper_0.95 = qD.UCL) %>%
  # join the two datasets
  full_join(occupancy_estimates)

CI_plotting


obs_sample_coverage <- max_t$DataInfo %>%
  mutate(pp_code = c(1:6,8)) %>%
  mutate(analysis_method = "iNEXT") %>%
  mutate(order = c(2,3,4,8,6,1,5))

estimates_with_CI <- ggplot(data = CI_plotting, aes(x = reorder(pp_code, spp_rich), y = spp_rich, color = analysis_method, shape = analysis_method)) +
  geom_point(size = 3) +
  geom_errorbar(data = CI_plotting, aes(x = reorder(pp_code, spp_rich), 
                                        ymin = spp_rich_lower_0.95,
                                        ymax = spp_rich_upper_0.95,
                                        color = analysis_method), width = 0.3) +
  labs(y = "Species Richness", x = "Season",
       color = "", shape = "") +
  scale_color_manual(breaks = c("iNEXT", "PRESENCE"),
                     values = c("purple", "red"),
                     labels = c("Extrapolation", "Multi-season\nmodel")) +
  scale_shape_manual(breaks = c("iNEXT", "PRESENCE"), 
                     values = c(17, 16),
                     labels = c("Extrapolation", "Multi-season\nmodel")) +
  geom_vline(xintercept = 4.5, linetype="dotted", 
             color = "black") +
  geom_text(data = obs_sample_coverage, aes(x = reorder(pp_code, order) , y = 0), 
            label = round(obs_sample_coverage$SC,2), color = "black") + 
  #annotate("text", x = 8.5, y = 0, label = "SC", fontface = "bold") + # keeps getting cut off when I print it so will need to add it with powerpoint later
  coord_flip() +
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
        legend.text = element_text(size = 12),
        #legend.title = element_blank(),
        axis.title.x = element_text(face="bold", size=14, color="black"), #size of x-axis title
        axis.title.y = element_text(face="bold", size=14, color="black"), #size of y-axis title
        axis.text.x = element_text(size=12, color="black"), #size of x-axis text
        axis.text.y = element_text(size=12, color="black"))#size of y-axis text
estimates_with_CI

# correlations between different annual estimates ####
specrich2_graphing <- specrich2_annual2 %>%
  select(year, richness_estimate, se_richness_estimate) %>%
  rename(specrich = richness_estimate) %>%
  rename(specrich_se = se_richness_estimate)
specrich2_graphing

# get a list of all the years we will need estimates for
specrich2_graphing_year_list <- as.vector(specrich2_graphing$year)

# raw data
num_species_per_year


# need to load in the inext rarefaction estimates that are based on SPECIRCH2 input data (i.e. estimated annually)

load(paste(Rsource_path,"sample_coverage_0.53_estimateD_iNEXT_output_setup_like_SPECRICH2.rda", sep = "/"))

inext_rarefaction_estimates <- sample_coverage_0.53 %>%
  filter(order == "0")
inext_rarefaction_estimates


inext_rarefaction_estimates2 <- inext_rarefaction_estimates %>%
  rename(inext_est = qD) %>%
  rename(inext_lower = qD.LCL) %>%
  rename(inext_upper = qD.UCL) %>%
  mutate(year = as.numeric(as.character(site))) %>%
  select(year, inext_est, inext_lower, inext_upper)

# make a table that has all the values in it for easier graphing
annual_corr_tb <- num_species_per_year %>%
  rename(obs = num_species) %>%
  full_join(inext_rarefaction_estimates2) %>%
  full_join(specrich2_graphing)

# ---- make the annual correlation figures
names(annual_corr_tb)
# - inext & specrich 
specrich_rarefaction_annual_corr_plot <- ggplot(annual_corr_tb, aes(x = inext_est, y = specrich)) +
  geom_point(size = 1) +
  #geom_line()+
  xlab("Extrapolation Estimates") +
  ylab("SPECRICH2 Estimates") +
  scale_y_continuous(expand = c(0,0), breaks=seq(-25, 200, 25)) +
  scale_x_continuous(expand = c(0,0), breaks=seq(0, 200, 25)) +
  expand_limits(x = 0, y = 0) +
  coord_flip() +
  #scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  #scale_y_continuous(breaks=seq(0, 100, 10)) +
  geom_segment(data = annual_corr_tb, aes(x = inext_lower, xend = inext_upper, y = specrich,
                                          yend = specrich),
               color = "grey30") +
  geom_segment(data = annual_corr_tb, aes(x = inext_est, xend = inext_est, 
                                          y = specrich - specrich_se, yend = specrich + specrich_se),
               color = "grey30") +
  #geom_text(label = annual_corr_tb$year, fontface = "bold", size = 5) +
  geom_abline(intercept = c(0,0), slope = 1, linetype = "dashed") +
  geom_smooth(method='lm', formula= y~x, color = "grey30") +
  #annotate("text", x = 50, y = -5, label = paste("italic(t) ==", signif(plot2_test$statistic,3),
  #                                               sep = " "), parse = TRUE) +
  #annotate("text", x = 65, y = -5, label = paste("df =", plot2_test$parameter,
  #                                               sep = " ")) +
  #annotate("text", x = 82, y = -5, label = paste("italic(p) ==", signif(plot2_test$p.value,2),
  #                                               sep = " "), parse = TRUE) +
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
specrich_rarefaction_annual_corr_plot


# - inext & obs 

annual_rarefaction_observed_corr_plot <- ggplot(annual_corr_tb, aes(x = inext_est, y = obs)) +
  geom_point(size = 1) +
  #geom_line()+
  xlab("Extrapolation Estimates") +
  ylab("Observed Richness") +
  scale_y_continuous(expand = c(0,0), breaks=seq(-25, 200, 25)) +
  scale_x_continuous(expand = c(0,0), breaks=seq(0, 200, 25)) +
  expand_limits(x = 0, y = 0) +
  #scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  #scale_y_continuous(breaks=seq(0, 100, 10)) +
  geom_segment(data = annual_corr_tb, aes(x = inext_lower, xend = inext_upper, y = obs,
                                          yend = obs),
               color = "grey30") +
  #geom_segment(data = annual_corr_tb, aes(x = inext_est, xend = inext_est, 
  #                                        y = obs - obs_se, yend = obs + obs_se),
  #             color = "grey30") +
  #geom_text(label = annual_corr_tb$year, fontface = "bold", size = 5) +
  geom_abline(intercept = c(0,0), slope = 1, linetype = "dashed") +
  geom_smooth(method='lm', formula= y~x, color = "grey30") +
  #annotate("text", x = 50, y = -5, label = paste("italic(t) ==", signif(plot2_test$statistic,3),
  #                                               sep = " "), parse = TRUE) +
  #annotate("text", x = 65, y = -5, label = paste("df =", plot2_test$parameter,
  #                                               sep = " ")) +
  #annotate("text", x = 82, y = -5, label = paste("italic(p) ==", signif(plot2_test$p.value,2),
  #                                               sep = " "), parse = TRUE) +
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
annual_rarefaction_observed_corr_plot

# - specrich and observed

specrich2_observed_annual_corr_plot <- ggplot(annual_corr_tb, aes(x = specrich, y = obs)) +
  geom_point(size = 1) +
  #geom_line()+
  xlab("SPECRICH2 Estimates") +
  ylab("Observed Richness") +
  scale_y_continuous(expand = c(0,0), breaks=seq(-25, 200, 25)) +
  scale_x_continuous(expand = c(0,0), breaks=seq(0, 200, 25)) +
  expand_limits(x = 0, y = 0) +
  #scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  #scale_y_continuous(breaks=seq(0, 100, 10)) +
  geom_segment(data = annual_corr_tb, aes(x = specrich - specrich_se, xend = specrich + specrich_se, y = obs,
                                          yend = obs),
               color = "grey30") +
  #geom_text(label = annual_corr_tb$year, fontface = "bold", size = 5) +
  geom_abline(intercept = c(0,0), slope = 1, linetype = "dashed") +
  geom_smooth(method='lm', formula= y~x, color = "grey30") +
  #annotate("text", x = 50, y = -5, label = paste("italic(t) ==", signif(plot2_test$statistic,3),
  #                                               sep = " "), parse = TRUE) +
  #annotate("text", x = 65, y = -5, label = paste("df =", plot2_test$parameter,
  #                                               sep = " ")) +
  #annotate("text", x = 82, y = -5, label = paste("italic(p) ==", signif(plot2_test$p.value,2),
  #                                               sep = " "), parse = TRUE) +
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
specrich2_observed_annual_corr_plot

# ------ to get the correlations (Pearson correlations)

# - iNEXT & specrich2 
cor.test(annual_corr_tb$inext_est,
         annual_corr_tb$specrich)
#Pearson's product-moment correlation
#
#data:  annual_corr_tb$inext_est and annual_corr_tb$specrich
#t = 15.951, df = 92, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.7918843 0.9028423
#sample estimates:
#  cor 
#0.8569968

2.2e-16 < 0.001 # TRUE

# - specrich2 & observed
cor.test(annual_corr_tb$specrich,
         annual_corr_tb$obs)
#Pearson's product-moment correlation
#
#data:  annual_corr_tb$specrich and annual_corr_tb$obs
#t = 17.845, df = 93, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.8243778 0.9184472
#sample estimates:
#  cor 
#0.8797521 

2.2e-16 < 0.001 # TRUE

# - observed & iNEXT
cor.test(annual_corr_tb$obs,
         annual_corr_tb$inext_est)
#Pearson's product-moment correlation
#
#data:  annual_corr_tb$obs and annual_corr_tb$inext_est
#t = 8.5562, df = 93, p-value = 2.306e-13
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.5334911 0.7631409
#sample estimates:
#  cor 
#0.6636749 

2.306e-13 < 0.001 # TRUE


# correlations between seasons (primary period) estimates ####

# - iNEXT data
sample_coverage_base_0.8976_2 <- sample_coverage_base_0.8976 %>%
  mutate(period = str_extract(site, regex("
                             \\d # a digit
                             ", comments = TRUE))) %>%
  mutate(period = as.numeric(period)) %>%
  filter(order == "0")
sample_coverage_base_0.8976_2

# - RPresence data

occupancy_estimates

# - specrich2 annual data

specrich2_annual2_pp <- specrich2_annual2 %>%
  mutate(grouping = if_else(year >= 1909 & year <= 1913, "period1", "NA")) %>%
  mutate(grouping = if_else(year >= 1924 & year <= 1928, "period2", grouping)) %>%
  mutate(grouping = if_else(year >= 1939 & year <= 1943, "period3", grouping)) %>%
  mutate(grouping = if_else(year >= 1954 & year <= 1958, "period4", grouping)) %>%
  mutate(grouping = if_else(year >= 1969 & year <= 1973, "period5", grouping)) %>%
  mutate(grouping = if_else(year >= 1984 & year <= 1988, "period6", grouping)) %>%
  mutate(grouping = if_else(year >= 1999 & year <= 2003, "period7", grouping)) %>%
  mutate(grouping = if_else(year >= 2014 & year <= 2018, "period8", grouping))
specrich2_annual2_pp %>%
  count(year, grouping) %>%
  print(n = 100)

period1 <- specrich2_annual2_pp %>% filter(grouping == "period1")
period1_avg <- mean(period1$richness_estimate)
period1_se <- mean(period1$se_richness_estimate)
period1_avg

period2 <- specrich2_annual2_pp %>% filter(grouping == "period2")
period2_avg <- mean(period2$richness_estimate)
period2_se <- mean(period2$se_richness_estimate)
period2_avg

period3 <- specrich2_annual2_pp %>% filter(grouping == "period3")
period3_avg <- mean(period3$richness_estimate)
period3_se <- mean(period3$se_richness_estimate)
period3_avg

period4 <- specrich2_annual2_pp %>% filter(grouping == "period4")
period4_avg <- mean(period4$richness_estimate)
period4_se <- mean(period4$se_richness_estimate)
period4_avg

period5 <- specrich2_annual2_pp %>% filter(grouping == "period5")
period5_avg <- mean(period5$richness_estimate)
period5_se <- mean(period5$se_richness_estimate)
period5_avg

period6 <- specrich2_annual2_pp %>% filter(grouping == "period6")
period6_avg <- mean(period6$richness_estimate)
period6_se <- mean(period6$se_richness_estimate)
period6_avg

period7 <- specrich2_annual2_pp %>% filter(grouping == "period7")
period7_avg <- mean(period7$richness_estimate)
period7_se <- mean(period7$se_richness_estimate)
period7_avg

period8 <- specrich2_annual2_pp %>% filter(grouping == "period8")
period8_avg <- mean(period8$richness_estimate)
period8_se <- mean(period8$se_richness_estimate)
period8_avg

avg_specrich_estimates <- c(period1_avg, period2_avg, period3_avg, period4_avg,
                            period5_avg, period6_avg, period7_avg, period8_avg)
avg_se_specrich_estimates <- c(period1_se, period2_se, period3_se, period4_se,
                               period5_se, period6_se, period7_se, period8_se)

avg_specrich_estimates
presence_estimates <- occupancy_estimates$spp_rich
presence_se_estimates <- occupancy_estimates$se_spp
timeperiods <- c(seq(1,8,1))

# - observed species richness over the same primary periods
num_species_per_year <- num_species_per_year %>%
  mutate(grouping = if_else(year >= 1909 & year <= 1913, "period1", "NA")) %>%
  mutate(grouping = if_else(year >= 1924 & year <= 1928, "period2", grouping)) %>%
  mutate(grouping = if_else(year >= 1939 & year <= 1943, "period3", grouping)) %>%
  mutate(grouping = if_else(year >= 1954 & year <= 1958, "period4", grouping)) %>%
  mutate(grouping = if_else(year >= 1969 & year <= 1973, "period5", grouping)) %>%
  mutate(grouping = if_else(year >= 1984 & year <= 1988, "period6", grouping)) %>%
  mutate(grouping = if_else(year >= 1999 & year <= 2003, "period7", grouping)) %>%
  mutate(grouping = if_else(year >= 2014 & year <= 2018, "period8", grouping))

observed_data <- num_species_per_year %>%
  filter(grouping != "NA") %>%
  group_by(grouping) %>%
  summarize(obs_avg = mean(num_species),
            obs_se = se(num_species)) %>%
  mutate(period = seq(1,8,1)) %>%
  select(-grouping)
observed_data

# join all the data together for easier comparison
# most of these use SE instead of CI but iNEXT only gives CI


inext_rarefaction <- sample_coverage_base_0.8976_2 %>%
  rename(inext_estimate = qD) %>%
  rename(inext_lower = qD.LCL) %>%
  rename(inext_upper = qD.UCL) %>%
  select(inext_estimate, inext_upper, inext_lower, period)
inext_rarefaction

correlation_tb <- tibble(specrich = as.numeric(avg_specrich_estimates),
                         presence = as.numeric(presence_estimates),
                         specrich_se = as.numeric(avg_se_specrich_estimates),
                         presence_se = as.numeric(presence_se_estimates),
                         period = timeperiods) %>%
  full_join(observed_data) %>%
  full_join(inext_rarefaction)

correlation_tb

# graphing the primary period correlations

SPECRICH_PRESENCE_cor_plot <- ggplot(correlation_tb, aes(x = specrich, y = presence)) +  #geom_point(size = 3) +
  #geom_line()+
  ylab("Multi-season Model Estimates") +
  xlab("Avg. SPECRICH2 Estimates") +
  scale_y_continuous(expand = c(0,0), breaks=seq(-25, 200, 25)) +
  scale_x_continuous(expand = c(0,0), breaks=seq(0, 200, 25)) +
  expand_limits(x = 0, y = 0) +
  coord_flip() +
  #scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  #scale_y_continuous(breaks=seq(0, 100, 10)) +
  geom_segment(data = correlation_tb, aes(x = specrich, xend = specrich, y = presence - presence_se,
                                          yend = presence + presence_se),
               color = "grey30") +
  geom_segment(data = correlation_tb, aes(x = specrich - specrich_se, xend = specrich + specrich_se, y = presence,
                                          yend = presence),
               color = "grey30") +
  geom_text(label = correlation_tb$period, fontface = "bold", size = 5) +
  geom_abline(intercept = c(0,0), slope = 1, linetype = "dashed") +
  geom_smooth(method='lm', formula= y~x, color = "grey30") +
  #annotate("text", x = 50, y = -5, label = paste("italic(t) ==", signif(plot2_test$statistic,3),
  #                                               sep = " "), parse = TRUE) +
  #annotate("text", x = 65, y = -5, label = paste("df =", plot2_test$parameter,
  #                                               sep = " ")) +
  #annotate("text", x = 82, y = -5, label = paste("italic(p) ==", signif(plot2_test$p.value,2),
  #                                               sep = " "), parse = TRUE) +
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
        axis.title.x = element_text(face="bold", size=12, color="black"), #size of x-axis title
        axis.title.y = element_text(face="bold", size=12, color="black"), #size of y-axis title
        axis.text.x = element_text(size=12, color="black"), #size of x-axis text
        axis.text.y = element_text(size=12, color="black"))#size of y-axis text
SPECRICH_PRESENCE_cor_plot


# - rarefaction and PRESENCE 

PRESENCE_rarefaction_cor_plot <- ggplot(correlation_tb, aes(x = presence, y = inext_estimate)) +
  #geom_point(size = 3) +
  #geom_line()+
  ylab("Extrapolation Estimates") +
  xlab("Multi-season Model Estimates") +
  scale_y_continuous(expand = c(0,0), breaks=seq(-25, 200, 25)) +
  scale_x_continuous(expand = c(0,0), breaks=seq(0, 200, 25)) +
  expand_limits(x = 0, y = 0) +
  #scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  #scale_y_continuous(breaks=seq(0, 100, 10)) +
  geom_segment(data = correlation_tb, aes(x = presence, xend = presence, y = inext_lower,
                                          yend = inext_upper),
               color = "grey30") +
  geom_segment(data = correlation_tb, aes(x = presence - presence_se, xend = presence + presence_se, 
                                          y = inext_estimate, yend = inext_estimate),
               color = "grey30") +
  geom_text(label = correlation_tb$period, fontface = "bold", size = 5) +
  geom_abline(intercept = c(0,0), slope = 1, linetype = "dashed") +
  geom_smooth(method='lm', formula= y~x, color = "grey30") +
  #annotate("text", x = 50, y = -5, label = paste("italic(t) ==", signif(plot2_test$statistic,3),
  #                                               sep = " "), parse = TRUE) +
  #annotate("text", x = 65, y = -5, label = paste("df =", plot2_test$parameter,
  #                                               sep = " ")) +
  #annotate("text", x = 82, y = -5, label = paste("italic(p) ==", signif(plot2_test$p.value,2),
  #                                               sep = " "), parse = TRUE) +
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
        axis.title.x = element_text(face="bold", size=12, color="black"), #size of x-axis title
        axis.title.y = element_text(face="bold", size=12, color="black"), #size of y-axis title
        axis.text.x = element_text(size=12, color="black"), #size of x-axis text
        axis.text.y = element_text(size=12, color="black"))#size of y-axis text
PRESENCE_rarefaction_cor_plot


# - rarefaction and avg. specrich2

specrich_rarefaction_pp_cor_plot <- ggplot(correlation_tb, aes(x = specrich, y = inext_estimate)) +
  #geom_point(size = 3) +
  #geom_line()+
  ylab("Extrapolation Estimates") +
  xlab("Avg. SPECRICH2 Estimates") +
  scale_y_continuous(expand = c(0,0), breaks=seq(-25, 200, 25)) +
  scale_x_continuous(expand = c(0,0), breaks=seq(0, 200, 25)) +
  expand_limits(x = 0, y = 0) +
  #scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  #scale_y_continuous(breaks=seq(0, 100, 10)) +
  geom_segment(data = correlation_tb, aes(x = specrich, xend = specrich, y = inext_lower,
                                          yend = inext_upper),
               color = "grey30") +
  geom_segment(data = correlation_tb, aes(x = specrich - specrich_se, xend = specrich + specrich_se, 
                                          y = inext_estimate, yend = inext_estimate),
               color = "grey30") +
  geom_text(label = correlation_tb$period, fontface = "bold", size = 5) +
  geom_abline(intercept = c(0,0), slope = 1, linetype = "dashed") +
  geom_smooth(method='lm', formula= y~x, color = "grey30") +
  #annotate("text", x = 50, y = -5, label = paste("italic(t) ==", signif(plot2_test$statistic,3),
  #                                               sep = " "), parse = TRUE) +
  #annotate("text", x = 65, y = -5, label = paste("df =", plot2_test$parameter,
  #                                               sep = " ")) +
  #annotate("text", x = 82, y = -5, label = paste("italic(p) ==", signif(plot2_test$p.value,2),
  #                                               sep = " "), parse = TRUE) +
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
        axis.title.x = element_text(face="bold", size=12, color="black"), #size of x-axis title
        axis.title.y = element_text(face="bold", size=12, color="black"), #size of y-axis title
        axis.text.x = element_text(size=12, color="black"), #size of x-axis text
        axis.text.y = element_text(size=12, color="black"))#size of y-axis text
specrich_rarefaction_pp_cor_plot

# -- obs (avg) & presence 

obs_PRESENCE_cor_plot <- ggplot(correlation_tb, aes(x = presence, y = obs_avg)) +
  #geom_point(size = 3) +
  #geom_line()+
  xlab("Multi-season Model Estimates") +
  ylab("Avg. Observed Richness") +
  scale_y_continuous(expand = c(0,0), breaks=seq(-25, 200, 25)) +
  scale_x_continuous(expand = c(0,0), breaks=seq(0, 200, 25)) +
  expand_limits(x = 0, y = 0) +
  #scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  #scale_y_continuous(breaks=seq(0, 100, 10)) +
  geom_segment(data = correlation_tb, aes(x = presence - presence_se, xend = presence + presence_se, y = obs_avg,
                                          yend = obs_avg),
               color = "grey30") +
  geom_segment(data = correlation_tb, aes(x = presence, xend = presence, 
                                          y = obs_avg - obs_se, yend = obs_avg + obs_se),
               color = "grey30") +
  geom_text(label = correlation_tb$period, fontface = "bold", size = 5) +
  geom_abline(intercept = c(0,0), slope = 1, linetype = "dashed") +
  geom_smooth(method='lm', formula= y~x, color = "grey30") +
  #annotate("text", x = 50, y = -5, label = paste("italic(t) ==", signif(plot2_test$statistic,3),
  #                                               sep = " "), parse = TRUE) +
  #annotate("text", x = 65, y = -5, label = paste("df =", plot2_test$parameter,
  #                                               sep = " ")) +
  #annotate("text", x = 82, y = -5, label = paste("italic(p) ==", signif(plot2_test$p.value,2),
  #                                               sep = " "), parse = TRUE) +
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
        axis.title.x = element_text(face="bold", size=12, color="black"), #size of x-axis title
        axis.title.y = element_text(face="bold", size=12, color="black"), #size of y-axis title
        axis.text.x = element_text(size=12, color="black"), #size of x-axis text
        axis.text.y = element_text(size=12, color="black"))#size of y-axis text
obs_PRESENCE_cor_plot

# -- obs (avg) & specrich (avg)

obs_SPECRICH_cor_plot <- ggplot(correlation_tb, aes(x = specrich, y = obs_avg)) +
  #geom_point(size = 3) +
  #geom_line()+
  xlab("Avg. SPECRICH2 Estimates") +
  ylab("Avg. Observed Richness") +
  scale_y_continuous(expand = c(0,0), breaks=seq(-25, 200, 25)) +
  scale_x_continuous(expand = c(0,0), breaks=seq(0, 200, 25)) +
  expand_limits(x = 0, y = 0) +
  #scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  #scale_y_continuous(breaks=seq(0, 100, 10)) +
  geom_segment(data = correlation_tb, aes(x = specrich - specrich_se, xend = specrich + specrich_se, y = obs_avg,
                                          yend = obs_avg),
               color = "grey30") +
  geom_segment(data = correlation_tb, aes(x = specrich, xend = specrich, 
                                          y = obs_avg - obs_se, yend = obs_avg + obs_se),
               color = "grey30") +
  geom_text(label = correlation_tb$period, fontface = "bold", size = 5) +
  geom_abline(intercept = c(0,0), slope = 1, linetype = "dashed") +
  geom_smooth(method='lm', formula= y~x, color = "grey30") +
  #annotate("text", x = 50, y = -5, label = paste("italic(t) ==", signif(plot2_test$statistic,3),
  #                                               sep = " "), parse = TRUE) +
  #annotate("text", x = 65, y = -5, label = paste("df =", plot2_test$parameter,
  #                                               sep = " ")) +
  #annotate("text", x = 82, y = -5, label = paste("italic(p) ==", signif(plot2_test$p.value,2),
  #                                               sep = " "), parse = TRUE) +
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
        axis.title.x = element_text(face="bold", size=12, color="black"), #size of x-axis title
        axis.title.y = element_text(face="bold", size=12, color="black"), #size of y-axis title
        axis.text.x = element_text(size=12, color="black"), #size of x-axis text
        axis.text.y = element_text(size=12, color="black"))#size of y-axis text
obs_SPECRICH_cor_plot


# -- obs (avg) & inext

obs_rarefaction_pp_cor_plot <- ggplot(correlation_tb, aes(x = inext_estimate, y = obs_avg)) +
  #geom_point(size = 3) +
  #geom_line()+
  xlab("Extrapolation Estimates") +
  ylab("Avg. Observed Richness") +
  scale_y_continuous(expand = c(0,0), breaks=seq(-25, 200, 25)) +
  scale_x_continuous(expand = c(0,0), breaks=seq(0, 200, 25)) +
  expand_limits(x = 0, y = 0) +
  #scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  #scale_y_continuous(breaks=seq(0, 100, 10)) +
  geom_segment(data = correlation_tb, aes(x = inext_lower, xend = inext_upper, y = obs_avg,
                                          yend = obs_avg),
               color = "grey30") +
  geom_segment(data = correlation_tb, aes(x = inext_estimate, xend = inext_estimate, 
                                          y = obs_avg - obs_se, yend = obs_avg + obs_se),
               color = "grey30") +
  geom_text(label = correlation_tb$period, fontface = "bold", size = 5) +
  geom_abline(intercept = c(0,0), slope = 1, linetype = "dashed") +
  geom_smooth(method='lm', formula= y~x, color = "grey30") +
  #annotate("text", x = 50, y = -5, label = paste("italic(t) ==", signif(plot2_test$statistic,3),
  #                                               sep = " "), parse = TRUE) +
  #annotate("text", x = 65, y = -5, label = paste("df =", plot2_test$parameter,
  #                                               sep = " ")) +
  #annotate("text", x = 82, y = -5, label = paste("italic(p) ==", signif(plot2_test$p.value,2),
  #                                               sep = " "), parse = TRUE) +
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
        axis.title.x = element_text(face="bold", size=12, color="black"), #size of x-axis title
        axis.title.y = element_text(face="bold", size=12, color="black"), #size of y-axis title
        axis.text.x = element_text(size=12, color="black"), #size of x-axis text
        axis.text.y = element_text(size=12, color="black"))#size of y-axis text
obs_rarefaction_pp_cor_plot

# ------ to get the correlations (Pearson correlations)

names(correlation_tb)

# -- PRESENCE & SPECRICH2 (avg)
cor.test(correlation_tb$specrich, 
         correlation_tb$presence)
#Pearson's product-moment correlation
#
#data:  correlation_tb$specrich and correlation_tb$presence
#t = 7.9395, df = 5, p-value = 0.0005108
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.7614091 0.9946389
#sample estimates:
#  cor 
#0.9625533 


# -- Rarefaction & PRESENCE
cor.test(correlation_tb$presence, 
         correlation_tb$inext_estimate)
#Pearson's product-moment correlation
#
#data:  correlation_tb$presence and correlation_tb$inext_estimate
#t = 9.2386, df = 5, p-value = 0.0002496
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.8164812 0.9959986
#sample estimates:
#  cor 
#0.9719365 


# -- PRESENCE & observed (avg)
cor.test(correlation_tb$presence, 
         correlation_tb$obs_avg)
#Pearson's product-moment correlation
#
#data:  correlation_tb$presence and correlation_tb$obs_avg
#t = 6.7008, df = 6, p-value = 0.0005362
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.6935816 0.9891978
#sample estimates:
#  cor 
#0.9392149 


# -- SPECRICH2 & inext
cor.test(correlation_tb$specrich, 
         correlation_tb$inext_estimate)
#Pearson's product-moment correlation
#
#data:  correlation_tb$specrich and correlation_tb$inext_estimate
#t = 5.0799, df = 5, p-value = 0.003835
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.5219302 0.9876113
#sample estimates:
#  cor 
#0.9152536 


# -- SPECRICH2 & observed (avg)
cor.test(correlation_tb$specrich, 
         correlation_tb$obs_avg)
#Pearson's product-moment correlation
#
#data:  correlation_tb$specrich and correlation_tb$obs_avg
#t = 9.749, df = 5, p-value = 0.0001931
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.8331994 0.9963956
#sample estimates:
#  cor 
#0.9746904


# -- iNEXT & observed (avg)
cor.test(correlation_tb$inext_estimate,
         correlation_tb$obs_avg)
#Pearson's product-moment correlation
#
#data:  correlation_tb$inext_estimate and correlation_tb$obs_avg
#t = 4.6447, df = 5, p-value = 0.005609
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.4602661 0.9854387
#sample estimates:
#  cor 
#0.9010232

# save the figures ####

all_4_estimate_methods
#ggsave("all_4_estimate_methods_spp_richness_trend.png", width = 6, height = 3,
#       units = "in", dpi = 600, plot = all_4_estimate_methods,
#       path = figure_path, family = "Arial") 

estimates_with_CI
#ggsave("which_time_period_has_greatest_sppRich.png", width = 6, height = 3.5,
#       units = "in", dpi = 600, plot = estimates_with_CI,
#       path = figure_path)

specrich_rarefaction_annual_corr_plot
#ggsave("specrich_rarefaction_annual_corr_plot.png", width = 4, height = 3,
#       units = "in", dpi = 600, plot = specrich_rarefaction_annual_corr_plot,
#       path = figure_path, family = "Arial")


annual_rarefaction_observed_corr_plot
#ggsave("annual_rarefaction_observed_corr_plot.png", width = 4, height = 3,
#       units = "in", dpi = 600, plot = annual_rarefaction_observed_corr_plot,
#       path = figure_path, family = "Arial")


specrich2_observed_annual_corr_plot
#ggsave("specrich2_observed_annual_corr_plot.png", width = 4, height = 3,
#       units = "in", dpi = 600, plot = specrich2_observed_annual_corr_plot,
#       path = figure_path, family = "Arial")



SPECRICH_PRESENCE_cor_plot
#ggsave("SPECRICH_PRESENCE_cor_plot.png", width = 3.1, height = 3,
#       units = "in", dpi = 600, plot = SPECRICH_PRESENCE_cor_plot,
#       path = figure_path, family = "Arial")

PRESENCE_rarefaction_cor_plot
#ggsave("PRESENCE_rarefaction_cor_plot.png", width = 3.1, height = 3,
#       units = "in", dpi = 600, plot = PRESENCE_rarefaction_cor_plot,
#       path = figure_path, family = "Arial")

specrich_rarefaction_pp_cor_plot
#ggsave("specrich_rarefaction_pp_cor_plot.png", width = 3.1, height = 3,
#       units = "in", dpi = 600, plot = specrich_rarefaction_pp_cor_plot,
#       path = figure_path, family = "Arial")

obs_PRESENCE_cor_plot
#ggsave("obs_PRESENCE_cor_plot.png", width = 3.1, height = 3,
#       units = "in", dpi = 600, plot = obs_PRESENCE_cor_plot,
#       path = figure_path, family = "Arial")


obs_SPECRICH_cor_plot
#ggsave("obs_SPECRICH_cor_plot.png", width = 3.1, height = 3,
#       units = "in", dpi = 600, plot = obs_SPECRICH_cor_plot,
#       path = figure_path, family = "Arial")

obs_rarefaction_pp_cor_plot
#ggsave("obs_rarefaction_pp_cor_plot.png", width = 3.1, height = 3,
#       units = "in", dpi = 600, plot = obs_rarefaction_pp_cor_plot,
#       path = figure_path, family = "Arial")

