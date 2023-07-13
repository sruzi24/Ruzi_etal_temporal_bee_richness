# graphing species richness estimates comparing Wake co and Raleigh

# load libraries ####
library(tidyverse)
library(here)
library(iNEXT)


# set relative pathways ####
data_path <- here::here("data")
figure_path <- here::here("figs")
Rsource_path <- here::here("R/Resources")


# load in the RPresence data for Raleigh ####
load(paste(Rsource_path,"Model_1_psi_gamSeason_epsSeason_detSeason_RaleighSubset.rda", sep = "/"))

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

# load in the RPresence data for all of Wake Co ####
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



# iNEXT estimates - Raleigh ####
load(paste(Rsource_path,"Raleigh_subset_sample_coverage_0.8953_iNEXT_output_setup_like_PRESENCE.rda", sep = "/"))

inext_plotting_Raleigh <- Raleigh_subset_coverage_base_0.8953 %>%
  filter(order == "0")
inext_plotting_Raleigh$pp_plotting <- c(1911,1926,1941,1956,1971,1986,2016)

load(paste(Rsource_path,"Raleigh_subset_sample_coverage_iNEXT_output_setup_like_PRESENCE_extended.rda", sep = "/"))


# iNEXT estimates - Wake Co ####
load(paste(Rsource_path,"sample_coverage_0.8976_iNEXT_output_setup_like_PRESENCE.rda", sep = "/"))

sample_coverage_base_0.8976 %>%
  filter(order == "0")

inext_plotting <- sample_coverage_base_0.8976 %>%
  filter(order == "0")
inext_plotting$pp_plotting <- c(1911,1926,1941,1956,1971,1986,2016)

load(paste(Rsource_path,"sample_coverage_iNEXT_output_setup_like_PRESENCE_extended.rda", sep = "/"))


# PRESENCE wake co & raleigh

Wake_vs_Raleigh_spp_rich_estimates <- ggplot(data = occupancy_estimates, aes(x = pp_plotting, y = spp_rich)) +
  geom_point(data = occupancy_estimates, aes(pp_plotting, y = spp_rich), size = 3, color = "dark red", shape = 19) +
  geom_point(data = occupancy_estimates_Raleigh, aes(x = pp_plotting, y = spp_rich), color = "deeppink", shape = 1, size = 3) +
  geom_point(data = inext_plotting, aes(x = pp_plotting, y = qD), color = "violet", shape = 17, size = 3) +
  geom_point(data = inext_plotting_Raleigh, aes(x = pp_plotting, y = qD), color = "purple", shape = 2, size = 3) +
  ylab("Species Richness") +
  xlab("Year") +
  scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  scale_y_continuous(expand = c(0,0), breaks=seq(0, 350, 25)) +
  geom_segment(data = occupancy_estimates, aes(x = pp_plotting, xend = pp_plotting, y = spp_rich_lower_0.95,
                                               yend = spp_rich_upper_0.95),
               color = "dark red", linetype = 2) +
  geom_segment(data = occupancy_estimates, aes(x = pp_plotting - 2.5, xend = pp_plotting + 2.5, y = spp_rich,
                                               yend = spp_rich),
               color = "dark red", linetype = 2) +
  geom_segment(data = occupancy_estimates_Raleigh, aes(x = pp_plotting, xend = pp_plotting, y = spp_rich_lower_0.95,
                                                       yend = spp_rich_upper_0.95),
               color = "deeppink", linetype = 4) +
  geom_segment(data = occupancy_estimates_Raleigh, aes(x = pp_plotting - 2.5, xend = pp_plotting + 2.5, y = spp_rich,
                                                       yend = spp_rich),
               color = "deeppink", linetype = 4) +
  geom_segment(data = inext_plotting, aes(x = pp_plotting - 2.5, xend = pp_plotting + 2.5, y = qD,
                                          yend = qD),
               color = "violet", linetype = 2) +
  geom_segment(data = inext_plotting, aes(x = pp_plotting, xend = pp_plotting, y = qD.LCL,
                                          yend = qD.UCL),
               color = "violet", linetype = 2) +
  geom_segment(data = inext_plotting_Raleigh, aes(x = pp_plotting - 2.5, xend = pp_plotting + 2.5, y = qD,
                                                  yend = qD),
               color = "purple", linetype = 4) +
  geom_segment(data = inext_plotting_Raleigh, aes(x = pp_plotting, xend = pp_plotting, y = qD.LCL,
                                                  yend = qD.UCL),
               color = "purple", linetype = 4) +
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
Wake_vs_Raleigh_spp_rich_estimates

#ggsave("Wake_vs_Raleigh_spp_rich_estimates.png", width = 6, height = 3,
#       units = "in", dpi = 600, plot = Wake_vs_Raleigh_spp_rich_estimates,
#       path = figure_path, family = "Arial") 



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


# to determine which time period has the greatest species richness ####
# will use RPresence and iNEXT data set up like for PRESEENCE
# with 95% CI

## Wake Co

occupancy_estimates$pp_code <- c(1:8)
occupancy_estimates$analysis_method <- "PRESENCE\nWake"
inext_plotting$pp_code <- c(1:6,8)

CI_plotting <- inext_plotting %>%
  mutate(analysis_method = "iNEXT\nWake") %>%
  # will need to rename some columns
  rename(spp_rich = qD) %>%
  rename(spp_rich_lower_0.95 = qD.LCL) %>%
  rename(spp_rich_upper_0.95 = qD.UCL) %>%
  # join the two datasets
  full_join(occupancy_estimates)

CI_plotting


obs_sample_coverage <- max_t$DataInfo %>%
  mutate(pp_code = c(1:6,8)) %>%
  mutate(analysis_method = "iNEXT\nWake") %>%
  mutate(order = c(2,3,4,8,6,1,5)) 


## Raleigh

occupancy_estimates_Raleigh$pp_code <- c(1:8)
occupancy_estimates_Raleigh$analysis_method <- "PRESENCE\nRaleigh"
inext_plotting_Raleigh$pp_code <- c(1:6,8)

CI_plotting_Raleigh <- inext_plotting %>%
  mutate(analysis_method = "iNEXT\nRaleigh") %>%
  # will need to rename some columns
  rename(spp_rich = qD) %>%
  rename(spp_rich_lower_0.95 = qD.LCL) %>%
  rename(spp_rich_upper_0.95 = qD.UCL) %>%
  # join the two datasets
  full_join(occupancy_estimates_Raleigh) 

CI_plotting_Raleigh


obs_sample_coverage_Raleigh <- Raleigh_subset_max_t$DataInfo %>%
  mutate(pp_code = c(1:6,8)) %>%
  mutate(analysis_method = "iNEXT\nRaleigh") %>%
  mutate(order = c(2,3,4,8,6,1,5)) 


# merge the plotting together
CI_plotting_merged <- CI_plotting %>%
  full_join(CI_plotting_Raleigh)

obs_sample_coverage_merged <- obs_sample_coverage %>%
  full_join(obs_sample_coverage_Raleigh)



estimates_with_CI_Wake_Raleigh <- ggplot(data = CI_plotting_merged, aes(x = reorder(pp_code, spp_rich), y = spp_rich, color = analysis_method, shape = analysis_method)) +
  geom_point(size = 3) +
  geom_errorbar(data = CI_plotting_merged, aes(x = reorder(pp_code, spp_rich), 
                                               ymin = spp_rich_lower_0.95,
                                               ymax = spp_rich_upper_0.95,
                                               color = analysis_method), 
                width = 0.3, linetype = 2) +
  labs(y = "Species Richness", x = "Season",
       color = "", shape = "") +
  scale_color_manual(breaks = c("iNEXT\nWake", "iNEXT\nRaleigh", "PRESENCE\nWake", "PRESENCE\nRaleigh"),
                     values = c("violet", "purple", "dark red", "deeppink")) +
  scale_shape_manual(breaks = c("iNEXT\nWake", "iNEXT\nRaleigh", "PRESENCE\nWake", "PRESENCE\nRaleigh"), 
                     values = c(19, 1, 17, 2)) +
  geom_vline(xintercept = 4.5, linetype="dotted", 
             color = "black") +
  geom_text(data = obs_sample_coverage, aes(x = reorder(pp_code, order) , y = 0), 
            label = round(obs_sample_coverage$SC,2), color = "black") + 
  geom_text(data = obs_sample_coverage_Raleigh, aes(x = reorder(pp_code, order) , y = 220), 
            label = round(obs_sample_coverage_Raleigh$SC,2), color = "black") + 
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
estimates_with_CI_Wake_Raleigh

#ggsave("Wake_Raleigh_which_time_period_has_greatest_sppRich.png", width = 6, height = 4,
#       units = "in", dpi = 600, plot = estimates_with_CI_Wake_Raleigh,
#       path = figure_path)

# WAke Co. vs. Raleigh inext estimates

cor.test(inext_plotting$qD,
         inext_plotting_Raleigh$qD)
#Pearson's product-moment correlation
#
#data:  inext_plotting$qD and inext_plotting_Raleigh$qD
#t = 9.7585, df = 5, p-value = 0.0001922
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.8334910 0.9964025
#sample estimates:
#  cor 
#0.9747381