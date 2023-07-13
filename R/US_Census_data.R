# Census data
# graphing and correlations

# load libraries ####
library(tidyverse)
library(here)

# set relative pathways ####
figure_path <- here::here("figs")

# set up census data ####
# - census data is from US Census Bureau 2021, and Forstall 1996
census_data <- tibble(censusYear = c("1890", "1900", "1910", "1920", "1930", "1940", "1950", "1960", "1970", "1980", "1990", "2000", "2010"),
                      population = c("49207", "54626", "63229", "75155", "94757", "109544", "136450", "169082",
                                     "228453", "301327", "423380", "627846", "900993"))

census_data$censusYear <- as.numeric(census_data$censusYear)
census_data$population <- as.numeric(census_data$population)
census_data

# run spearman correlation ####
spearman_census_cor <- cor.test(census_data$censusYear, census_data$population, method = "spearman")
spearman_census_cor
#Spearman's rank correlation rho
#
#data:  census_data$censusYear and census_data$population
#S = 0, p-value < 2.2e-16
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#rho 
#  1 

spearman_census_cor$p.value # 0
spearman_census_cor$estimate # 1

# make the figure ####
census_plot <- ggplot(census_data, aes(x = censusYear, y = population)) +
  geom_point(size = 3) +
  ylab("Population of Wake County\n(thousands)") +
  xlab("Census Year") +
  scale_y_continuous(breaks=seq(0, 1000000, 250000), labels = c("0", "250", "500", "750", "1000"))+
  scale_x_continuous(breaks=seq(1890, 2020, 10)) +
  #annotate("text", x=1910, y=750000,
  #         label=paste('Spearman\'s rho =', spearman_census_cor$estimate))+
  #annotate("text", x=1910, y=720000,
  #         label=paste('p-value =', spearman_census_cor$p.value))+
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
        legend.title = element_blank(),
        axis.title.x = element_text(face="bold", size=14, color="black"), #size of x-axis title
        axis.title.y = element_text(face="bold", size=14, color="black"), #size of y-axis title
        axis.text.x = element_text(size=12, color="black", angle=45, hjust=1), #size of x-axis text
        axis.text.y = element_text(size=12, color="black"))#size of y-axis text
census_plot


# save the figure ####
#ggsave("census_plot_main_text.png", width = 4, height = 3.5,
#       units = "in", dpi = 600, plot = census_plot,
#       path = figure_path) 

