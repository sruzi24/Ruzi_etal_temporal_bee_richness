# urban growth for Raleigh, NC
# urban pixels determined using methods from:
# - Li, X., Y. Zhou, M. Hejazi, M. Wise, C. Vernon, G. Iyer, and W. Chen. 2021.
#   Global urban growth between 1870 and 2100 from integrated high resolution
#   mapped data and urban dynamic modeling. Communications Earth & Environment.
#   2(1):201. 10.1038/s43247-021-00273-w

# load libraries ####
library(here)
library(tidyverse)

# set relative pathways ####
data_path <- here::here("data")
figure_path <- here::here("figs")
Rsource_path <- here::here("R/Resources")

# set up the data ####
# - pixel values and others obtained from Curtis Belyea

year_decadal <- c(seq(1900, 2010, 10))
year_decadal

urb_pixels <- c(20,28,37,43,45,50,58,71,72,73,87,99)

# get a table for the raw data
pixel_table <- cbind(year_decadal, urb_pixels)
pixel_table

# data analyses will be done on the year that is inbetween two seasons
# - for example: season1 (primary period 1) is from 1909-1913 and season 2 (primary period 2)
# is from 1924-1928, so would want the amount urban from the middle of 1913-1924 which is 1918
# so the estimated pixel count formula is = (t2 - t1)*x + t1
# where t2 = the urb_pixels at time2, t1 = the urb_pixels at time1,
# x = either 0.3 or 0.8 depending on if the extinction_year ends in 3 or 8

# 2008 is chosen instead

extinction_years <- c(1903,1918,1933,1948,1963,1978,1993,2008)
estimated_pixel_count <- c(22.4,35.2,43.6,49,61.9,71.8,77.2,96.6)

# checking some calculations
(28-20)*0.3+20 # 22.4 for 1903
(99-87)*0.8+87 # 96.6 for 2008

# estimated pixel change would be t2 - t1

estimated_pixel_change <- c(NA,12.8,8.4,5.4,12.9,9.9,5.4,19.4)

# pixel dimension information from Curtis
pixel_length_height_m <- 1831.528563
pixel_area_sq_m <- pixel_length_height_m*pixel_length_height_m
pixel_area_sq_km <- pixel_area_sq_m/(1000*1000)

# get the estimated sq km change in urban pixels

pixel_change_table <- cbind(extinction_years, estimated_pixel_count, estimated_pixel_change)
pixel_change_table2 <- pixel_change_table %>%
  as_tibble()%>%
  mutate(estimated_sq_km_change = estimated_pixel_change*pixel_area_sq_km) %>%
  # amount or urban area in km in total
  mutate(estimated_urban_area_sq_km = estimated_pixel_count * pixel_area_sq_km)
pixel_change_table2


# year and estimated urban area 
cor.test(pixel_change_table2$extinction_years, pixel_change_table2$estimated_urban_area_sq_km)
#Pearson's product-moment correlation
#
#data:  extinction_years and estimated_urban_area_sq_km
#t = 19.409, df = 6, p-value = 1.211e-06
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.9554099 0.9986321
#sample estimates:
#      cor 
#0.9921299 


  #scale_x_continuous(expand = c(0,0)) +
  #scale_y_continuous(expand = c(0,0))+
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


# make the figure ####
urban_extent_fig <- ggplot(pixel_change_table2, aes(x = extinction_years, y = estimated_urban_area_sq_km)) +
  geom_point(stat = "identity", size = 3) +
  labs(x = "Year", y = "Estimated Urban Extent\n(sq km)") +
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
urban_extent_fig

# save the figure ####
#ggsave("urban_extent_fig.png", width = 4, height = 3.5,
#       units = "in", dpi = 600, plot = urban_extent_fig,
#       path = figure_path) 
