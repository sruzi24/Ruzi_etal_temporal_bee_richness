## - getting the long and lat for the Chatham, Durham, and Wake Counties of North Carolina
## and creating base maps that can be used later for graphing the three counties

# load libraries ####
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(wellknown)
library(here)

# set file paths ####
figure_path <- here::here("figs")
data_path <- here::here("data")

# first get the map information for the states in USA ####
states <- map_data("state")
head(states)

# now get all the counties map information  ####
counties <- map_data("county")
head(counties)

# subset out north carolina from the state data ####
nc_df <- subset(states, region=="north carolina")
head(nc_df)

# now let's also get the county lines there ####
nc_county <- subset(counties, region=="north carolina")
head(nc_county)

# Plotting the state and removing the theme ####
nc_base <- ggplot(data=nc_df, mapping=aes(x=long, y=lat, group=group))+
  coord_fixed(1.3)+
  geom_polygon(color="black", fill="gray")+
  theme_nothing()
nc_base


nc_base

# adding in all the NC counties with white outlines
nc_base+
  geom_polygon(data=nc_county, fill=NA, color="white")+
  geom_polygon(color="black", fill=NA) # to get the state border back on top

# only drawing in the 3 counties interest in (Chathum, Durham, and Wake)
nc_RDU <- subset(nc_county, subregion %in% c("chatham","durham","wake"))
head(nc_RDU)

nc_Wake <- subset(nc_county, subregion == "wake")

NC_RDU <- nc_base+
  geom_polygon(data=nc_RDU, fill=NA, color="white")+
  geom_polygon(color="black", fill=NA) # to get the state border back on top
NC_RDU

NC_Wake <- nc_base+
  geom_polygon(data=nc_Wake, fill="red", color="black")#+
#geom_polygon(color="black", fill=NA) # to get the state border back on top
NC_Wake


ggsave("NC_Wake_polygon.png", width = 4, height = 2,
       units = "in", dpi = 600, plot = NC_Wake,
       path = figure_path, family = "Arial")


# Plotting Wake county only ####
nc_wake_grey1 <- ggplot(data=nc_Wake, mapping=aes(x=long, y=lat, group=group))+
  coord_fixed(1.3)+
  geom_polygon(color="black", fill="gray80")+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )
nc_wake_grey1

nc_wake_grey2 <- ggplot(data=nc_Wake, mapping=aes(x=long, y=lat, group=group))+
  coord_fixed(1.3)+
  geom_polygon(color="black", fill="gray60")+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )
nc_wake_grey2

nc_wake_grey3 <- ggplot(data=nc_Wake, mapping=aes(x=long, y=lat, group=group))+
  coord_fixed(1.3)+
  geom_polygon(color="black", fill="gray40")+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )
nc_wake_grey3

nc_wake_grey4 <- ggplot(data=nc_Wake, mapping=aes(x=long, y=lat, group=group))+
  coord_fixed(1.3)+
  geom_polygon(color="black", fill="gray20")+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )
nc_wake_grey4

nc_wake_red <- ggplot(data=nc_Wake, mapping=aes(x=long, y=lat, group=group))+
  coord_fixed(1.3)+
  geom_polygon(color="black", fill="red")+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )
nc_wake_red

ggsave("nc_wake_grey1.png", width = 2, height = 2,
       units = "in", dpi = 600, plot = nc_wake_grey1,
       path = figure_path, family = "Arial", bg = "transparent")
ggsave("nc_wake_grey2.png", width = 2, height = 2,
       units = "in", dpi = 600, plot =nc_wake_grey2 ,
       path = figure_path, family = "Arial", bg = "transparent")
ggsave("nc_wake_grey3.png", width = 2, height = 2,
       units = "in", dpi = 600, plot = nc_wake_grey3,
       path = figure_path, family = "Arial", bg = "transparent")
ggsave("nc_wake_grey4.png", width = 2, height = 2,
       units = "in", dpi = 600, plot = nc_wake_grey4,
       path = figure_path, family = "Arial", bg = "transparent")
ggsave("nc_wake_red.png", width = 2, height = 2,
       units = "in", dpi = 600, plot = nc_wake_red,
       path = figure_path, family = "Arial", bg = "transparent")
ggsave("nc_base.png", width = 4, height = 4,
       units = "in", dpi = 600, plot = nc_base,
       path = figure_path, family = "Arial", bg = "transparent")

#### -- forming polygons for each of the counties for use in querying the data repositories ####
## uses the wellknown package
### - Chatham county 
Chatham_coords <- subset(nc_RDU, subregion=="chatham")
head(Chatham_coords)
# this polygon is not closed (i.e. the first coordinate set is the not the same as the
# last one, but either one works with GBIF anyway)
Chatham_polygon <- polygon(Chatham_coords[,1:2]) 

### - Durham county
Durham_coords <- subset(nc_RDU, subregion=="durham")
head(Durham_coords)
# this polygon is not closed (i.e. the first coordinate set is the not the same as the
# last one, but either one works with GBIF anyway)
Durham_polygon <- polygon(Durham_coords[,1:2])

### - Wake county
Wake_coords <- subset(nc_RDU, subregion=="wake")
head(Wake_coords)
# this polygon is not closed (i.e. the first coordinate set is the not the same as the
# last one, but either one works with GBIF anyway)
Wake_polygon <- polygon(Wake_coords[,1:2])
# this is a closed polygon but returns the same number of specimens found when using
# GBIF as when not closed 
Wake_coords2 <- rbind(Wake_coords, Wake_coords[1,])
Wake_polygon2 <- polygon(Wake_coords2[,1:2])


### saving the polygones as RData

#save(Chatham_polygon, file = paste(data_path, "Chatham_polygon.RData", sep = "/"))
#save(Durham_polygon, file = paste(data_path, "Durham_polygon.RData", sep = "/"))
save(Wake_polygon, file = paste(data_path, "Wake_polygon.RData", sep = "/"))
save(Wake_polygon2,  file = paste(data_path, "Wake_polygon2.RData", sep = "/"))
