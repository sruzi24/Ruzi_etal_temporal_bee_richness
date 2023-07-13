# Top collectors over time

# load libraries ####
library(here)
library(tidyverse)
library(naniar)
library(vegan)
library(data.table)
library(directlabels)

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
                            .default = col_character()))
condensed_data

condensed_data <- condensed_data %>%
  mutate(year = as.numeric(year)) %>%
  mutate(decade = ifelse(year >= 1900 & year < 1910, "1900s", NA)) %>%
  mutate(decade = ifelse(year >= 1910 & year < 1920, "1910s", decade)) %>%
  mutate(decade = ifelse(year >= 1920 & year < 1930, "1920s", decade)) %>%
  mutate(decade = ifelse(year >= 1930 & year < 1940, "1930s", decade)) %>%
  mutate(decade = ifelse(year >= 1940 & year < 1950, "1940s", decade)) %>%
  mutate(decade = ifelse(year >= 1950 & year < 1960, "1950s", decade)) %>%
  mutate(decade = ifelse(year >= 1960 & year < 1970, "1960s", decade)) %>%
  mutate(decade = ifelse(year >= 1970 & year < 1980, "1970s", decade)) %>%
  mutate(decade = ifelse(year >= 1980 & year < 1990, "1980s", decade)) %>%
  mutate(decade = ifelse(year >= 1990 & year < 2000, "1990s", decade)) %>%
  mutate(decade = ifelse(year >= 2000 & year < 2010, "2000s", decade)) %>%
  mutate(decade = ifelse(year >= 2010 & year < 2019, "2010s", decade))
condensed_data


# see if there is a collector bias ####
# including all unique records, will remove species that were only collected once by any of the top collectors

# need to make a frequency table
# so to do that need to get the number of collection events per collector
# and the number of times each species was collected by that collector
# and need to set it up so that each collector is a row and each column is a different bee species
# - will do this based on detected/nondetected

# going to do this only for the top collectors of unique records (50 or more unique records)
# removing the unknown collectors as well

# - first without removing the singletons or doubletons
top_collectors <- condensed_data %>%
  count(recorded_by) %>%
  filter(n >= 50) %>%
  # remove unknown collectors
  filter(!is.na(recorded_by))
top_collectors
# A tibble: 6 x 2
#   recorded_by                  n
#   <chr>                    <int>
#1 A. Carper                  117
#2 A. Hamblin                 953
#3 C.S. Brimley               480
#4 E. Youngsteadt             130
#5 H. Levenson & assistants    89
#6 T.B. Mitchell             2703

top_collectors_list <- as.vector(unique(top_collectors$recorded_by))


condensed_data %>%
  count(day, month, year, recorded_by, decimal_latitude, decimal_longitude) %>%
  filter(recorded_by %in% top_collectors_list) %>%
  count(recorded_by) 
# A tibble: 6 x 2
#recorded_by                  n
#<chr>                    <int>
#1 A. Carper                   35
#2 A. Hamblin                 195
#3 C.S. Brimley               330
#4 E. Youngsteadt              63
#5 H. Levenson & assistants    12
#6 T.B. Mitchell             1014

# - now removing singletons
top_collectors_data <- condensed_data %>%
  filter(recorded_by %in% top_collectors_list)

top_collectors_data %>%
  count(scientific_name) %>% #306 species in this dataset
  filter(n == 1) # 68 of these species are singletons

singletons_tb <- top_collectors_data %>%
  count(scientific_name) %>%
  filter(n == 1)

singletons_list <- as.vector(singletons_tb$scientific_name)
singletons_list

# get a record of who collected these singltons to add to a table
top_collectors_data %>%
  filter(scientific_name %in% singletons_list) %>%
  count(scientific_name, recorded_by) %>%
  count(recorded_by)
# A tibble: 6 x 2
#recorded_by                  n
#<chr>                    <int>
#1 A. Carper                    3
#2 A. Hamblin                   5
#3 C.S. Brimley                12
#4 E. Youngsteadt               2
#5 H. Levenson & assistants     2
#6 T.B. Mitchell               43

# remove these singletons from the datset 
top_collectors_data_single_rm <- top_collectors_data %>%
  filter(scientific_name %nin% singletons_list)
top_collectors_data_single_rm

top_collectors_data_single_rm %>% 
  count(scientific_name) # now there are 238 species

# get the number of records & collection events with singletons removed by collector
top_collectors_data_single_rm %>%
  count(recorded_by)
# A tibble: 6 x 2
#recorded_by                  n
#<chr>                    <int>
#1 A. Carper                  114
#2 A. Hamblin                 948
#3 C.S. Brimley               468
#4 E. Youngsteadt             128
#5 H. Levenson & assistants    87
#6 T.B. Mitchell             2660

top_collectors_data_single_rm %>%
  count(day, month, year, recorded_by, decimal_latitude, decimal_longitude) %>%
  count(recorded_by)
# A tibble: 6 x 2
#recorded_by                  n
#<chr>                    <int>
#1 A. Carper                   35
#2 A. Hamblin                 195
#3 C.S. Brimley               320
#4 E. Youngsteadt              62
#5 H. Levenson & assistants    12
#6 T.B. Mitchell             1004

# set up the top collector data with singletons removed to work with NMDS####
collector_by_bee_spp_singl_rm <- top_collectors_data_single_rm %>%
  # to get the number of times each collector collected a species (since already split up by unique collection events)
  count(scientific_name, recorded_by) %>%
  rename(tally = n) %>%
  # need to move the bee species to columns with the tally as the values
  pivot_wider(names_from = scientific_name, values_from = tally) #%>%
  # need to rename the NA recorded by to unknown
  #mutate(recorded_by = if_else(is.na(recorded_by), "unknown", recorded_by))
collector_by_bee_spp_singl_rm


# need to fill in all the NAs with zero
collector_by_bee_spp_singl_rm[ is.na( collector_by_bee_spp_singl_rm ) ] <- 0



collector_sampling_effort_sing_rm <- top_collectors_data_single_rm %>%
  # to get the number of unique collection events
  count(day, month, year, recorded_by, decimal_latitude, decimal_longitude) %>%
  # to get the number of those events by each collector
  count(recorded_by) %>%
  rename(total_events = n) #%>%
  # need to rename the NA recorded by to unknown
  #mutate(recorded_by = if_else(is.na(recorded_by), "unknown", recorded_by))
collector_sampling_effort_sing_rm


# to get when the collectors were active
condensed_data %>%
  filter(recorded_by %in% top_collectors_list) %>%
  count(recorded_by, decade) %>%
  print(n = 50)
# contemporary: A. Carper (2000s); A. Hamblin (2010s); E. youngsteadt (2010s); H. Levenson & assistants (2010s)
# historic: C.S. Brimley (1900s-1980s); T.B. Mitchell (1900s-1980s)

condensed_data %>%
  filter(recorded_by == "C.S. Brimley") %>% #480 unique records
  count(recorded_by, decade)
# A tibble: 8 x 3
#recorded_by  decade     n
#<chr>        <chr>  <int>
#1 C.S. Brimley 1900s      6
#2 C.S. Brimley 1910s     12
#3 C.S. Brimley 1920s    354
#4 C.S. Brimley 1930s     94
#5 C.S. Brimley 1940s     10
#6 C.S. Brimley 1950s      2
#7 C.S. Brimley 1960s      1
#8 C.S. Brimley 1980s      1

condensed_data %>%
  filter(recorded_by == "T.B. Mitchell") %>% #2703 unique records
  count(recorded_by, decade)
# A tibble: 9 x 3
#recorded_by   decade     n
#<chr>         <chr>  <int>
#1 T.B. Mitchell 1900s      2
#2 T.B. Mitchell 1910s      3
#3 T.B. Mitchell 1920s   1068
#4 T.B. Mitchell 1930s    284
#5 T.B. Mitchell 1940s    670
#6 T.B. Mitchell 1950s    484
#7 T.B. Mitchell 1960s    180
#8 T.B. Mitchell 1970s      2
#9 T.B. Mitchell 1980s     10


# join the sampling effort to the tally datafile

joined_collector_by_spp_tally_sing_rm <- collector_sampling_effort_sing_rm %>%
  left_join(collector_by_bee_spp_singl_rm)

names(joined_collector_by_spp_tally_sing_rm) # 3:240
bee_spp_list_sing_rm <- names(joined_collector_by_spp_tally_sing_rm[3:240])

# need this to be a frequency table though
joined_collector_by_spp_freq_sing_rm <- joined_collector_by_spp_tally_sing_rm %>%
  # make it longer so that can get frequencies
  pivot_longer(bee_spp_list_sing_rm, names_to = "bee_spp") %>%
  mutate(freq = value/total_events) %>% 
  select(-value) %>%
  # make it wider again so that columns are the bee species
  pivot_wider(names_from = bee_spp, values_from = freq)
joined_collector_by_spp_freq_sing_rm



joined_collector_by_spp_freq_sing_rm <- joined_collector_by_spp_freq_sing_rm %>%
  mutate(time_period = if_else(recorded_by == "A. Carper" | 
                                 recorded_by == "A. Hamblin" |
                                 recorded_by == "E. Youngsteadt" |
                                 recorded_by == "H. Levenson & assistants", 
                               "contemporary", "historic"))

joined_collector_by_spp_freq_sing_rm %>%
  count(recorded_by, time_period)


# convert data to a matrix
names(joined_collector_by_spp_freq_sing_rm) # 3:241

collector_mat_sing_rm <- as.matrix(joined_collector_by_spp_freq_sing_rm[,3:240])
dimnames(collector_mat_sing_rm)
collector_names_mat_sing_rm <- as.vector(joined_collector_by_spp_freq_sing_rm$recorded_by)
dimnames(collector_mat_sing_rm)[[1]] <- c(collector_names_mat_sing_rm)
head(collector_mat_sing_rm)

## to check if there are any na's anywhere in the dataset
csum_mat_sing_rm <- colSums(collector_mat_sing_rm)
any(is.na(csum_mat_sing_rm)) # FALSE


## -- make the initial NMDS ####
## will comment out to prevent overwriting in future

#collectors_NMDS_2dim_sing_rm <- metaMDS(collector_mat_sing_rm, distance = "bray", k = 2,
#                                        trymax = 50, autotransform = TRUE, pc = TRUE)

#collectors_NMDS_3dim_sing_rm <- metaMDS(collector_mat_sing_rm, distance = "bray", k = 3, 
#                                        trymax = 50, autotransform = TRUE, pc = TRUE)

collectors_NMDS_3dim_sing_rm$stress # 0
collectors_NMDS_2dim_sing_rm$stress # 0
# stress is very low

## -- save the metaMDS output ####
#save(collectors_NMDS_2dim_sing_rm, 
#     file = paste(Rsource_path, "recorded_by_NMDS_2dim_sing_rm.rda", sep = "/"))

## -- load the saved Rdata metaMDS output 
load(paste(Rsource_path,"recorded_by_NMDS_2dim_sing_rm.rda", sep = "/"))

stressplot(collectors_NMDS_2dim_sing_rm)
collectors_NMDS_2dim_sing_rm


# run a perMANOVA ####
collector.dist_sing_rm <- vegdist(collector_mat_sing_rm, method = "bray")

# null hypothesis 
# groups do not differ in spread of position in multivariate space

collector_spp_perMANOVA_time_period_sing_rm <- adonis2(collector.dist_sing_rm~time_period,
                                                       data = joined_collector_by_spp_freq_sing_rm,
                                                       permutations = 999, method = "bray", strata = recorded_by)

collector_spp_perMANOVA_time_period_sing_rm # time period is signifance
#Permutation test for adonis under reduced model
#Terms added sequentially (first to last)
#Permutation: free
#Number of permutations: 719
#
#adonis2(formula = collector.dist_sing_rm ~ time_period, data = joined_collector_by_spp_freq_sing_rm, permutations = 999, method = "bray", strata = recorded_by)
#             Df SumOfSqs      R2      F  Pr(>F)  
#time_period  1  0.58979 0.43635 3.0966 0.06667 .
#Residual     4  0.76186 0.56365                 
#Total        5  1.35165 1.00000                 
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## -- make the NMDS figure of the collector bias ####
collector_NMDS1_sing_rm <- collectors_NMDS_2dim_sing_rm$points[,1]
collector_NMDS2_sing_rm <- collectors_NMDS_2dim_sing_rm$points[,2]
collector_species1_sing_rm <- collectors_NMDS_2dim_sing_rm$species[,1]
collector_species2_sing_rm <- collectors_NMDS_2dim_sing_rm$species[,2]
collector_graphing_file_sing_rm <- cbind(joined_collector_by_spp_freq_sing_rm, collector_NMDS1_sing_rm, collector_NMDS2_sing_rm)


collector_species_graphing_sing_rm <- cbind(collector_species1_sing_rm,collector_species2_sing_rm)
collector_level_sing_rm <- as.data.table(dimnames(collector_species_graphing_sing_rm)[[1]])

## fitting environmental conditions on it
#fit_collection_sing_rm <- envfit(collectors_NMDS_2dim_sing_rm, collector_mat_sing_rm)

## -- save the metaMDS output
#save(fit_collection_sing_rm, 
#     file = paste(Rsource_path, "fit_collection_sing_rm.rda", sep = "/"))

## -- load the saved Rdata metaMDS output 
load(paste(Rsource_path,"fit_collection_sing_rm.rda", sep = "/"))

arrow_sing_rm <- data.frame(fit_collection_sing_rm$vectors$arrows,R=fit_collection_sing_rm$vectors$r,
                            P=fit_collection_sing_rm$vectors$pvals)

arrow_sing_rm$FG <- rownames(arrow_sing_rm)
arrow.p_sing_rm<-subset(arrow_sing_rm, P <= 0.05)
nrow(arrow.p_sing_rm) #25
arrow.p_sing_rm
#                             NMDS1       NMDS2         R           P                  FG
#Agapostemon virescens    -0.82691861  0.5623216 0.7063300 0.020833333    Agapostemon virescens
#Andrena andrenoides       0.89988032  0.4361369 0.8226098 0.033333333      Andrena andrenoides
#Andrena erigeniae         0.89988032  0.4361369 0.8226098 0.033333333        Andrena erigeniae
#Andrena macra             0.02990926 -0.9995526 0.8738337 0.041666667            Andrena macra
#Andrena nida              0.89988032  0.4361369 0.8226098 0.033333333             Andrena nida
#Anthidium maculifrons     0.85594848  0.5170611 0.8281343 0.033333333    Anthidium maculifrons
#Apis mellifera           -0.99706385  0.0765747 0.8027328 0.047222222           Apis mellifera
#Bombus pensylvanicus     -0.80088723  0.5988152 0.6272487 0.019444444     Bombus pensylvanicus
#Calliopsis andreniformis  0.16884378 -0.9856428 0.9096449 0.047222222 Calliopsis andreniformis
#Ceratina strenua          0.09557331 -0.9954224 0.9758051 0.013888889         Ceratina strenua
#Colletes brevicornis      0.85594848  0.5170611 0.8281343 0.033333333     Colletes brevicornis
#Dianthidium curvatum      0.89988032  0.4361369 0.8226098 0.033333333     Dianthidium curvatum
#Epeolus pusillus          0.89988032  0.4361369 0.8226098 0.033333333         Epeolus pusillus
#Eucera pruinosa           0.37260241  0.9279911 0.8320176 0.033333333          Eucera pruinosa
#Halictus confusus        -0.84550007  0.5339753 0.9793438 0.001388889        Halictus confusus
#Halictus ligatus/poeyi   -0.83914099  0.5439140 0.7343523 0.009722222   Halictus ligatus/poeyi
#Heriades carinata        -0.06489490 -0.9978921 0.8028950 0.050000000        Heriades carinata
#Lasioglossum coreopsis    0.20294394 -0.9791904 0.9588991 0.013888889   Lasioglossum coreopsis
#Macropis steironematis    0.89988032  0.4361369 0.8226098 0.033333333   Macropis steironematis
#Megachile mendica        -0.95555483  0.2948134 0.8250629 0.013888889        Megachile mendica
#Megachile petulans       -0.87193875  0.4896150 0.9240931 0.012500000       Megachile petulans
#Nomada articulata         0.87181269  0.4898394 0.8278205 0.033333333        Nomada articulata
#Nomada ovata              0.87181269  0.4898394 0.8278205 0.033333333             Nomada ovata
#Paranthidium jugatorium   0.82977478  0.5580984 0.8250221 0.033333333  Paranthidium jugatorium
#Svastra obliqua          -0.72017457  0.6937929 0.7056582 0.019444444          Svastra obliqua

sig_bee_spp_sing_rm <- as.vector(unique(arrow.p_sing_rm$FG))

collector_species_graphing_sing_rm <- as.data.frame(collector_species_graphing_sing_rm)

collector_species_graphing_subset_sing_rm <- collector_species_graphing_sing_rm %>%
  rownames_to_column() %>%
  filter(rowname %in% sig_bee_spp_sing_rm) %>%
  rename(NMDS1 = collector_species1_sing_rm) %>%
  rename(NMDS2 = collector_species2_sing_rm) %>%
  mutate(label = seq(1:25))



collector_graphing_file_sing_rm$time_period


collector_graphing_file_subset_NS_sing_rm <- collector_species_graphing_sing_rm %>%
  rownames_to_column() %>%
  filter(rowname %nin% sig_bee_spp_sing_rm) %>%
  rename(NMDS1 = collector_species1_sing_rm) %>%
  rename(NMDS2 = collector_species2_sing_rm)


collector_graphing_file2_sing_rm <- collector_graphing_file_sing_rm %>%
  mutate(collector = if_else(recorded_by == "A. Carper", "AC", recorded_by)) %>%
  mutate(collector = if_else(recorded_by == "A. Hamblin", "AH", collector)) %>%
  mutate(collector = if_else(recorded_by == "C.S. Brimley", "CSB", collector)) %>%
  mutate(collector = if_else(recorded_by == "E. Youngsteadt", "EY", collector)) %>%
  mutate(collector = if_else(recorded_by == "H. Levenson & assistants", "HL", collector)) %>%
  mutate(collector = if_else(recorded_by == "T.B. Mitchell", "TBM", collector)) 

# make the figure ####

NMDS_top_collectors_spp_color_blind_sing_rm <- ggplot(collector_graphing_file2_sing_rm, 
                                                      aes(x=collector_NMDS1_sing_rm,y=collector_NMDS2_sing_rm, 
                                                          fill=time_period, colour=time_period))+ 
  geom_text(inherit.aes = TRUE, label = collector_graphing_file2_sing_rm$collector, size = 4) +
  #geom_point()+ #position=position_jitter(.1), shape=3
  #geom_text(inherit.aes = FALSE, data = collector_species_graphing_subset, 
  #          aes(x = NMDS1, y = NMDS2), label = collector_species_graphing_subset$rowname,
  #          size = 4) + # too many species to really make this practical, would be better as a separate table
  geom_point(inherit.aes = FALSE, data = collector_graphing_file_subset_NS_sing_rm,
             aes(x = NMDS1, y = NMDS2), 
             shape = 4, size = 1) + # the non sig species (x sumbol)
  geom_text(inherit.aes = FALSE, data = collector_species_graphing_subset_sing_rm, 
             aes(x = NMDS1, y = NMDS2, label = label)) + # the sig species (the numbers points)
  #geom_point(inherit.aes = FALSE, data = collector_species_graphing_subset_sing_rm, 
  #           aes(x = NMDS1, y = NMDS2)) + # the sig species (normal points)
  #geom_text(data=pitfalls_graphing_file, 
  #          aes(x=pitfalls_NMDS1,y=pitfalls_NMDS2, 
  #              label=Plot_abbre))+
  #geom_text(data=plot_level_species_graphing,
  #          aes(x=plot_level_species1,y=plot_level_species2,
  #              label=species_names_plot_level))+
  xlab("NMDS 1")+
  ylab("NMDS 2")+
  annotate("text", x=min(collector_NMDS1_sing_rm), y=min(collector_NMDS2_sing_rm)-.5,
           label=paste('Stress =',round(collectors_NMDS_2dim_sing_rm$stress,3)))+
  #stat_ellipse(type='t',size=1, geom="polygon", alpha=.2)+ #t assumes multivariet t distribution
  scale_fill_manual(values=c("#0072B2","#D55E00"),
                    breaks=c("contemporary","historic"),
                    labels=c("Contemporary","Historic"))+
  scale_colour_manual(values=c("#0072B2","#D55E00"),
                      breaks=c("contemporary","historic"),
                      labels=c("Contemporary","Historic"))+
  theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
        panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
        panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
        panel.border=element_rect(colour="black", fill=NA),
        axis.ticks=element_line(colour = 'black', size = 0.5), #sets the tick lines
        legend.position="top",
        legend.title=element_blank(),
        legend.key=element_blank(), #remove the border around each item
        axis.title.x = element_text(face="bold", size=14, color="black"), #size of x-axis title
        axis.title.y = element_text(face="bold", size=14, color="black"), #size of y-axis title
        axis.text.x = element_text(size=12, color="black"), #size of x-axis text
        axis.text.y = element_text(size=12, color="black"))#size of y-axis text
NMDS_top_collectors_spp_color_blind_sing_rm

NMDS_top_collectors_spp_color_blind_sing_rm_ZOOM <- ggplot(collector_graphing_file2_sing_rm, 
                     aes(x=collector_NMDS1_sing_rm,y=collector_NMDS2_sing_rm, 
                         fill=time_period, colour=time_period))+ 
  geom_text(inherit.aes = TRUE, label = collector_graphing_file2_sing_rm$collector, size = 0.5) +
  #geom_point()+ #position=position_jitter(.1), shape=3
  #geom_text(inherit.aes = FALSE, data = collector_species_graphing_subset, 
  #          aes(x = NMDS1, y = NMDS2), label = collector_species_graphing_subset$rowname,
  #          size = 4) + # too many species to really make this practical, would be better as a separate table
  xlim(1.25, 1.35) +
  ylim(0, 0.5) +
  geom_point(inherit.aes = FALSE, data = collector_graphing_file_subset_NS_sing_rm,
             aes(x = NMDS1, y = NMDS2), 
             shape = 4, size = .05) + # the non sig species (x sumbol)
  geom_text(inherit.aes = FALSE, data = collector_species_graphing_subset_sing_rm, 
            aes(x = NMDS1, y = NMDS2, label = label), size = 3, position=position_jitter(width = 0.015, height = 0.015)) + # the sig species (the numbers points)
  #geom_point(inherit.aes = FALSE, data = collector_species_graphing_subset_sing_rm, 
  #           aes(x = NMDS1, y = NMDS2)) + # the sig species (normal points)
  #geom_text(data=pitfalls_graphing_file, 
  #          aes(x=pitfalls_NMDS1,y=pitfalls_NMDS2, 
  #              label=Plot_abbre))+
  #geom_text(data=plot_level_species_graphing,
  #          aes(x=plot_level_species1,y=plot_level_species2,
  #              label=species_names_plot_level))+
  xlab("")+
  ylab("")+
  #annotate("text", x=min(collector_NMDS1_sing_rm), y=min(collector_NMDS2_sing_rm)-.5,
  #         label=paste('Stress =',round(collectors_NMDS_2dim_sing_rm$stress,3)))+
  #stat_ellipse(type='t',size=1, geom="polygon", alpha=.2)+ #t assumes multivariet t distribution
  scale_fill_manual(values=c("#0072B2","#D55E00"),
                    breaks=c("contemporary","historic"),
                    labels=c("Contemporary","Historic"))+
  scale_colour_manual(values=c("#0072B2","#D55E00"),
                      breaks=c("contemporary","historic"),
                      labels=c("Contemporary","Historic"))+
  theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
        panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
        panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
        panel.border=element_rect(colour="black", fill=NA),
        axis.ticks=element_line(colour = 'black', size = 0.5), #sets the tick lines
        legend.position="",
        legend.title=element_blank(),
        legend.key=element_blank(), #remove the border around each item
        axis.title.x = element_text(face="bold", size=14, color="black"), #size of x-axis title
        axis.title.y = element_text(face="bold", size=14, color="black"), #size of y-axis title
        axis.text.x = element_text(size=10, color="black"), #size of x-axis text
        axis.text.y = element_text(size=10, color="black"))#size of y-axis text
NMDS_top_collectors_spp_color_blind_sing_rm_ZOOM



# adding in arrows

ggplot(collector_graphing_file2_sing_rm, 
       aes(x=collector_NMDS1_sing_rm,y=collector_NMDS2_sing_rm, 
           fill=time_period, colour=time_period))+ 
  geom_text(inherit.aes = TRUE, label = collector_graphing_file2_sing_rm$collector, size = 4) +
  #geom_point()+ #position=position_jitter(.1), shape=3
  #geom_text(inherit.aes = FALSE, data = collector_species_graphing_subset, 
  #          aes(x = NMDS1, y = NMDS2), label = collector_species_graphing_subset$rowname,
  #          size = 4) + # too many species to really make this practical, would be better as a separate table
  geom_point(inherit.aes = FALSE, data = collector_graphing_file_subset_NS_sing_rm,
             aes(x = NMDS1, y = NMDS2), 
             shape = 4, size = 1) + # the non sig species (x sumbol)
  geom_point(inherit.aes = FALSE, data = collector_species_graphing_subset_sing_rm, 
             aes(x = NMDS1, y = NMDS2)) + # the sig species (normal points)
  #geom_text(data=pitfalls_graphing_file, 
  #          aes(x=pitfalls_NMDS1,y=pitfalls_NMDS2, 
  #              label=Plot_abbre))+
  #geom_text(data=plot_level_species_graphing,
  #          aes(x=plot_level_species1,y=plot_level_species2,
  #              label=species_names_plot_level))+
  xlab("NMDS 1")+
  ylab("NMDS 2")+
  geom_segment(data=arrow.p_sing_rm, aes(x=0, y=0, xend=NMDS1, yend=NMDS2, lty=FG), #lty=FG, colour=FG
               arrow=arrow(length=unit(.2,"cm")*arrow.p_sing_rm$R), inherit.aes=FALSE)+
  geom_dl(data=arrow.p_sing_rm, aes(x=NMDS1-.05, y=NMDS2+0.1, label = FG), #colour=FG
          method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.60),
          inherit.aes=FALSE) +
  annotate("text", x=min(collector_NMDS1_sing_rm), y=min(collector_NMDS2_sing_rm)-.5,
           label=paste('Stress =',round(collectors_NMDS_2dim_sing_rm$stress,3)))+
  #stat_ellipse(type='t',size=1, geom="polygon", alpha=.2)+ #t assumes multivariet t distribution
  scale_fill_manual(values=c("#0072B2","#D55E00"),
                    breaks=c("contemporary","historic"),
                    labels=c("Contemporary","Historic"))+
  scale_colour_manual(values=c("#0072B2","#D55E00"),
                      breaks=c("contemporary","historic"),
                      labels=c("Contemporary","Historic"))+
  theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
        panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
        panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
        panel.border=element_rect(colour="black", fill=NA),
        axis.ticks=element_line(colour = 'black', size = 0.5), #sets the tick lines
        legend.position="top",
        legend.title=element_blank(),
        legend.key=element_blank(), #remove the border around each item
        axis.title.x = element_text(face="bold", size=14, color="black"), #size of x-axis title
        axis.title.y = element_text(face="bold", size=14, color="black"), #size of y-axis title
        axis.text.x = element_text(size=12, color="black"), #size of x-axis text
        axis.text.y = element_text(size=12, color="black"))#size of y-axis text


# species at the far right of nmds ####
far_right_sing_rm <- collector_species_graphing_sing_rm %>%
  rownames_to_column() %>%
  filter(collector_species1_sing_rm > 1.3)

far_right_sing_rm_spp_list <- as.vector(far_right_sing_rm$rowname)

top_collectors_data %>%
  mutate(recorded_by = if_else(is.na(recorded_by), "unknown", recorded_by)) %>%
  mutate(time_period = if_else(recorded_by == "A. Carper" | 
                                 recorded_by == "A. Hamblin" |
                                 recorded_by == "E. Youngsteadt" |
                                 recorded_by == "H. Levenson & assistants", 
                               "contemporary", "historic")) %>%
  #count(scientific_name, recorded_by)
  count(scientific_name, time_period) %>%
  pivot_wider(names_from = time_period, values_from = n) %>%
  filter(scientific_name %in% far_right_sing_rm_spp_list) #%>% print(n = 200)
  count(contemporary) # all on that list are only collected historically except for 1 (Augochlorella gratiosa)
# which has one collection record in contemporary times

# save the figure ####
#ggsave("NMDS_species_top__color_blind_singletons_removed.png", width = 6, height = 4,
#units = "in", dpi = 600, plot = NMDS_top_collectors_spp_color_blind_sing_rm,
#       path = figure_path, family = "Arial")

#ggsave("NMDS_species_top__color_blind_singletons_removed_ZOOMED.png", width = 3, height = 3,
#units = "in", dpi = 600, plot = NMDS_top_collectors_spp_color_blind_sing_rm_ZOOM,
#       path = figure_path, family = "Arial")
