# R script for additiona multiseason models - RPresence trends

# load libraries ####
library(here)
library(tidyverse)
library(naniar)
library(RPresence)

# set relative pathways ####
# - these pathways will not be useful for reading data in since I'm not sure where the data is saved
# on your computer
data_path <- here::here("data")
figure_path <- here::here("figs")
Rsource_path <- here::here("R/Resources")


# focal primary periods ####
focal_periods1 <- c(seq(1909, 1913, 1),
                    seq(1924, 1928, 1),
                    seq(1939, 1943, 1),
                    seq(1954, 1958, 1),
                    seq(1969, 1973, 1),
                    seq(1984, 1988, 1),
                    seq(1999, 2003, 1),
                    seq(2014, 2018, 1))
focal_periods1


### -- Wake Co data ####

load(paste(Rsource_path,"presence_subset_data1.rda", sep = "/"))

presence_subset_data1

# - pull out detection history
dethist1 <- presence_subset_data1[,2:ncol(presence_subset_data1)]

# - create input "pao" object, for use wit occMod function
bees1 <- createPao(data = dethist1,
                   nsurveyseason = rep(5,8), # 5 surveys/season for 8 seasons
                   title = "Bees multispecies multiseason")

# - set up linear covariate
# need to create a linear covariate for season
# following example from http://www.phidot.org/forum/viewtopic.php?f=57&t=3924&p=12982&hilit=seasonal+covariate#p12982

# want a linear covariate, so for each transition between season want in the matrix for PRESENCE would want 1,2,3,4,5,6,7

urbancov1 <- rep(1, bees1$nunits)
urbancov2 <- rep(2, bees1$nunits)
urbancov3 <- rep(3, bees1$nunits)
urbancov4 <- rep(4, bees1$nunits)
urbancov5 <- rep(5, bees1$nunits)
urbancov6 <- rep(6, bees1$nunits)
urbancov7 <- rep(7, bees1$nunits)

newcov <- data.frame(urban=c(urbancov1, urbancov2, urbancov3, urbancov4,
                             urbancov5, urbancov6, urbancov7))

# - set up quadratic covariate
# so for each transition between season in the matrix for PRESENCE would want 1, 4, 9, 16, 25, 36, 49

quadcov1 <- rep(1, bees1$nunits)
quadcov2 <- rep(4, bees1$nunits)
quadcov3 <- rep(9, bees1$nunits)
quadcov4 <- rep(16, bees1$nunits)
quadcov5 <- rep(25, bees1$nunits)
quadcov6 <- rep(36, bees1$nunits)
quadcov7 <- rep(49, bees1$nunits)

quadnewcov <- data.frame(quadratic_cov=c(quadcov1, quadcov2, quadcov3, quadcov4,
                             quadcov5, quadcov6, quadcov7))


## Run different models
#psi,gamma(),eps(),p()
#psi_gam_eps_det <- occMod(data = bees1, type = "do.1",
#                     model= list(psi~1, p~1, gamma~1, epsilon~1),
#                     outfile='modname')


## -- save the PRESENCE output
#save(psi_gam_eps_det, 
#     file = paste(Rsource_path, "Wake_psi_gam_eps_det.rda", sep = "/"))
load(paste(Rsource_path,"Wake_psi_gam_eps_det.rda", sep = "/"))
print(summary(psi_gam_eps_det)) # print model summary
# AIC = 5950.5657
# parameter estimates converged to approximately 7 significant digits


#psi,gamma(),eps(),p(S)
#psi_gam_eps_detSeason <- occMod(data = bees1, type = "do.1",
#                          model= list(psi~1, p~SEASON, gamma~1, epsilon~1),
#                          outfile='modname')

## -- save the PRESENCE output
#save(psi_gam_eps_detSeason, 
#     file = paste(Rsource_path, "Wake_psi_gam_eps_detSeason.rda", sep = "/"))
load(paste(Rsource_path,"Wake_psi_gam_eps_detSeason.rda", sep = "/"))
print(summary(psi_gam_eps_detSeason)) # print model summary
# AIC = 5560.2062
# parameter estimates converged to approximately 7 significant digits

#psi,gamma(S),eps(.),p(S)
#psi_gamSeason_eps_detSeason <- occMod(data = bees1, type = "do.1",
#                          model= list(psi~1, p~SEASON, gamma~SEASON, epsilon~1),
#                          outfile='modname')

## -- save the PRESENCE output
#save(psi_gamSeason_eps_detSeason, 
#     file = paste(Rsource_path, "Wake_psi_gamSeason_eps_detSeason.rda", sep = "/"))
load(paste(Rsource_path,"Wake_psi_gamSeason_eps_detSeason.rda", sep = "/"))
print(summary(psi_gamSeason_eps_detSeason)) # print model summary
# AIC = 5542.1539
# parameter estimates converged to approximately 5.51 significant digits


#psi,gamma(.),eps(S),p(S)
#psi_gam_epsSeason_detSeason <- occMod(data = bees1, type = "do.1",
#                          model= list(psi~1, p~SEASON, gamma~1, epsilon~SEASON),
#                          outfile='modname')

## -- save the PRESENCE output
#save(psi_gam_epsSeason_detSeason, 
#     file = paste(Rsource_path, "Wake_psi_gam_epsSeason_detSeason.rda", sep = "/"))
load(paste(Rsource_path,"Wake_psi_gam_epsSeason_detSeason.rda", sep = "/"))
print(summary(psi_gam_epsSeason_detSeason)) # print model summary
# AIC = 5552.449
# parameter estimates converged to approximately 4.65 significant digits
# warnings produced

#psi,gamma(S),eps(S),p(S) -- Model currently highlighted in the main text


load(paste(Rsource_path,"Model_1_psi_gamSeason_epsSeason_detSeason.rda", sep = "/"))
print(summary(model1_psi_gamSeason_epsSeason_detSeason)) # print model summary
# AIC = 5512.48
# parameter estimates converged to approximately 5.45 significant digits

print(summary(model1_psi_gamSeason_epsSeason_detSeason))

print(unique(model1_psi_gamSeason_epsSeason_detSeason$real$psi))
print(unique(model1_psi_gamSeason_epsSeason_detSeason$derived$psi)) # print seasonal occupancy estimates
print(unique(model1_psi_gamSeason_epsSeason_detSeason$real$p))
print(unique(model1_psi_gamSeason_epsSeason_detSeason$real$gamma))
print(unique(model1_psi_gamSeason_epsSeason_detSeason$real$epsilon))


gamma_quad_Wake_season <- model1_psi_gamSeason_epsSeason_detSeason$beta$gamma
gamma_quad_Wake_season <- gamma_quad_Wake_season %>%
  mutate(SD = se * 1.96) %>%
  mutate(lower_CI = est - SD) %>%
  mutate(upper_CI = est + SD)
gamma_quad_Wake_season
#        est       se         SD    lower_CI    upper_CI
#1 -0.451148 0.220705  0.4325818  -0.8837298 -0.01856620
#2 -1.051432 0.354872  0.6955491  -1.7469811 -0.35588288
#3 -0.582588 0.290008  0.5684157  -1.1510037 -0.01417232
#4 -5.702141 6.342350 12.4310060 -18.1331470  6.72886500
#5 -2.659218 0.725411  1.4218056  -4.0810236 -1.23741244
#6 -1.600239 2.015166  3.9497254  -5.5499644  2.34948636
#7 -1.374007 1.701090  3.3341364  -4.7081434  1.96012940

epsilon_quad_Wake_season <- model1_psi_gamSeason_epsSeason_detSeason$beta$epsilon
epsilon_quad_Wake_season <- epsilon_quad_Wake_season %>%
  mutate(SD = se * 1.96) %>%
  mutate(lower_CI = est - SD) %>%
  mutate(upper_CI = est + SD)
epsilon_quad_Wake_season
#         est           se           SD      lower_CI      upper_CI
#1  -1.881907     0.728879     1.428603 -3.310510e+00    -0.4533042
#2   1.541259     0.773005     1.515090  2.616920e-02     3.0563488
#3   0.913970     0.769743     1.508696 -5.947263e-01     2.4226663
#4   2.889867     0.822660     1.612414  1.277453e+00     4.5022806
#5 -20.565676 23919.817947 46882.843176 -4.690341e+04 46862.2775001
#6  -0.488483     1.252290     2.454488 -2.942971e+00     1.9660054
#7 -15.582780  4381.245398  8587.240980 -8.602824e+03  8571.6582001


wake_season_eps <- unique(model1_psi_gamSeason_epsSeason_detSeason$real$epsilon)
wake_season_eps$dynamic <- "Extinction"
wake_season_eps$transition_num <- seq(1,7,1)

wake_season_gam <- unique(model1_psi_gamSeason_epsSeason_detSeason$real$gamma)
wake_season_gam$dynamic <- "Colonization"
wake_season_gam$transition_num <- seq(1,7,1)

wake_season_dynamics_plotting <- wake_season_eps %>%
  full_join(wake_season_gam)
wake_season_dynamics_plotting


wake_season_dynamics_graph <- ggplot(data = wake_season_dynamics_plotting, aes(x = transition_num, y = est, color = dynamic)) +
  geom_point(size = 2) +
  ylab("Estimate") +
  xlab("Transition Number") +
  scale_x_continuous(breaks=seq(1, 7, 1)) +
  geom_segment(aes(x = transition_num, xend = transition_num, y = lower_0.95,
                   yend = upper_0.95))+
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
        axis.text.x = element_text(size=12, color="black"), #size of x-axis text
        axis.text.y = element_text(size=12, color="black"))#size of y-axis text
wake_season_dynamics_graph

#ggsave("wake_season_dynamics_graph.png", width = 6, height = 3,
#       units = "in", dpi = 600, plot = wake_season_dynamics_graph,
#       path = figure_path, family = "Arial")


#psi,gamma(),eps(linear trend),p(S)
#psi_gam_epsLinear_detSeason <- occMod(data = bees1, type = "do.1",
#                          model= list(psi~1, p~SEASON, gamma~1, epsilon~1+urban),
#                          cov.list=list(epsilon.cov=newcov),
#                          outfile='modname')

## -- save the PRESENCE output
#save(psi_gam_epsLinear_detSeason, 
#     file = paste(Rsource_path, "Wake_psi_gam_epsLinear_detSeason.rda", sep = "/"))
load(paste(Rsource_path,"Wake_psi_gam_epsLinear_detSeason.rda", sep = "/"))
print(summary(psi_gam_epsLinear_detSeason)) # print model summary
# AIC = 5561.4285
# parameter estimates converged to approximately 5.88 significant digits
# warnings produced

#psi,gamma(linear trend),eps(.),p(S)
#psi_gamLinear_eps_detSeason <- occMod(data = bees1, type = "do.1",
#                                      model= list(psi~1, p~SEASON, gamma~1+urban, epsilon~1),
#                                      cov.list=list(gamma.cov=newcov),
#                                      outfile='modname')

## -- save the PRESENCE output
#save(psi_gamLinear_eps_detSeason, 
#     file = paste(Rsource_path, "Wake_psi_gamLinear_eps_detSeason.rda", sep = "/"))
load(paste(Rsource_path,"Wake_psi_gamLinear_eps_detSeason.rda", sep = "/"))
print(summary(psi_gamLinear_eps_detSeason)) # print model summary
# AIC = 5550.7353
# parameter estimates converged to approximately 5.38 significant digits

#psi,gamma(linear trend),eps(linear trend),p(S)
#psi_gamLinear_epsLinear_detSeason <- occMod(data = bees1, type = "do.1",
#                                      model= list(psi~1, p~SEASON, gamma~1+urban, epsilon~1+urban),
#                                      cov.list=list(epsilon.cov=newcov, gamma.cov=newcov),
#                                      outfile='modname')

## -- save the PRESENCE output
#save(psi_gamLinear_epsLinear_detSeason, 
#     file = paste(Rsource_path, "Wake_psi_gamLinear_epsLinear_detSeason.rda", sep = "/"))
load(paste(Rsource_path,"Wake_psi_gamLinear_epsLinear_detSeason.rda", sep = "/"))
print(summary(psi_gamLinear_epsLinear_detSeason)) # print model summary
# AIC = 5542.7835
# parameter estimates converged to approximately 5.46 significant digits

#psi,gamma(),eps(quadratic trend),p(S)
#psi_gam_epsQuad_detSeason <- occMod(data = bees1, type = "do.1",
#                                      model= list(psi~1, p~SEASON, gamma~1, epsilon~1+newcov$urban+quadnewcov$quadratic_cov),
#                                      cov.list=list(epsilon.cov=newcov, epsilon.cov=quadnewcov),
#                                      outfile='modname')

## -- save the PRESENCE output
#save(psi_gam_epsQuad_detSeason, 
#     file = paste(Rsource_path, "Wake_psi_gam_epsQuad_detSeason.rda", sep = "/"))
load(paste(Rsource_path,"Wake_psi_gam_epsQuad_detSeason.rda", sep = "/"))
print(summary(psi_gam_epsQuad_detSeason)) # print model summary
# AIC = 5559.5934
# parameter estimates converged to approximately 5.27 significant digits

#psi,gamma(quadratic trend),eps(),p(S)
#psi_gamQuad_eps_detSeason <- occMod(data = bees1, type = "do.1",
#                                    model= list(psi~1, p~SEASON, gamma~1+newcov$urban+quadnewcov$quadratic_cov, epsilon~1),
#                                    cov.list=list(gamma.cov=newcov, gamma.cov=quadnewcov),
#                                    outfile='modname')

## -- save the PRESENCE output
#save(psi_gamQuad_eps_detSeason, 
#     file = paste(Rsource_path, "Wake_psi_gamQuad_eps_detSeason.rda", sep = "/"))
load(paste(Rsource_path,"Wake_psi_gamQuad_eps_detSeason.rda", sep = "/"))
print(summary(psi_gamQuad_eps_detSeason)) # print model summary
# AIC = 5548.679
# parameter estimates converged to approximately 7 significant digits

#psi,gamma(quadratic trend),eps(quadratic trend),p(S)
#psi_gamQuad_epsQuad_detSeason <- occMod(data = bees1, type = "do.1",
#                                    model= list(psi~1, p~SEASON, gamma~1+newcov$urban+quadnewcov$quadratic_cov, epsilon~1+newcov$urban+quadnewcov$quadratic_cov),
#                                    cov.list=list(gamma.cov=newcov, gamma.cov=quadnewcov, 
#                                                  epsilon.cov=newcov, epsilon.cov=quadnewcov),
#                                    outfile='modname')

## -- save the PRESENCE output
#save(psi_gamQuad_epsQuad_detSeason, 
#     file = paste(Rsource_path, "Wake_psi_gamQuad_epsQuad_detSeason.rda", sep = "/"))
load(paste(Rsource_path,"Wake_psi_gamQuad_epsQuad_detSeason.rda", sep = "/"))
print(summary(psi_gamQuad_epsQuad_detSeason)) # print model summary
# AIC = 5537.505
# parameter estimates converged to approximately 4.3 significant digits
#warnings produced

print(summary(psi_gamQuad_epsQuad_detSeason))

print(unique(psi_gamQuad_epsQuad_detSeason$real$psi))
#               est         se lower_0.95 upper_0.95
#unit1_1 0.2038257 NaN        NaN        NaN
print(unique(psi_gamQuad_epsQuad_detSeason$derived$psi)) # print seasonal occupancy estimates
#               est         se lower_0.95 upper_0.95
#unit1_2 0.4590532        NaN        NaN        NaN
#unit1_3 0.4544352        NaN        NaN        NaN
#unit1_4 0.3860681        NaN        NaN        NaN
#unit1_5 0.3328594        NaN        NaN        NaN
#unit1_6 0.3130779        NaN        NaN        NaN
#unit1_7 0.3214337        NaN        NaN        NaN
#unit1_8 0.3445898 0.02244518  0.3020212  0.3898075
print(unique(psi_gamQuad_epsQuad_detSeason$real$p))
#                 est         se  lower_0.95 upper_0.95
#p1_unit1  0.09572985         NaN         NaN        NaN
#p6_unit1  0.35169727         NaN         NaN        NaN
#p11_unit1 0.30462770         NaN         NaN        NaN
#p16_unit1 0.40499189         NaN         NaN        NaN
#p21_unit1 0.03322960         NaN         NaN        NaN
#p26_unit1 0.06461788         NaN         NaN        NaN
#p31_unit1 0.01916166 0.012170956 0.005460545 0.06499383
#p36_unit1 0.42810718 0.009520134 0.409558670 0.44685997
print(unique(psi_gamQuad_epsQuad_detSeason$real$gamma))
#                    est          se lower_0.95 upper_0.95
#gamma1_unit1 0.37273018 NaN        NaN        NaN
#gamma2_unit1 0.25446591 NaN        NaN        NaN
#gamma3_unit1 0.17294551 NaN        NaN        NaN
#gamma4_unit1 0.12021434 NaN        NaN        NaN
#gamma5_unit1 0.08694995 NaN        NaN        NaN
#gamma6_unit1 0.06610856 NaN        NaN        NaN
#gamma7_unit1 0.05314037 NaN        NaN        NaN
print(unique(psi_gamQuad_epsQuad_detSeason$real$epsilon))
#                      est         se   lower_0.95 upper_0.95
#epsilon1_unit1 0.20375608 NaN        NaN        NaN
#epsilon2_unit1 0.30992157 NaN        NaN        NaN
#epsilon3_unit1 0.35807110 NaN        NaN        NaN
#epsilon4_unit1 0.32898883 NaN        NaN        NaN
#epsilon5_unit1 0.23370027 NaN        NaN        NaN
#epsilon6_unit1 0.11835906 NaN        NaN        NaN
#epsilon7_unit1 0.04014262 NaN        NaN        NaN

gamma_quad_Wake_quadratic <- psi_gamQuad_epsQuad_detSeason$beta$gamma
gamma_quad_Wake_quadratic <- gamma_quad_Wake_quadratic %>%
  mutate(SD = se * 1.96) %>%
  mutate(lower_CI = est - SD) %>%
  mutate(upper_CI = est + SD)
gamma_quad_Wake_quadratic
#         est       se       SD  lower_CI upper_CI
#1  0.098342 NA NA       NA       NA
#2 -0.651090 NA NA       NA       NA
#3  0.032226 NA NA       NA       NA

epsilon_quad_Wake_quadratic <- psi_gamQuad_epsQuad_detSeason$beta$epsilon
epsilon_quad_Wake_quadratic <- epsilon_quad_Wake_quadratic %>%
  mutate(SD = se * 1.96) %>%
  mutate(lower_CI = est - SD) %>%
  mutate(upper_CI = est + SD)
epsilon_quad_Wake_quadratic
#         est       se        SD   lower_CI  upper_CI
#1 -2.271234 NA NA       NA       NA
#2  1.081130 NA NA       NA       NA
#3 -0.172878 NA NA       NA       NA


Wake_quad_initial_psi <- unique(psi_gamQuad_epsQuad_detSeason$real$psi)
Wake_quad_initial_psi$primary_period <- "1909-1913"
Wake_quad_derived_psi <- unique(psi_gamQuad_epsQuad_detSeason$derived$psi)
Wake_quad_derived_psi$primary_period <- c("1924-1928",
                                "1939-1943",
                                "1954-1958",
                                "1969-1973",
                                "1984-1988",
                                "1999-2003",
                                "2014-2018")

Wake_quad_estimates <- Wake_quad_initial_psi %>%
  full_join(Wake_quad_derived_psi) %>%
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
Wake_quad_estimates 
Wake_quad_estimates$pp_plotting <- as.numeric(c(1911,
                                                1926,
                                                1941,
                                                1956,
                                                1971,
                                                1986,
                                                2001,
                                                2016))

Wake_quad_estimates

wake_quad_graph <- ggplot(data = Wake_quad_estimates, aes(x = pp_plotting, y = spp_rich)) +
  geom_point(size = 2, color = "red") +
  ylab("Species Richness") +
  xlab("Year") +
  scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  ylim(0,225) +
  #scale_y_continuous(expand = c(0,0), breaks=seq(-25, 350, 25)) +

  geom_segment(data = Wake_quad_estimates, aes(x = pp_plotting, xend = pp_plotting, y = spp_rich_lower_0.95,
                                               yend = spp_rich_upper_0.95),
               color = "red") +
  geom_segment(data = Wake_quad_estimates, aes(x = pp_plotting - 2.5, xend = pp_plotting + 2.5, y = spp_rich,
                                               yend = spp_rich),
               color = "red") +
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
wake_quad_graph

wake_quad_eps <- unique(psi_gamQuad_epsQuad_detSeason$real$epsilon)
wake_quad_eps$dynamic <- "Extinction"
wake_quad_eps$transition_num <- seq(1,7,1)

wake_quad_gam <- unique(psi_gamQuad_epsQuad_detSeason$real$gamma)
wake_quad_gam$dynamic <- "Colonization"
wake_quad_gam$transition_num <- seq(1,7,1)

wake_quad_dynamics_plotting <- wake_quad_eps %>%
  full_join(wake_quad_gam)
wake_quad_dynamics_plotting


wake_dynamics_graph <- ggplot(data = wake_quad_dynamics_plotting, aes(x = transition_num, y = est, color = dynamic)) +
  geom_point(size = 2) +
  ylab("Estimate") +
  xlab("Transition Number") +
  scale_x_continuous(breaks=seq(1, 7, 1)) +
  geom_segment(aes(x = transition_num, xend = transition_num, y = lower_0.95,
                                                       yend = upper_0.95))+
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
        axis.text.x = element_text(size=12, color="black"), #size of x-axis text
        axis.text.y = element_text(size=12, color="black"))#size of y-axis text
wake_dynamics_graph

#ggsave("wake_quad_graph.png", width = 6, height = 3,
#       units = "in", dpi = 600, plot = wake_quad_graph,
#       path = figure_path, family = "Arial")

#ggsave("wake_dynamics_graph.png", width = 6, height = 3,
#       units = "in", dpi = 600, plot = wake_dynamics_graph,
#       path = figure_path, family = "Arial")



# create an AIC table

aictable_Wake_Co <- createAicTable((list(psi_gam_eps_det,
                                          psi_gam_eps_detSeason,
                                          psi_gamSeason_eps_detSeason,
                                          psi_gam_epsSeason_detSeason,
                                          model1_psi_gamSeason_epsSeason_detSeason,
                                          psi_gam_epsLinear_detSeason,
                                          psi_gamLinear_eps_detSeason,
                                          psi_gamLinear_epsLinear_detSeason,
                                          psi_gam_epsQuad_detSeason,
                                          psi_gamQuad_eps_detSeason,
                                          psi_gamQuad_epsQuad_detSeason)))
summary(aictable_Wake_Co)
#                                                                                                                 Model   DAIC wgt npar  neg2ll warn.conv warn.VC
#1                                                                            psi()p(SEASON)gamma(SEASON)epsilon(SEASON)   0.00   1   23 5466.48      5.45       0
#2  psi()p(SEASON)gamma(1 P newcov$urban P quadnewcov$quadratic_cov)epsilon(1 P newcov$urban P quadnewcov$quadratic_cov)  25.02   0   15 5507.51      4.30       1
#3                                                                                  psi()p(SEASON)gamma(SEASON)epsilon()  29.67   0   17 5508.15      5.51       0
#4                                                                      psi()p(SEASON)gamma(1 P urban)epsilon(1 P urban)  30.30   0   13 5516.78      5.46       0
#5                                             psi()p(SEASON)gamma(1 P newcov$urban P quadnewcov$quadratic_cov)epsilon()  36.20   0   13 5522.68      7.00       0
#6                                                                               psi()p(SEASON)gamma(1 P urban)epsilon()  38.26   0   12 5526.74      5.38       0
#7                                                                                  psi()p(SEASON)gamma()epsilon(SEASON)  39.97   0   17 5518.45      4.65       0
#8                                             psi()p(SEASON)gamma()epsilon(1 P newcov$urban P quadnewcov$quadratic_cov)  47.11   0   13 5533.59      5.27       0
#9                                                                                        psi()p(SEASON)gamma()epsilon()  47.73   0   11 5538.21      7.00       0
#10                                                                              psi()p(SEASON)gamma()epsilon(1 P urban)  48.95   0   12 5537.43      5.88       0
#11                                                                                             psi()p()gamma()epsilon() 438.09   0    4 5942.57      7.00       0

# the model were all paramaters were set at time-variant is still the best model both by AIC and by model weight followed by the eps(Quadratic), gam(Quadratic) though warning


### -- Raleigh data ####

load(paste(Rsource_path, "Raleigh_presence_spatial_subset_data1.rda", sep = "/"))


presence_spatial_subset_data1


# - pull out detection history
dethist1_Raleigh <- presence_spatial_subset_data1[,2:ncol(presence_spatial_subset_data1)]

# - create input "pao" object, for use wit occMod function
bees_subset1 <- createPao(data = dethist1_Raleigh,
                          nsurveyseason = rep(5,8), # 5 surveys/season for 8 seasons
                          title = "Bees multispecies multiseason - Raleigh subset")


# want a linear covariate, so for each transition between season want in the matrix for PRESENCE would want 1,2,3,4,5,6,7

urbancov1_subset <- rep(1, bees_subset1$nunits)
urbancov2_subset <- rep(2, bees_subset1$nunits)
urbancov3_subset <- rep(3, bees_subset1$nunits)
urbancov4_subset <- rep(4, bees_subset1$nunits)
urbancov5_subset <- rep(5, bees_subset1$nunits)
urbancov6_subset <- rep(6, bees_subset1$nunits)
urbancov7_subset <- rep(7, bees_subset1$nunits)

newcov_subset <- data.frame(urban=c(urbancov1_subset, urbancov2_subset, urbancov3_subset, urbancov4_subset,
                                    urbancov5_subset, urbancov6_subset, urbancov7_subset))


# - set up quadratic covariate
# so for each transition between season in the matrix for PRESENCE would want 1, 4, 9, 16, 25, 36, 49

quadcov1_subset <- rep(1, bees_subset1$nunits)
quadcov2_subset <- rep(4, bees_subset1$nunits)
quadcov3_subset <- rep(9, bees_subset1$nunits)
quadcov4_subset <- rep(16, bees_subset1$nunits)
quadcov5_subset <- rep(25, bees_subset1$nunits)
quadcov6_subset <- rep(36, bees_subset1$nunits)
quadcov7_subset <- rep(49, bees_subset1$nunits)

quadnewcov_subset <- data.frame(quadratic_cov=c(quadcov1_subset, quadcov2_subset, quadcov3_subset, quadcov4_subset,
                                         quadcov5_subset, quadcov6_subset, quadcov7_subset))



## Run different models
#psi()gam()eps()p()
#psi_gam_eps_det_RaleighSubset <- occMod(data = bees_subset1, type = "do.1",
#                          model= list(psi~1, p~1, gamma~1, epsilon~1),
#                          outfile='modname')


## -- save the PRESENCE output
#save(psi_gam_eps_det_RaleighSubset, 
#     file = paste(Rsource_path, "psi_gam_eps_det_RaleighSubset.rda", sep = "/"))
load(paste(Rsource_path,"psi_gam_eps_det_RaleighSubset.rda", sep = "/"))
print(summary(psi_gam_eps_det_RaleighSubset)) # print model summary
# AIC = 5569.7182
# parameter estimates converged to approximately 7 significant digits


#psi,gamma(),eps(),p(S)
#psi_gam_eps_detSeason_RaleighSubset <- occMod(data = bees_subset1, type = "do.1",
#                                model= list(psi~1, p~SEASON, gamma~1, epsilon~1),
#                                outfile='modname')

## -- save the PRESENCE output
#save(psi_gam_eps_detSeason_RaleighSubset, 
#     file = paste(Rsource_path, "psi_gam_eps_detSeason_RaleighSubset.rda", sep = "/"))
load(paste(Rsource_path,"psi_gam_eps_detSeason_RaleighSubset.rda", sep = "/"))
print(summary(psi_gam_eps_detSeason_RaleighSubset)) # print model summary
# AIC = 5206.8785
# parameter estimates converged to approximately 5.23 significant digits

#psi,gamma(S),eps(.),p(S)
#psi_gamSeason_eps_detSeason_RaleighSubset <- occMod(data = bees_subset1, type = "do.1",
#                                      model= list(psi~1, p~SEASON, gamma~SEASON, epsilon~1),
#                                      outfile='modname')
## -- save the PRESENCE output
#save(psi_gamSeason_eps_detSeason_RaleighSubset, 
#     file = paste(Rsource_path, "psi_gamSeason_eps_detSeason_RaleighSubset.rda", sep = "/"))
load(paste(Rsource_path,"psi_gamSeason_eps_detSeason_RaleighSubset.rda", sep = "/"))
print(summary(psi_gamSeason_eps_detSeason_RaleighSubset)) # print model summary
# AIC = 5187.7795
# parameter estimates converged to approximately 5.51 significant digits

#psi,gamma(.),eps(S),p(S)
#psi_gam_epsSeason_detSeason_RaleighSubset <- occMod(data = bees_subset1, type = "do.1",
#                                      model= list(psi~1, p~SEASON, gamma~1, epsilon~SEASON),
#                                      outfile='modname')

## -- save the PRESENCE output
#save(psi_gam_epsSeason_detSeason_RaleighSubset, 
#     file = paste(Rsource_path, "psi_gam_epsSeason_detSeason_RaleighSubset.rda", sep = "/"))
load(paste(Rsource_path,"psi_gam_epsSeason_detSeason_RaleighSubset.rda", sep = "/"))
print(summary(psi_gam_epsSeason_detSeason_RaleighSubset)) # print model summary
# AIC = 5200.5937
# parameter estimates converged to approximately 5.69 significant digits

#psi,gamma(S),eps(S),p(S)
load(paste(Rsource_path,"Model_1_psi_gamSeason_epsSeason_detSeason_RaleighSubset.rda", sep = "/"))

print(summary(model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset)) # print model summary
# AIC = 5161.8791
# parameter estimates converged to approximately 5.09 significant digits
# warnings produced

print(summary(model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset))

print(unique(model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset$real$psi))
print(unique(model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset$derived$psi)) # print seasonal occupancy estimates
print(unique(model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset$real$p))
print(unique(model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset$real$gamma))
print(unique(model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset$real$epsilon))


gamma_quad_RaleighSubset_season <- model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset$beta$gamma
gamma_quad_RaleighSubset_season <- gamma_quad_RaleighSubset_season %>%
  mutate(SD = se * 1.96) %>%
  mutate(lower_CI = est - SD) %>%
  mutate(upper_CI = est + SD)
gamma_quad_RaleighSubset_season
#         est          se           SD      lower_CI      upper_CI
#1  -0.385129 2.245560e-01 4.401298e-01 -8.252588e-01  5.500076e-02
#2  -1.045433 3.623300e-01 7.101668e-01 -1.755600e+00 -3.352662e-01
#3  -0.787415 3.053130e-01 5.984135e-01 -1.385828e+00 -1.890015e-01
#4 -32.859409 1.969417e+05 3.860057e+05 -3.860385e+05  3.859728e+05
#5  -3.539031 1.188495e+00 2.329450e+00 -5.868481e+00 -1.209581e+00
#6  -1.418750 1.886221e+00 3.696993e+00 -5.115743e+00  2.278243e+00
#7  -2.087233 3.478891e+00 6.818626e+00 -8.905859e+00  4.731393e+00

epsilon_quad_RaleighSubset_season <- model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset$beta$epsilon
epsilon_quad_RaleighSubset_season <- epsilon_quad_RaleighSubset_season %>%
  mutate(SD = se * 1.96) %>%
  mutate(lower_CI = est - SD) %>%
  mutate(upper_CI = est + SD)
epsilon_quad_RaleighSubset_season
#         est           se           SD      lower_CI      upper_CI
#1  -1.907461     0.745193     1.460578 -3.368039e+00    -0.4468827
#2   1.552208     0.789581     1.547579  4.629240e-03     3.0997868
#3   1.090363     0.785399     1.539382 -4.490190e-01     2.6297450
#4   2.584286     0.826738     1.620406  9.638795e-01     4.2046925
#5 -46.285691           NA           NA            NA            NA
#6  -1.336399     2.179049     4.270936 -5.607335e+00     2.9345370
#7 -18.927717 21252.944218 41655.770667 -4.167470e+04 41636.8429503


raleigh_season_eps <- unique(model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset$real$epsilon)
raleigh_season_eps$dynamic <- "Extinction"
raleigh_season_eps$transition_num <- seq(1,7,1)

raleigh_season_gam <- unique(model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset$real$gamma)
raleigh_season_gam$dynamic <- "Colonization"
raleigh_season_gam$transition_num <- seq(1,7,1)

raleigh_season_dynamics_plotting <- raleigh_season_eps %>%
  full_join(raleigh_season_gam)
raleigh_season_dynamics_plotting


raleigh_season_dynamics_graph <- ggplot(data = raleigh_season_dynamics_plotting, aes(x = transition_num, y = est, color = dynamic)) +
  geom_point(size = 2) +
  ylab("Estimate") +
  xlab("Transition Number") +
  scale_x_continuous(breaks=seq(1, 7, 1)) +
  geom_segment(aes(x = transition_num, xend = transition_num, y = lower_0.95,
                   yend = upper_0.95))+
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
        axis.text.x = element_text(size=12, color="black"), #size of x-axis text
        axis.text.y = element_text(size=12, color="black"))#size of y-axis text
raleigh_season_dynamics_graph

#ggsave("raleigh_season_dynamics_graph.png", width = 6, height = 3,
#       units = "in", dpi = 600, plot = raleigh_season_dynamics_graph,
#       path = figure_path, family = "Arial")


#psi,gamma(),eps(linear trend),p(S)
#psi_gam_epsLinear_detSeason_RaleighSubset <- occMod(data = bees_subset1, type = "do.1",
#                                      model= list(psi~1, p~SEASON, gamma~1, epsilon~1+urban),
#                                      cov.list=list(epsilon.cov=newcov_subset),
#                                      outfile='modname')

## -- save the PRESENCE output
#save(psi_gam_epsLinear_detSeason_RaleighSubset, 
#     file = paste(Rsource_path, "psi_gam_epsLinear_detSeason_RaleighSubset.rda", sep = "/"))
load(paste(Rsource_path,"psi_gam_epsLinear_detSeason_RaleighSubset.rda", sep = "/"))
print(summary(psi_gam_epsLinear_detSeason_RaleighSubset)) # print model summary
# AIC = 5207.2152
# parameter estimates converged to approximately 5.36 significant digits

#psi,gamma(linear trend),eps(.),p(S)
#psi_gamLinear_eps_detSeason_RaleighSubset <- occMod(data = bees_subset1, type = "do.1",
#                                      model= list(psi~1, p~SEASON, gamma~1+urban, epsilon~1),
#                                      cov.list=list(gamma.cov=newcov_subset),
#                                      outfile='modname')
## -- save the PRESENCE output
#save(psi_gamLinear_eps_detSeason_RaleighSubset, 
#     file = paste(Rsource_path, "psi_gamLinear_eps_detSeason_RaleighSubset.rda", sep = "/"))
load(paste(Rsource_path,"psi_gamLinear_eps_detSeason_RaleighSubset.rda", sep = "/"))
print(summary(psi_gamLinear_eps_detSeason_RaleighSubset)) # print model summary
# AIC = 5197.025
# parameter estimates converged to approximately 5.36 significant digits
# warnings produced

#psi,gamma(linear trend),eps(linear trend),p(S)
#psi_gamLinear_epsLinear_detSeason_RaleighSubset <- occMod(data = bees_subset1, type = "do.1",
#                                            model= list(psi~1, p~SEASON, gamma~1+urban, epsilon~1+urban),
#                                            cov.list=list(epsilon.cov=newcov_subset, gamma.cov=newcov_subset),
#                                            outfile='modname')
## -- save the PRESENCE output
#save(psi_gamLinear_epsLinear_detSeason_RaleighSubset, 
#     file = paste(Rsource_path, "psi_gamLinear_epsLinear_detSeason_RaleighSubset.rda", sep = "/"))
load(paste(Rsource_path,"psi_gamLinear_epsLinear_detSeason_RaleighSubset.rda", sep = "/"))
print(summary(psi_gamLinear_epsLinear_detSeason_RaleighSubset)) # print model summary
# AIC = 5185.9484
# parameter estimates converged to approximately 5.42 significant digits
# warnings produced

#psi,gamma(),eps(quadratic trend),p(S)
#psi_gam_epsQuad_detSeason_RaleighSubset <- occMod(data = bees_subset1, type = "do.1",
#                                    model= list(psi~1, p~SEASON, gamma~1, epsilon~1+newcov_subset$urban+quadnewcov_subset$quadratic_cov),
#                                    cov.list=list(epsilon.cov=newcov_subset, epsilon.cov=quadnewcov_subset),
#                                    outfile='modname')
## -- save the PRESENCE output
#save(psi_gam_epsQuad_detSeason_RaleighSubset, 
#     file = paste(Rsource_path, "psi_gam_epsQuad_detSeason_RaleighSubset.rda", sep = "/"))
load(paste(Rsource_path,"psi_gam_epsQuad_detSeason_RaleighSubset.rda", sep = "/"))
print(summary(psi_gam_epsQuad_detSeason_RaleighSubset)) # print model summary
# AIC = 5203.5265
# parameter estimates converged to approximately 4.94 significant digits

#psi,gamma(quadratic trend),eps(),p(S)
#psi_gamQuad_eps_detSeason_RaleighSubset <- occMod(data = bees_subset1, type = "do.1",
#                                    model= list(psi~1, p~SEASON, gamma~1+newcov_subset$urban+quadnewcov_subset$quadratic_cov, epsilon~1),
#                                    cov.list=list(gamma.cov=newcov_subset, gamma.cov=quadnewcov_subset),
#                                    outfile='modname')

## -- save the PRESENCE output
#save(psi_gamQuad_eps_detSeason_RaleighSubset, 
#     file = paste(Rsource_path, "psi_gamQuad_eps_detSeason_RaleighSubset.rda", sep = "/"))
load(paste(Rsource_path,"psi_gamQuad_eps_detSeason_RaleighSubset.rda", sep = "/"))
print(summary(psi_gamQuad_eps_detSeason_RaleighSubset)) # print model summary
# AIC = 5193.0188
# parameter estimates converged to approximately 7 significant digits
# warnings produced

#psi,gamma(quadratic trend),eps(quadratic trend),p(S)
#psi_gamQuad_epsQuad_detSeason_RaleighSubset <- occMod(data = bees_subset1, type = "do.1",
#                                        model= list(psi~1, p~SEASON, gamma~1+newcov_subset$urban+quadnewcov_subset$quadratic_cov, epsilon~1+newcov_subset$urban+quadnewcov_subset$quadratic_cov),
#                                        cov.list=list(gamma.cov=newcov_subset, gamma.cov=quadnewcov_subset, 
#                                                      epsilon.cov=newcov_subset, epsilon.cov=quadnewcov_subset),
#                                        outfile='modname')

## -- save the PRESENCE output
#save(psi_gamQuad_epsQuad_detSeason_RaleighSubset, 
#     file = paste(Rsource_path, "psi_gamQuad_epsQuad_detSeason_RaleighSubset.rda", sep = "/"))
load(paste(Rsource_path,"psi_gamQuad_epsQuad_detSeason_RaleighSubset.rda", sep = "/"))
print(summary(psi_gamQuad_epsQuad_detSeason_RaleighSubset)) # print model summary
# AIC = 5177.9154
# parameter estimates converged to approximately 5.86 significant digits
# warnings produced

print(summary(psi_gamQuad_epsQuad_detSeason_RaleighSubset))

print(unique(psi_gamQuad_epsQuad_detSeason_RaleighSubset$real$psi))
print(unique(psi_gamQuad_epsQuad_detSeason_RaleighSubset$derived$psi)) # print seasonal occupancy estimates
print(unique(psi_gamQuad_epsQuad_detSeason_RaleighSubset$real$p))
print(unique(psi_gamQuad_epsQuad_detSeason_RaleighSubset$real$gamma))
print(unique(psi_gamQuad_epsQuad_detSeason_RaleighSubset$real$epsilon))


gamma_quad_RaleighSubset <- psi_gamQuad_epsQuad_detSeason_RaleighSubset$beta$gamma
gamma_quad_RaleighSubset <- gamma_quad_RaleighSubset %>%
  mutate(SD = se * 1.96) %>%
  mutate(lower_CI = est - SD) %>%
  mutate(upper_CI = est + SD)
gamma_quad_RaleighSubset

epsilon_quad_RaleighSubset <- psi_gamQuad_epsQuad_detSeason_RaleighSubset$beta$epsilon
epsilon_quad_RaleighSubset <- epsilon_quad_RaleighSubset %>%
  mutate(SD = se * 1.96) %>%
  mutate(lower_CI = est - SD) %>%
  mutate(upper_CI = est + SD)
epsilon_quad_RaleighSubset




# create an AIC table

aictable_Raleigh <- createAicTable((list(psi_gam_eps_det_RaleighSubset,
                                         psi_gam_eps_detSeason_RaleighSubset,
                                         psi_gamSeason_eps_detSeason_RaleighSubset,
                                         psi_gam_epsSeason_detSeason_RaleighSubset,
                                         model1_psi_gamSeason_epsSeason_detSeason_RaleighSubset,
                                         psi_gam_epsLinear_detSeason_RaleighSubset,
                                         psi_gamLinear_eps_detSeason_RaleighSubset,
                                         psi_gamLinear_epsLinear_detSeason_RaleighSubset,
                                         psi_gam_epsQuad_detSeason_RaleighSubset,
                                         psi_gamQuad_eps_detSeason_RaleighSubset,
                                         psi_gamQuad_epsQuad_detSeason_RaleighSubset)))
summary(aictable_Raleigh)
#                                                                                                                                             Model   DAIC   wgt npar  neg2ll
#1                                                                                                        psi()p(SEASON)gamma(SEASON)epsilon(SEASON)   0.00 1e+00   23 5115.88
#2  psi()p(SEASON)gamma(1 P newcov_subset$urban P quadnewcov_subset$quadratic_cov)epsilon(1 P newcov_subset$urban P quadnewcov_subset$quadratic_cov)  16.04 3e-04   15 5147.92
#3                                                                                                  psi()p(SEASON)gamma(1 P urban)epsilon(1 P urban)  24.07 0e+00   13 5159.95
#4                                                                                                              psi()p(SEASON)gamma(SEASON)epsilon()  25.90 0e+00   17 5153.78
#5                                                           psi()p(SEASON)gamma(1 P newcov_subset$urban P quadnewcov_subset$quadratic_cov)epsilon()  31.14 0e+00   13 5167.02
#6                                                                                                           psi()p(SEASON)gamma(1 P urban)epsilon()  35.15 0e+00   12 5173.02
#7                                                                                                              psi()p(SEASON)gamma()epsilon(SEASON)  38.71 0e+00   17 5166.59
#8                                                           psi()p(SEASON)gamma()epsilon(1 P newcov_subset$urban P quadnewcov_subset$quadratic_cov)  41.65 0e+00   13 5177.53
#9                                                                                                                    psi()p(SEASON)gamma()epsilon()  45.00 0e+00   11 5184.88
#10                                                                                                          psi()p(SEASON)gamma()epsilon(1 P urban)  45.34 0e+00   12 5183.22
#11                                                                                                                         psi()p()gamma()epsilon() 407.84 0e+00    4 5561.72

# full time-variant still the best despite having warnings