######PLOTTING STRATIGRAPHIES#######

#done using the tutorial: https://cran.r-project.org/web/packages/tidypaleo/vignettes/strat_diagrams.html

library(ggplot2)
library(tidyverse)
library(tidypaleo)
library(ggh4x)

theme_set(theme_paleo(8))

#view these example datasets to see how your data should be set up
#you have to create two spreadsheets in .csv format:
  #1) a spreadsheet containing all the geochem data, like this
      view(alta_lake_geochem)

  #2) a spreadsheet containing all the abundance data, like this
      view(keji_lakes_plottable)


setwd("~/Desktop/proxydata")

################
      
#load geochem data for the IMP2 site
geochem_IMP2 <- read.csv("proxydata_geochem_IMP2.csv")

#plot the base straitgraphic diagram for your geochem data
geo_strat_IMP2 <- ggplot(geochem_IMP2, aes(x = value, y = depth)) +
  geom_lineh() +
  geom_point() +
  facet_geochem_gridh(vars(param)) +
  labs(x = NULL, y = "Depth (cm)") +
  facetted_pos_scales(x = list(scale_x_continuous(limits = c(0,0.00025)),
                               scale_x_continuous(limits = c(0,0.15)),
                               scale_x_continuous(limits = c(-2,11)),
                               scale_x_continuous(limits = c(0,10000)),
                               scale_x_continuous(limits = c(0,0.025))),
                               scale_y_reverse(breaks=seq(0,17, by=1), limits = c(17,0)))

geo_strat_IMP2

#######################

#load geochem data for the REF3 site
geochem_REF3 <- read.csv("proxydata_geochem_REF3.csv")

#plot the base straitgraphic diagram for your geochem data
geo_strat_REF3 <- ggplot(geochem_REF3, aes(x = value, y = depth)) +
  geom_lineh() +
  geom_point() +
  facet_geochem_gridh(vars(param)) +
  labs(x = NULL, y = "Depth (cm)") +
  facetted_pos_scales(x = list(scale_x_continuous(limits = c(0,0.00025)),
                               scale_x_continuous(limits = c(0,0.15)),
                               scale_x_continuous(limits = c(-2,11)),
                               scale_x_continuous(limits = c(0,10000)),
                               scale_x_continuous(limits = c(0,0.025))),
                      scale_y_reverse(breaks=seq(0,17, by=1), limits = c(17,0)))

geo_strat_REF3

########################

#load the abundance data for the IMP2 site
diatoms_IMP2 <- read.csv("proxydata_diatoms_IMP2.csv")

#plot the base straitgraphic diagram for your geochem data
diatom_strat_IMP2 <- ggplot(diatoms_IMP2, aes(x = rel_abund, y = depth)) +
  geom_areah() +
  geom_col_segsh() +
  facet_abundanceh(vars(taxon), grouping = vars(location), rotate_facet_labels = 0) + 
  labs(x = NULL, y = "Depth (cm)") +
  facetted_pos_scales(x = list(scale_x_abundance(limits = c(0,100)),
                               scale_x_abundance(limits = c(0,100))),
                      scale_y_reverse(limits = c(17,0)))
diatom_strat_IMP2

#######################

#load the abundance data for the REF3 site
diatoms_REF3 <- read.csv("proxydata_diatoms_REF3.csv")

#plot the base straitgraphic diagram for your geochem data
diatom_strat_REF3 <- ggplot(diatoms_REF3, aes(x = rel_abund, y = depth)) +
  geom_areah() +
  geom_col_segsh() +
  facet_abundanceh(vars(taxon), grouping = vars(location), rotate_facet_labels = 0) + 
  labs(x = NULL, y = "Depth (cm)") +
  facetted_pos_scales(x = list(scale_x_abundance(limits = c(0,100)),
                               scale_x_abundance(limits = c(0,100))),
                      scale_y_reverse(limits = c(17,0)))
diatom_strat_REF3



#######################
#use patchwork to put all the plots together
library(patchwork)

#plot the abundance and geochem data together for the IMP2 site

strat_final_IMP2 <- wrap_plots(geo_strat_IMP2 + 
    theme(strip.background = element_blank(), strip.text.y = element_blank()),
    diatom_strat_IMP2 +
    theme(axis.text.y.left = element_blank(), axis.ticks.y.left = element_blank()) +
    labs(y = NULL), nrow = 1, widths = c(3, 1))

strat_final_IMP2

#plot the abundance and geochem data together for the REF3 site

strat_final_REF3 <- wrap_plots(geo_strat_REF3 + 
     theme(strip.background = element_blank(), strip.text.y = element_blank()),
     diatom_strat_REF3 +
     theme(axis.text.y.left = element_blank(), axis.ticks.y.left = element_blank()) +
     labs(y = NULL), nrow = 1, widths = c(3, 1))

strat_final_REF3

#plot IMP2 and REF3 together 

final_strat <- wrap_plots(strat_final_IMP2,strat_final_REF3, nrow=2, ncol=1)
  
final_strat
#after plotting, make sure that all your data is aligned properly with the depth axis, make sure you have set your limits in the scale_y_reverse(limits = c()) argument to be fixed to the max scale depth


#######################
##EXTRAS##################


#adding points of interest
strat_final <- strat_final +
  geom_hline(yintercept = c(0, 4, 8, 12, 16), col = "red", lty = 2, alpha = 1)

#if you want to change the position of facets in any of the datasets use mutate
geo_strat %>%
  mutate(param = fct_relevel(param, "chla", "Cd/Al", "Zn/Al")) %>%
  ggplot(aes(x = value, y = depth)) +
  ...

#if you want to add year as secondary y-axis create an age-depth model
geo_adm <- age_depth_model(geochem, depth = depth, age = age)

geo_strat +
  scale_y_depth_age(geo_adm, age_name = "year")