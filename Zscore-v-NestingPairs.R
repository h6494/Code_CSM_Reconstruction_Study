########################Simple Way to Calculate Z-score for proxy data#######################
library(ggplot2)
library(dplyr)
library(viridis)
library(ggpubr)

##This example uses isotope data from the Cape St Mary's isotope analysis dataset
  #The .csv files has the following column headers:
  IMP2_isotopes <- read.csv("d15N.csv") 
  head(IMP2_isotopes, n=2) 
  
  #the .csv file I am working with looks like this
   # SampleID                       midpoint  d15N    N.percent   d13C    C.percent
   # CSM-IMP2 0.50-0.75 05/25/21    0.625     10.91   2.80       -27.05   21.29
   # CSM-IMP2 1.50-1.75 05/29/21    1.625     10.52   2.65       -27.24   24.53
    
##To calculate the Z-score of N, we need to calculate the average d15N and standard deviation of d15N
d15N <- IMP2_isotopes$d15N
percentN <- IMP2_isotopes$N.percent
d13C <- IMP2_isotopes$d15N
percentC <- IMP2_isotopes$C.percent

##The formula for calculating a z-score:
  #take the actual d15N value, substract the mean and divide the difference by the standard deviation
  #here I just calculate the z-score for d15N, %N, d13C and %C 

Zscore_isotopes <- IMP2_isotopes %>% 
  mutate(d15N_z = (d15N - mean(d15N))/sd(d15N), 
         d13C_z = (d13C - mean(d13C))/sd(d13C),
         percentN_z = (percentN - mean(percentN))/sd(percentN),
         percentN_z = (percentN - mean(percentN))/sd(percentN))

#export the values to a .csv or excel file
write.csv(Zscore_isotopes, "IMP2_isotopes_zscores")

########################Simple Way to Calculate Z-score for proxy data#######################

##PURPOSE:
#calculate your z-scores here, save the csv file and use the file for further downstream analyses!
#for exmple// run a GAM analysis on your proxy data using the z-score values 
#which provides a better fit to GAMs when your K value cannot be greater than 20

##load packages
library(tidyverse)
library(ggplot2)

##This example uses isotope data from the Cape St Mary's isotope analysis dataset
#The .csv files has the following column headers:
zscores <- read.csv("z-scores.csv")

##The formula for calculating a z-score:
#take the actual d15N value, substract the mean and divide the difference by the standard deviation
#I just calculated the z-score for my proxy data in excel, and transferred data over to R

#EXAMPLE//
z_dN15 <- (dN15 - mean(dN15))/sd(dN15)

#you can export the values to a .csv or excel file
write.csv(zscores, "proxydata_zscored.csv")

ggplot()+geom_line(data=zscores, mapping=aes(x=year, y=z_S.cons), color="lightblue")



z_plot <- ggplot() +
  geom_smooth(zscores, mapping=aes(x=year, y=mean), method="lm", na.rm=TRUE, formula = y ~ poly(x, 2), color="lightgrey", fill="lightgrey", linetype = 0) +
  scale_x_continuous(breaks=seq(1800,2023, by=10), limits = c(1800,2025), position="top") +
  geom_line(data=zscores[!is.na(zscores$mean),], mapping=aes(x=year, y=mean), color="black", linewidth=1.5) +
  geom_line(data=zscores[!is.na(zscores$z_chlA),], mapping=aes(x=year, y=z_chlA), color="green") +
  geom_line(data=zscores[!is.na(zscores$z_d15N),], mapping=aes(x=year, y=z_d15N), color="blue3") +
  geom_line(data=zscores[!is.na(zscores$z_P),], mapping=aes(x=year, y=z_P), color="orange") +
  geom_line(data=zscores[!is.na(zscores$z_Zn),], mapping=aes(x=year, y=z_Zn), color="firebrick1") +
  geom_line(data=zscores[!is.na(zscores$Z_Cd),], mapping=aes(x=year, y=Z_Cd), color="red4") +
  geom_line(data=zscores[!is.na(zscores$z_S.cons),], mapping=aes(x=year, y=z_S.cons), color="mediumaquamarine") +
  geom_point(data=zscores, mapping=aes(x=year, y=z_chlA), color="green") +
  geom_point(data=zscores, mapping=aes(x=year, y=z_d15N), color="blue3") +
  geom_point(data=zscores, mapping=aes(x=year, y=z_P), color="orange") +
  geom_point(data=zscores, mapping=aes(x=year, y=z_Zn), color="firebrick1") +
  geom_point(data=zscores, mapping=aes(x=year, y=Z_Cd), color="red4") +
  geom_point(data=zscores, mapping=aes(x=year, y=z_S.cons), color="mediumaquamarine") +
  theme_classic()+
  theme(panel.grid.major.x=element_line(), text = element_text(size = 15), axis.text.x.top= element_text(vjust=0.5, angle=90))+
  xlab(NULL)+
  ylab("z-score")

popn <- read.csv("../populationcounts_CSM.csv")

NOGA <- popn$Northern.gannet
COMU <- popn$Common.murre
BLKI <- popn$Black.legged.kittiwake
year <- popn$year

popn_plot <- ggplot(popn, aes(x=year)) +
  geom_col(aes(y=NOGA), width=1, fill="slateblue2", just=0, color="slateblue4") +
  geom_col(aes(y=BLKI), width=1, fill="hotpink1", just=0, color="hotpink3") +
  geom_col(aes(y=COMU), width=1, fill="olivedrab2", just=0, color="olivedrab4") +
  scale_x_continuous(breaks=seq(1800,2023, by=10), limits = c(1800,2025), position="bottom") +
  scale_y_continuous(breaks=seq(0,16000, by=4000)) +
  ylab("nesting pairs")+
  theme_classic()+
  theme(panel.grid.major.x=element_line(), text = element_text(size = 15), axis.text.x = element_text(margin = margin(t=1)), axis.title.x = element_blank(), axis.text.x.bottom = element_text(vjust=0.5, angle=90))


ggarrange(z_plot, popn_plot, 
          ncol = 1, nrow = 2,
          heights=c(15,12),
          common.legend=TRUE,
          align="hv")

