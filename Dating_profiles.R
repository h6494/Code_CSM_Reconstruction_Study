??ggplot
ggplot(?)

library(ggplot2)
library(dplyr)
library(viridis)
library(ggpubr)

dp_IMP2 <- read.csv("dp_IMP2.csv") 
dp_REF3 <- read.csv("dp_REF3.csv") 

depth_IMP2 <- dp_IMP2$depth
depth_REF3 <- dp_REF3$midpoint

act_IMP2 <- dp_IMP2$activity
act_REF3 <- dp_REF3$activity

sed_IMP2 <- dp_IMP2$sed.rate
sed_REF3 <- dp_REF3$sed.rate

year_IMP <- dp_IMP2$year
year_REF <- dp_REF3$year
error_IMP <- dp_IMP2$error
error_REF <- dp_REF3$error

###################### plotting dating profiles

activity <- ggplot() +
  geom_point(data = dp_IMP2, (aes(x= depth_IMP2, y=act_IMP2)), shape=22, fill="paleturquoise4", size=3, color = "paleturquoise4", alpha=0.5) +
  geom_line(data = dp_IMP2, (aes(x = depth_IMP2, y=act_IMP2)), color="paleturquoise4") +
  geom_point(data = dp_REF3, (aes(x = depth_REF3, y=act_REF3)), size =3, color="palegreen3") +
  geom_line(data = dp_REF3, (aes(x = depth_REF3, y=act_REF3)), color="palegreen3") +
  labs(x="Midpoint depth (cm)", y="Pb-210 Activity (Bq/kg)") +
  scale_x_continuous(breaks=seq(0,19, by= 1), limits = c(0,19)) +
  theme_classic()+
  theme(text = element_text(size = 12), panel.grid.major.x=element_line(), axis.text.y = element_text(size=12), axis.title.y = element_text(margin = margin(t=0, r= 20, b=0, l=0)))

activity


sed.rate <- ggplot() +
  geom_point(data = dp_IMP2, (aes(x = depth_IMP2, y=sed_IMP2)),shape=21, color = "paleturquoise4") +
  geom_line(data = dp_IMP2, (aes(x = depth_IMP2, y=sed_IMP2)), color="paleturquoise4") +
  geom_point(data = dp_REF3, (aes(x = depth_REF3, y=sed_REF3)), color="palegreen3") +
  geom_line(data = dp_REF3, (aes(x = depth_REF3, y=sed_REF3)), color="palegreen3") +
  labs(x = "Core Depth (cm/yr)", y = "Sedimentation rate (cm/yr)") +
  scale_x_continuous(breaks=seq(0,19, by= 1), limits = c(0,19)) +
  theme_classic() +
  theme(panel.grid.major.x=element_line(), text = element_text(size = 12), axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12), axis.title.y = element_text(margin = margin(t=0, r= 20, b=0, l=0)))

par(mar = c(10, 10,10,10))

year <- ggplot() +
  geom_point(data = dp_IMP2, (aes(x = year_IMP, y= depth_IMP2)), na.rm=TRUE, shape=22, fill="paleturquoise4", size=3, color = "paleturquoise4", alpha=0.5) +
  geom_line(data = dp_IMP2, (aes(x = year_IMP, y= depth_IMP2)), na.rm=TRUE, color = "paleturquoise4") + 
  stat_smooth(dp_IMP2, mapping=aes(year_IMP, depth_IMP2), method="lm", na.rm=TRUE, formula = y ~ poly(x, 2), se=FALSE, color="red", alpha=0.3) +
  geom_errorbar(aes(x=year_IMP, xmin=year_IMP-error_IMP, xmax=year_IMP+error_IMP, y=depth_IMP2, ymax=NULL, ymin=NULL), width=.2) +
  geom_point(data = dp_REF3, (aes(x = year_REF, y=depth_REF3)), size=3, na.rm=TRUE, color="palegreen3") +
  geom_line(data = dp_REF3, (aes(x = year_REF, y=depth_REF3)), na.rm=TRUE, color="palegreen3", alpha=0.5) +
  geom_errorbar(aes(x=year_REF, xmin=year_REF-error_REF, xmax=year_REF+error_REF, y=depth_REF3, ymax=NULL, ymin=NULL), width=.2) +
  stat_smooth(dp_REF3, mapping=aes(year_REF, depth_REF3), method="lm", na.rm=TRUE, formula = y ~ poly(x, 2), se=FALSE, color="red", alpha=0.3) +
  labs(x = "Year (CRS model)", y = "Midpoint depth (cm)") +
  scale_x_continuous(breaks=seq(1800,2023, by= 20)) +
  scale_y_reverse(breaks=seq(0,10, by=1), expand=c(0,0)) +
  theme_classic() +
  theme(panel.grid.major.x=element_line(), text = element_text(size = 12), axis.text.y = element_text(size=12), axis.text.x = element_text(size=12))

library (ggpmisc)

year + stat_poly_eq(aes(label = paste(stat(adj.rr.label), stat(poly_eq.label), sep = "~~~~")), formula = y ~ poly(x, 2), parse = TRUE, label.x.npc = "right", label.y.npc = "top", geom = "label")

##############################
####################################

year <- ggplot() +
  geom_point(data = dp_IMP2, (aes(x = year_IMP, y= depth_IMP2)), na.rm=TRUE, shape=22, fill="paleturquoise4", size=3, color = "paleturquoise4", alpha=0.5) +
  geom_line(data = dp_IMP2, (aes(x = year_IMP, y= depth_IMP2)), na.rm=TRUE, color = "paleturquoise4") +
  stat_smooth(dp_IMP2, mapping=aes(year_IMP, depth_IMP2), method="lm") +
  geom_errorbar(aes(x=year_IMP, xmin=year_IMP-error_IMP, xmax=year_IMP+error_IMP, y=depth_IMP2, ymax=NULL, ymin=NULL), width=.2) +
  labs(x = "Year (CRS model)", y = "Midpoint depth (cm)") +
  scale_x_continuous(breaks=seq(1800,2023, by= 20)) +
  scale_y_reverse(breaks=seq(0,15, by=1), expand=c(0,0)) +
  theme_classic() +
  theme(panel.grid.major.x=element_line(), text = element_text(size = 12), axis.text.y = element_text(size=12), axis.text.x = element_text(size=12))

year

#################
#################

ggarrange(activity, year, ncol = 1, nrow = 2, heights=20, widths=5, common.legend=TRUE, legend=c("top"))
             
