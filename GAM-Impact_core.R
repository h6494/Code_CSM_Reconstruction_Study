#####Create GAM plots showing significant peridos in time####

setwd()

##install the following packages 
mgcv, analogue, ggplot2, mvnfast, gratia, viridis

#if using ubuntu, run this first to install the dependency RCPArmadillo and mvnfast for gratia
sudo apt-get update -y 
sudo apt-get install -y r-cran-rcpparmadillo
sudo apt-get install -y r-cran-mvnfast

## load packages
library(mgcv) # For gams
library(ggplot2) # For plotting
library(analogue) # For general use and support of other packages
library(gratia) # For derivatives to identify significant periods of change
library(viridis) # Package to use colours that are effective for people with colour blindness

# Load a local function for first derivatives
signifD <- function(x, d, upper, lower, eval = 0) {
  miss <- upper > eval & lower < eval
  incr <- decr <- x
  want <- d > eval
  incr[!want | miss] <- NA
  want <- d < eval
  decr[!want | miss] <- NA
  list(incr = incr, decr = decr)
}


##  chlA  ##############################################################################
pd_chlA <-read.csv("chlA.csv")
head(pd_chlA) #Check that data looks correct

#REML FUNCTION
chlA_gam<-gam(chlA~s(year, k = 26), data= pd_chlA, method="REML") 
gam.check(chlA_gam)
plot(chlA_gam)

# And now add standard error
fit_gcv<- predict(chlA_gam, data = pd_chlA, se.fit = TRUE, length.out=200)
crit.t <- qt(0.975, df.residual(chlA_gam))
chlA_new <- data.frame(Year = pd_chlA[["year"]],
                        fit = fit_gcv$fit,
                        se.fit = fit_gcv$se.fit)

chlA_new <- transform(chlA_new,
                       upper = fit + (crit.t * se.fit),
                       lower = fit - (crit.t * se.fit))

#  need equal lengths everywhere (i.e., 200 rows)
chlA_new_sigperiod<-data.frame(approx(chlA_new$Year, chlA_new$fit, n=200)) 
colnames(chlA_new_sigperiod)<-c("Year", "fit")
head(chlA_new_sigperiod) # Check that it looks right. 

# Identify derivatives of significant value
BTP_chlA_der<-derivatives(chlA_gam, type="central", n=200)
draw(BTP_chlA_der)

BTP_chlA_sig<-signifD(chlA_new_sigperiod$fit, d= BTP_chlA_der$derivative, BTP_chlA_der$upper, BTP_chlA_der$lower)
BTP_chlA_sig

#plot
chlA<-ggplot()+
  geom_ribbon(data=chlA_new, mapping= aes(x= Year, ymax= upper, ymin=lower), fill="#b3e0ff", inherit.aes = FALSE, alpha=0.5)+
  geom_line(data= chlA_new, mapping=aes(y= fit, x= Year), colour="lightblue4")+
  geom_line(data= chlA_new_sigperiod, mapping=aes(y=BTP_chlA_sig$decr, x=Year), colour="deepskyblue4", size=1)+ ## Periods of increase
  geom_line(data=chlA_new_sigperiod, mapping=aes(y=BTP_chlA_sig$incr, x=Year), colour="deepskyblue4", size=1)+ ## Periods of decrease
  geom_point(data=pd_chlA, aes(y=chlA, x= year), shape= 21, fill= "#0099cc", colour="lightblue4", size = 0.5)+
  scale_x_continuous(breaks=seq(1000,2022, by= 100), limits = c(1000,2022))+
  ylim(c(-0.01, 0.16))+
  labs(x=NULL, y=NULL) +
  theme_classic() + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x=element_blank(), text = element_text(size = 10, face = "bold"))

chlA



##  _d15N  ##############################################################################

pd_d15N <-read.csv("d15N.csv")
head(pd_d15N) #Check that data looks correct

#REML FUNCTION
d15N_gam<-gam(d15N~s(year, k = 20), data= pd_d15N, method="REML") 
gam.check(d15N_gam)
plot(d15N_gam)

# And now add standard error
fit_gcv<- predict(d15N_gam, data = pd_d15N, se.fit = TRUE, length.out=200)
crit.t <- qt(0.975, df.residual(d15N_gam))
d15N_new <- data.frame(Year = pd_d15N[["year"]],
                      fit = fit_gcv$fit,
                      se.fit = fit_gcv$se.fit)

d15N_new <- transform(d15N_new,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))

#  need equal lengths everywhere (i.e., 200 rows)
d15N_new_sigperiod<-data.frame(approx(d15N_new$Year, d15N_new$fit, n=200)) 
colnames(d15N_new_sigperiod)<-c("Year", "fit")
head(d15N_new_sigperiod) # Check that it looks right. 

# Identify derivatives of significant value
BTP_d15N_der<-derivatives(d15N_gam, type="central", n=200)
draw(BTP_d15N_der)

BTP_d15N_sig<-signifD(d15N_new_sigperiod$fit, d= BTP_d15N_der$derivative, BTP_d15N_der$upper, BTP_d15N_der$lower)
BTP_d15N_sig

#plot
d15N<-ggplot()+
  geom_ribbon(data=d15N_new, mapping= aes(x= Year, ymax= upper, ymin=lower), fill="#b3e0ff", inherit.aes = FALSE, alpha=0.5)+
  geom_line(data= d15N_new, mapping=aes(y= fit, x= Year), colour="lightblue4")+
  geom_line(data= d15N_new_sigperiod, mapping=aes(y=BTP_d15N_sig$decr, x=Year), colour="deepskyblue4", size=1)+ ## Periods of increase
  geom_line(data=d15N_new_sigperiod, mapping=aes(y=BTP_d15N_sig$incr, x=Year), colour="deepskyblue4", size=1)+ ## Periods of decrease
  geom_point(data=pd_d15N, aes(y= d15N, x= year), shape= 21, fill= "#0099cc", colour="lightblue4", size = 0.5)+
  scale_x_continuous(breaks=seq(1000,2022, by= 100), limits = c(1000,2022))+
  ylim(c(-2, 13))+
  labs(x=NULL, y=NULL) +
  theme_classic() + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x=element_blank(), text = element_text(size = 10, face = "bold"))
d15N





##  d13C  ##############################################################################
pd_d13C <-read.csv("d13C.csv")
head(pd_d13C) #Check that data looks correct

#REML FUNCTION
d13C_gam<-gam(d13C~s(year, k = 19), data= pd_d13C, method="REML") 
gam.check(d13C_gam)
plot(d13C_gam)

# And now add standard error
fit_gcv<- predict(d13C_gam, data = pd_d13C, se.fit = TRUE, length.out=200)
crit.t <- qt(0.975, df.residual(d13C_gam))
d13C_new <- data.frame(Year = pd_d13C[["year"]],
                        fit = fit_gcv$fit,
                        se.fit = fit_gcv$se.fit)

d13C_new <- transform(d13C_new,
                       upper = fit + (crit.t * se.fit),
                       lower = fit - (crit.t * se.fit))

#  need equal lengths everywhere (i.e., 200 rows)
d13C_new_sigperiod<-data.frame(approx(d13C_new$Year, d13C_new$fit, n=200)) 
colnames(d13C_new_sigperiod)<-c("Year", "fit")
head(d13C_new_sigperiod) # Check that it looks right. 

# Identify derivatives of significant value
BTP_d13C_der<-derivatives(d13C_gam, type="central", n=200)
draw(BTP_d13C_der)

BTP_d13C_sig<-signifD(d13C_new_sigperiod$fit, d= BTP_d13C_der$derivative, BTP_d13C_der$upper, BTP_d13C_der$lower)
BTP_d13C_sig

#plot
d13C<-ggplot()+
  geom_ribbon(data=d13C_new, mapping= aes(x= Year, ymax= upper, ymin=lower), fill="#b3e0ff", inherit.aes = FALSE, alpha=0.5)+
  geom_line(data= d13C_new, mapping=aes(y= fit, x= Year), colour="lightblue4")+
  geom_line(data= d13C_new_sigperiod, mapping=aes(y=BTP_d13C_sig$decr, x=Year), colour="deepskyblue4", size=1)+ ## Periods of increase
  geom_line(data=d13C_new_sigperiod, mapping=aes(y=BTP_d13C_sig$incr, x=Year), colour="deepskyblue4", size=1)+ ## Periods of decrease
  geom_point(data=pd_d13C, aes(y= d13C, x= year), shape= 21, fill= "#0099cc", colour="lightblue4", size = 0.5)+
  theme(text = element_text(size = 10, face = "bold")) +
  ylab("d13C")+
  xlab("year Year (cm)")+
  scale_x_continuous(breaks=seq(1000,2022, by= 100), limits = c(1000,2022))+
  ylim(c(-28, -23))+
  labs(x=NULL, y=NULL) +
  theme_classic() + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x=element_blank(), text = element_text(size = 10, face = "bold"))

d13C



## Phosphorus  ##############################################################################
pd_metals <-read.csv("metals.csv")
head(pd_metals) #Check that data looks correct

#REML FUNCTION
P_gam<-gam(P~s(year, k = 15), data= pd_metals, method="REML") 
gam.check(P_gam)
plot(P_gam)

# And now add standard error
fit_gcv<- predict(P_gam, data = pd_metals, se.fit = TRUE, length.out=200)
crit.t <- qt(0.975, df.residual(P_gam))
P_new <- data.frame(Year = pd_metals[["year"]],
                        fit = fit_gcv$fit,
                        se.fit = fit_gcv$se.fit)

P_new <- transform(P_new,
                       upper = fit + (crit.t * se.fit),
                       lower = fit - (crit.t * se.fit))

#  need equal lengths everywhere (i.e., 200 rows)
P_new_sigperiod<-data.frame(approx(P_new$Year, P_new$fit, n=200)) 
colnames(P_new_sigperiod)<-c("Year", "fit")
head(P_new_sigperiod) # Check that it looks right. 

# Identify derivatives of significant value
BTP_P_der<-derivatives(P_gam, type="central", n=200)
draw(BTP_P_der)

BTP_P_sig<-signifD(P_new_sigperiod$fit, d= BTP_P_der$derivative, BTP_P_der$upper, BTP_P_der$lower)
BTP_P_sig

#plot
P<-ggplot()+
  geom_ribbon(data=P_new, mapping= aes(x= Year, ymax= upper, ymin=lower), fill="#b3e0ff", inherit.aes = FALSE, alpha=0.5)+
  geom_line(data= P_new, mapping=aes(y= fit, x= Year), colour="lightblue4")+
  geom_line(data= P_new_sigperiod, mapping=aes(y=BTP_P_sig$decr, x=Year), colour="deepskyblue4", size=1)+ ## Periods of increase
  geom_line(data=P_new_sigperiod, mapping=aes(y=BTP_P_sig$incr, x=Year), colour="deepskyblue4", size=1)+ ## Periods of decrease
  geom_point(data=pd_metals, aes(y=P, x= year), shape= 21, fill= "#0099cc", colour="lightblue4", size = 0.5)+
  theme(text = element_text(size = 10, face = "bold")) +
  ylab("")+
  xlab("year")+
  scale_x_continuous(breaks=seq(1000,2022, by= 100), limits = c(1000,2022))+
  ylim(c(-3000, 15000))+
  labs(x=NULL, y=NULL) +
  theme_classic() + 
  theme(text = element_text(size = 10, face = "bold"))

P

##  Zinc/Al  ##############################################################################
pd_metals <-read.csv("metals.csv")
head(pd_metals) #Check that data looks correct

#REML FUNCTION
Zn_gam<-gam(Zn~s(year, k = 15), data= pd_metals, method="REML") 
gam.check(Zn_gam)
plot(Zn_gam)

# And now add standard error
fit_gcv<- predict(Zn_gam, data = pd_metals, se.fit = TRUE, length.out=200)
crit.t <- qt(0.975, df.residual(Zn_gam))
Zn_new <- data.frame(Year = pd_metals[["year"]],
                     fit = fit_gcv$fit,
                     se.fit = fit_gcv$se.fit)

Zn_new <- transform(Zn_new,
                    upper = fit + (crit.t * se.fit),
                    lower = fit - (crit.t * se.fit))

#  need equal lengths everywhere (i.e., 200 rows)
Zn_new_sigperiod<-data.frame(approx(Zn_new$Year, Zn_new$fit, n=200)) 
colnames(Zn_new_sigperiod)<-c("Year", "fit")
head(Zn_new_sigperiod) # Check that it looks right. 

# Identify derivatives of significant value
BTP_Zn_der<-derivatives(Zn_gam, type="central", n=200)
draw(BTP_Zn_der)

BTP_Zn_sig<-signifD(Zn_new_sigperiod$fit, d= BTP_Zn_der$derivative, BTP_Zn_der$upper, BTP_Zn_der$lower)
BTP_Zn_sig

#plot
Zn <-ggplot() +
  geom_ribbon(data=Zn_new, mapping= aes(x= Year, ymax= upper, ymin=lower), fill="#b3e0ff", inherit.aes = FALSE, alpha=0.5) +
  geom_line(data= Zn_new, mapping=aes(y= fit, x= Year), colour="lightblue4") +
  geom_line(data= Zn_new_sigperiod, mapping=aes(y=BTP_Zn_sig$decr, x=Year), colour="deepskyblue4", size=1) + ## Periods of increase
  geom_line(data=Zn_new_sigperiod, mapping=aes(y=BTP_Zn_sig$incr, x=Year), colour="deepskyblue4", size=1) + ## Periods of decrease
  geom_point(data=pd_metals, aes(y= Zn, x= year), shape= 21, fill= "#0099cc", colour="lightblue4", size = 0.5) +
  theme(text = element_text(size = 10, face = "bold")) +
  ylab("")+
  xlab("")+
  scale_x_continuous(breaks=seq(1000,2022, by= 100), limits = c(1000,2022))+
  ylim(c(0.00006, 0.015)) +
  labs(x=NULL, y=NULL) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x=element_blank(), text = element_text(size = 10, face = "bold"))

Zn
       



##  Cadmium/Al ##############################################################################
pd_metals <-read.csv("metals.csv")
head(pd_metals) #Check that data looks correct

#REML FUNCTION
Cd_gam<-gam(Cd~s(year, k = 15), data= pd_metals, method="REML") 
gam.check(Cd_gam)
plot(Cd_gam)

# And now add standard error
fit_gcv<- predict(Cd_gam, data = pd_metals, se.fit = TRUE, length.out=200)
crit.t <- qt(0.975, df.residual(Cd_gam))
Cd_new <- data.frame(Year = pd_metals[["year"]],
                      fit = fit_gcv$fit,
                      se.fit = fit_gcv$se.fit)

Cd_new <- transform(Cd_new,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))

#  need equal lengths everywhere (i.e., 200 rows)
Cd_new_sigperiod<-data.frame(approx(Cd_new$Year, Cd_new$fit, n=200)) 
colnames(Cd_new_sigperiod)<-c("Year", "fit")
head(Cd_new_sigperiod) # Check that it looks right. 

# Identify derivatives of significant value
BTP_Cd_der<-derivatives(Cd_gam, type="central", n=200)
draw(BTP_Cd_der)

BTP_Cd_sig<-signifD(Cd_new_sigperiod$fit, d= BTP_Cd_der$derivative, BTP_Cd_der$upper, BTP_Cd_der$lower)
BTP_Cd_sig

#plot
Cd<-ggplot()+
geom_ribbon(data=Cd_new, mapping= aes(x= Year, ymax= upper, ymin=lower), fill="#b3e0ff", inherit.aes = FALSE, alpha=0.5)+
  geom_line(data= Cd_new, mapping=aes(y= fit, x= Year), colour="lightblue4")+
  geom_line(data= Cd_new_sigperiod, mapping=aes(y=BTP_Cd_sig$decr, x=Year), colour="deepskyblue4", size=1)+ ## Periods of increase
  geom_line(data=Cd_new_sigperiod, mapping=aes(y=BTP_Cd_sig$incr, x=Year), colour="deepskyblue4", size=1)+ ## Periods of decrease
  geom_point(data=pd_metals, aes(y= Cd, x= year), shape= 21, fill= "#0099cc", colour="lightblue4", size = 0.5)+
  theme(text = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks=seq(1000,2022, by= 100), limits = c(1000,2022))+
  ylim(c(0.05e-05, 3e-04))+
  labs(x=NULL, y=NULL) +
  theme_classic()+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x=element_blank(), text = element_text(size = 10, face = "bold"))


Cd

##  S.exigiformis  ##############################################################################
pd_diatoms <-read.csv("diatoms.csv")
head(pd_diatoms) #Check that data looks correct

#REML FUNCTION
S.exi_gam<-gam(S.exigiformis~s(year, k = 17), data= pd_diatoms, method="REML") 
gam.check(S.exi_gam)
plot(S.exi_gam)

# And now add standard error
fit_gcv<- predict(S.exi_gam, data = pd_diatoms, se.fit = TRUE, length.out=200)
crit.t <- qt(0.975, df.residual(S.exi_gam))
S.exi_new <- data.frame(Year = pd_diatoms[["year"]],
                      fit = fit_gcv$fit,
                      se.fit = fit_gcv$se.fit)

S.exi_new <- transform(S.exi_new,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))

#  need equal lengths everywhere (i.e., 200 rows)
S.exi_new_sigperiod<-data.frame(approx(S.exi_new$Year, S.exi_new$fit, n=200)) 
colnames(S.exi_new_sigperiod)<-c("Year", "fit")
head(S.exi_new_sigperiod) # Check that it looks right. 

# Identify derivatives of significant value
BTP_S.exi_der<-derivatives(S.exi_gam, type="central", n=200)
draw(BTP_S.exi_der)

BTP_S.exi_sig<-signifD(S.exi_new_sigperiod$fit, d= BTP_S.exi_der$derivative, BTP_S.exi_der$upper, BTP_S.exi_der$lower)
BTP_S.exi_sig

#plot
S.exi<-ggplot()+
  geom_ribbon(data=S.exi_new, mapping= aes(x= Year, ymax= upper, ymin=lower), fill="#b3e0ff", inherit.aes = FALSE, alpha=0.5)+
  geom_line(data= S.exi_new, mapping=aes(y= fit, x= Year), colour="lightblue4")+
  geom_line(data= S.exi_new_sigperiod, mapping=aes(y=BTP_S.exi_sig$decr, x=Year), colour="deepskyblue4", size=1)+ ## Periods of increase
  geom_line(data=S.exi_new_sigperiod, mapping=aes(y=BTP_S.exi_sig$incr, x=Year), colour="deepskyblue4", size=1)+ ## Periods of decrease
  geom_point(data=pd_diatoms, aes(y= S.exigiformis, x= year), shape= 21, fill= "#0099cc", colour="lightblue4", size = 0.5)+
  theme(text = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks=seq(1000,2022, by= 100), limits = c(1000,2022))+
  ylim(c(-15, 110))+
  labs(x=NULL, y=NULL) +
  theme_classic()+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x=element_blank(), text = element_text(size = 10, face = "bold"))


S.exi





##  S.construens  ##############################################################################
pd_diatoms <-read.csv("diatoms.csv")
head(pd_diatoms) #Check that data looks correct

#REML FUNCTION
S.cons_gam<-gam(S.construens~s(year, k = 17), data= pd_diatoms, method="REML") 
gam.check(S.cons_gam)
plot(S.cons_gam)

# And now add standard error
fit_gcv<- predict(S.cons_gam, data = pd_diatoms, se.fit = TRUE, length.out=200)
crit.t <- qt(0.975, df.residual(S.cons_gam))
S.cons_new <- data.frame(Year = pd_diatoms[["year"]],
                         fit = fit_gcv$fit,
                         se.fit = fit_gcv$se.fit)

S.cons_new <- transform(S.cons_new,
                        upper = fit + (crit.t * se.fit),
                        lower = fit - (crit.t * se.fit))

#  need equal lengths everywhere (i.e., 200 rows)
S.cons_new_sigperiod<-data.frame(approx(S.cons_new$Year, S.cons_new$fit, n=200)) 
colnames(S.cons_new_sigperiod)<-c("Year", "fit")
head(S.cons_new_sigperiod) # Check that it looks right. 

# Identify derivatives of significant value
BTP_S.cons_der<-derivatives(S.cons_gam, type="central", n=200)
draw(BTP_S.cons_der)

BTP_S.cons_sig<-signifD(S.cons_new_sigperiod$fit, d= BTP_S.cons_der$derivative, BTP_S.cons_der$upper, BTP_S.cons_der$lower)
BTP_S.cons_sig

#plot
S.cons<-ggplot()+
  geom_ribbon(data=S.cons_new, mapping= aes(x= Year, ymax= upper, ymin=lower), fill="#b3e0ff", inherit.aes = FALSE, alpha=0.5)+
  geom_line(data= S.cons_new, mapping=aes(y= fit, x= Year), colour="lightblue4")+
  geom_line(data= S.cons_new_sigperiod, mapping=aes(y=BTP_S.cons_sig$decr, x=Year), colour="deepskyblue4", size=1)+ ## Periods of increase
  geom_line(data=S.cons_new_sigperiod, mapping=aes(y=BTP_S.cons_sig$incr, x=Year), colour="deepskyblue4", size=1)+ ## Periods of decrease
  geom_point(data=pd_diatoms, aes(y= S.construens, x= year), shape= 21, fill= "#0099cc", colour="lightblue4", size = 0.5)+
  scale_x_continuous(breaks=seq(1000,2022, by= 100), limits = c(1000,2022))+
  ylim(c(-15, 115))+
  labs(x=NULL, y=NULL) +
  theme_classic()+
  theme(text = element_text(size = 10, face = "bold"))
        
S.cons



## final plot ###################################################################
library(ggpubr)

ggarrange(chlA, Zn, d13C, Cd, d15N, S.exi, P, S.cons, ncol = 2, nrow = 4, heights=5, widths=5)


?mgcv
?ggplot2
