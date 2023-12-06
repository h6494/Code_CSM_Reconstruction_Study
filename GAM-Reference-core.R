
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
pd_chlA <-read.csv("ref3/chlA.csv")
head(pd_chlA) #Check that data looks correct

#REML FUNCTION
chlA_gam<-gam(chla~s(depth, k = 23), data= pd_chlA, method="REML") 
gam.check(chlA_gam, old.style = TRUE)
plot(chlA_gam)

# And now add standard error
fit_gcv<- predict(chlA_gam, data = pd_chlA, se.fit = TRUE, length.out=200)
crit.t <- qt(0.975, df.residual(chlA_gam))
chlA_new <- data.frame(Depth = pd_chlA[["depth"]],
                        fit = fit_gcv$fit,
                        se.fit = fit_gcv$se.fit)

chlA_new <- transform(chlA_new,
                       upper = fit + (crit.t * se.fit),
                       lower = fit - (crit.t * se.fit))
chlA_new
#  need equal lengths everywhere (i.e., 200 rows)
chlA_new_sigperiod<-data.frame(approx(chlA_new$Depth, chlA_new$fit, n=200)) 
colnames(chlA_new_sigperiod)<-c("Depth", "fit")
head(chlA_new_sigperiod) # Check that it looks right. 
chlA_new_sigperiod

# Identify derivatives of significant value
BTP_chlA_der<-derivatives(chlA_gam, type="central", n=200)
draw(BTP_chlA_der)

BTP_chlA_sig <- signifD(chlA_new_sigperiod$fit, d= BTP_chlA_der$derivative, BTP_chlA_der$upper, BTP_chlA_der$lower)

BTP_chlA_sig


#plot
chlA<-ggplot()+
  geom_ribbon(data=chlA_new, mapping= aes(x= Depth, ymax= upper, ymin=lower), fill="#b3e0ff", inherit.aes = FALSE, alpha=0.5)+
  geom_line(data= chlA_new_sigperiod, mapping=aes(y=BTP_chlA_sig$decr, x=Depth), colour="deepskyblue4", size=1)+ ## Periods of increase
  geom_line(data=chlA_new_sigperiod, mapping=aes(y=BTP_chlA_sig$incr, x=Depth), colour="deepskyblue4", size=1)+ ## Periods of decrease
  geom_point(data=pd_chlA, aes(y= chla, x= depth), shape= 21, fill= "#0099cc", colour="lightblue4", size = 0.5)+
  ggtitle("chlorophyll a (mg/g dry wt.") +
  scale_x_continuous(breaks=seq(0, 21, by= 2), limits=c(0,26))+
  ylim(c(-0.002, 0.030))+
  labs(x=NULL, y=NULL) +
  theme_classic()

chlA

##  d15N  ##############################################################################

pd_d15N <-read.csv("ref3/isotopes.csv")
head(pd_d15N) #Check that data looks correct

#REML FUNCTION
d15N_gam<-gam(d15N~s(depth, k = 16), data= pd_d15N, method="REML") 
gam.check(d15N_gam)
plot(d15N_gam)

# And now add standard error
fit_gcv<- predict(d15N_gam, data = pd_d15N, se.fit = TRUE, length.out=200)
crit.t <- qt(0.975, df.residual(d15N_gam))
d15N_new <- data.frame(Depth = pd_d15N[["depth"]],
                        fit = fit_gcv$fit,
                        se.fit = fit_gcv$se.fit)

d15N_new <- transform(d15N_new,
                       upper = fit + (crit.t * se.fit),
                       lower = fit - (crit.t * se.fit))

#  need equal lengths everywhere (i.e., 200 rows)
d15N_new_sigperiod<-data.frame(approx(d15N_new$Depth, d15N_new$fit, n=200)) 
colnames(d15N_new_sigperiod)<-c("Depth", "fit")
head(d15N_new_sigperiod) # Check that it looks right. 

# Identify derivatives of significant value
BTP_d15N_der<-derivatives(d15N_gam, type="central", n=200)
draw(BTP_d15N_der)

BTP_d15N_sig<-signifD(d15N_new_sigperiod$fit, d= BTP_d15N_der$derivative, BTP_d15N_der$upper, BTP_d15N_der$lower)
BTP_d15N_sig


#plot
d15N<-ggplot()+
  geom_ribbon(data=d15N_new, mapping= aes(x= Depth, ymax= upper, ymin=lower), fill="#b3e0ff", inherit.aes = FALSE, alpha=0.5)+
  geom_line(data= d15N_new_sigperiod, mapping=aes(y=BTP_d15N_sig$decr, x=Depth), colour="deepskyblue4", size=1)+ ## Periods of increase
  geom_line(data=d15N_new_sigperiod, mapping=aes(y=BTP_d15N_sig$incr, x=Depth), colour="deepskyblue4", size=1)+ ## Periods of decrease
  geom_point(data=pd_d15N, aes(y= d15N, x= depth), shape= 21, fill= "#0099cc", colour="lightblue4", size = 0.5)+
  ggtitle("δ15N") +
  scale_x_continuous(breaks=seq(0, 21, by= 2), limits=c(0,26))+
  ylim(c(-4, 2))+
  labs(x=NULL, y=NULL) +
  theme_classic()

d15N

##  Phosphorus  ##############################################################################
pd_metals <-read.csv("ref3/metals.csv")
head(pd_metals) #Check that data looks correct

#REML FUNCTION
P_gam<-gam(P~s(depth, k = 13), data= pd_metals, method="REML") 
gam.check(P_gam)
plot(P_gam)

# And now add standard error
fit_gcv<- predict(P_gam, data = pd_metals, se.fit = TRUE, length.out=200)
crit.t <- qt(0.975, df.residual(P_gam))
P_new <- data.frame(Depth = pd_metals[["depth"]],
                     fit = fit_gcv$fit,
                     se.fit = fit_gcv$se.fit)

P_new <- transform(P_new,
                    upper = fit + (crit.t * se.fit),
                    lower = fit - (crit.t * se.fit))

#  need equal lengths everywhere (i.e., 200 rows)
P_new_sigperiod<-data.frame(approx(P_new$Depth, P_new$fit, n=200)) 
colnames(P_new_sigperiod)<-c("Depth", "fit")
head(P_new_sigperiod) # Check that it looks right. 

# Identify derivatives of significant value
BTP_P_der<-derivatives(P_gam, type="central", n=200)
draw(BTP_P_der)

BTP_P_sig<-signifD(P_new_sigperiod$fit, d= BTP_P_der$derivative, BTP_P_der$upper, BTP_P_der$lower)
BTP_P_sig

#plot

P<-ggplot()+
  geom_ribbon(data=P_new, mapping= aes(x= Depth, ymax= upper, ymin=lower), fill="#b3e0ff", inherit.aes = FALSE, alpha=0.5)+
  geom_line(data= P_new_sigperiod, mapping=aes(y=BTP_P_sig$decr, x=Depth), colour="deepskyblue4", size=1)+ ## Periods of increase
  geom_line(data=P_new_sigperiod, mapping=aes(y=BTP_P_sig$incr, x=Depth), colour="deepskyblue4", size=1)+ ## Periods of decrease
  geom_point(data=pd_metals, aes(y= P, x= depth), shape= 21, fill= "#0099cc", colour="lightblue4", size = 0.5)+
  ggtitle("P") +
  scale_x_continuous(breaks=seq(0, 21, by= 2), limits=c(0,26))+
  ylim(c(330, 2600))+
  labs(x=NULL, y=NULL) +
  theme_classic()
P


##  Zinc/Al  ##############################################################################
pd_metals <-read.csv("ref3/metals.csv")
head(pd_metals) #Check that data looks correct

#REML FUNCTION
Zn_gam<-gam(Zn.Al~s(depth, k = 13), data= pd_metals, method="REML") 
gam.check(Zn_gam)
plot(Zn_gam)

# And now add standard error
fit_gcv<- predict(Zn_gam, data = pd_metals, se.fit = TRUE, length.out=200)
crit.t <- qt(0.975, df.residual(Zn_gam))
Zn_new <- data.frame(Depth = pd_metals[["depth"]],
                      fit = fit_gcv$fit,
                      se.fit = fit_gcv$se.fit)

Zn_new <- transform(Zn_new,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))

#  need equal lengths everywhere (i.e., 200 rows)
Zn_new_sigperiod<-data.frame(approx(Zn_new$Depth, Zn_new$fit, n=200)) 
colnames(Zn_new_sigperiod)<-c("Depth", "fit")

head(Zn_new_sigperiod) # Check that it looks right. 

# Identify derivatives of significant value
BTP_Zn_der<-derivatives(Zn_gam, type="central", n=200)
draw(BTP_Zn_der)

BTP_Zn_sig<-signifD(Zn_new_sigperiod$fit, d= BTP_Zn_der$derivative, BTP_Zn_der$upper, BTP_Zn_der$lower)
BTP_Zn_sig

#plot

Zn<-ggplot()+
  geom_ribbon(data=Zn_new, mapping= aes(x= Depth, ymax= upper, ymin=lower), fill="#b3e0ff", inherit.aes = FALSE, alpha=0.5)+
  geom_line(data= Zn_new_sigperiod, mapping=aes(y=BTP_Zn_sig$decr, x=Depth), colour="deepskyblue4", size=1)+ ## Zneriods of increase
  geom_line(data=Zn_new_sigperiod, mapping=aes(y=BTP_Zn_sig$incr, x=Depth), colour="deepskyblue4", size=1)+ ## Zneriods of decrease
  geom_point(data=pd_metals, aes(y= Zn.Al, x= depth), shape= 21, fill= "#0099cc", colour="lightblue4", size = 0.5)+
  ggtitle("Zn") +
  scale_x_continuous(breaks=seq(0, 21, by= 2), limits=c(0,26))+
  ylim(c(0.01444444, 0.02142857))+
  labs(x=NULL, y=NULL) +
  theme_classic()

Zn
##  z_Cadmium/Al ##############################################################################
pd_metals <-read.csv("ref3/metals.csv")
head(pd_metals) #Check that data looks correct

#REML FUNCTION
Cd_gam<-gam(Cd.Al~s(depth, k = 13), data= pd_metals, method="REML") 
gam.check(Cd_gam)
plot(Cd_gam)

# And now add standard error
fit_gcv<- predict(Cd_gam, data = pd_metals, se.fit = TRUE, length.out=200)
crit.t <- qt(0.975, df.residual(Cd_gam))
Cd_new <- data.frame(Depth = pd_metals[["depth"]],
                      fit = fit_gcv$fit,
                      se.fit = fit_gcv$se.fit)

Cd_new <- transform(Cd_new,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))

#  need equal lengths everywhere (i.e., 200 rows)
Cd_new_sigperiod<-data.frame(approx(Cd_new$Depth, Cd_new$fit, n=200)) 
colnames(Cd_new_sigperiod)<-c("Depth", "fit")
head(Cd_new_sigperiod) # Check that it looks right. 

# Identify derivatives of significant value
BTP_Cd_der<-derivatives(Cd_gam, type="central", n=200)
draw(BTP_Cd_der)

BTP_Cd_sig<-signifD(Cd_new_sigperiod$fit, d= BTP_Cd_der$derivative, BTP_Cd_der$upper, BTP_Cd_der$lower)
BTP_Cd_sig

#plot
Cd<-ggplot()+
  geom_ribbon(data=Cd_new, mapping= aes(x= Depth, ymax= upper, ymin=lower), fill="#b3e0ff", inherit.aes = FALSE, alpha=0.5)+
  geom_line(data= Cd_new_sigperiod, mapping=aes(y=BTP_Cd_sig$decr, x=Depth), colour="deepskyblue4", size=1)+ ## Cderiods of increase
  geom_line(data=Cd_new_sigperiod, mapping=aes(y=BTP_Cd_sig$incr, x=Depth), colour="deepskyblue4", size=1)+ ## Cderiods of decrease
  geom_point(data=pd_metals, aes(y= Cd.Al, x= depth), shape= 21, fill= "#0099cc", colour="lightblue4", size = 0.5)+
  ggtitle("Cd") +
  scale_x_continuous(breaks=seq(0, 21, by= 2), limits=c(0,26))+
  ylim(c(5.071429e-05, 1.750000e-04))+
  labs(x=NULL, y=NULL) +
  theme_classic() + 
  scale_y_reverse()

Cd

##  S.construens  ##############################################################################
pd_diatoms <-read.csv("ref3/diatoms.csv")
rel_abund <- pd_diatoms$S_construens_rel_abund
head(pd_diatoms) #Check that data looks correct

#REML FUNCTION
S.cons_gam<-gam(S_construens_rel_abund~s(depth, k = 23), data= pd_diatoms, method="REML") 
gam.check(S.cons_gam)
plot(S.cons_gam)

# And now add standard error
fit_gcv<- predict(S.cons_gam, data = pd_diatoms, se.fit = TRUE, length.out=200)
crit.t <- qt(0.975, df.residual(S.cons_gam))
S.cons_new <- data.frame(Depth = pd_diatoms[["depth"]],
                         fit = fit_gcv$fit,
                         se.fit = fit_gcv$se.fit)

S.cons_new <- transform(S.cons_new,
                        upper = fit + (crit.t * se.fit),
                        lower = fit - (crit.t * se.fit))

#  need equal lengths everywhere (i.e., 200 rows)
S.cons_new_sigperiod<-data.frame(approx(S.cons_new$Depth, S.cons_new$fit, n=200)) 
colnames(S.cons_new_sigperiod)<-c("Depth", "fit")
head(S.cons_new_sigperiod) # Check that it looks right. 

# Identify derivatives of significant value
BTP_S.cons_der<-derivatives(S.cons_gam, type="central", n=200)
draw(BTP_S.cons_der)

BTP_S.cons_sig<-signifD(S.cons_new_sigperiod$fit, d= BTP_S.cons_der$derivative, BTP_S.cons_der$upper, BTP_S.cons_der$lower)
BTP_S.cons_sig

#plot
S.cons<-ggplot()+
  geom_ribbon(data=S.cons_new, mapping= aes(x= Depth, ymax= upper, ymin=lower), fill="#b3e0ff", inherit.aes = FALSE, alpha=0.5)+
  geom_line(data= S.cons_new_sigperiod, mapping=aes(y=BTP_S.cons_sig$decr, x=Depth), colour="deepskyblue4", size=1)+ ## S.conseriods of increase
  geom_line(data=S.cons_new_sigperiod, mapping=aes(y=BTP_S.cons_sig$incr, x=Depth), colour="deepskyblue4", size=1)+ ## S.conseriods of decrease
  geom_point(data=pd_diatoms, aes(y=rel_abund , x= depth), shape= 21, fill= "#0099cc", colour="lightblue4", size = 0.5)+
  ggtitle("S.construens") +
  scale_x_continuous(breaks=seq(0, 21, by= 2), limits=c(0,26))+
  ylim(c(0,100))+
  labs(x=NULL, y=NULL) +
  theme_classic()

S.cons


#############################################################
library(ggpubr)

ggarrange(chlA, Zn, Cd, d15N, S.cons, P, ncol = 2, nrow = 3, heights=5, widths=5)

