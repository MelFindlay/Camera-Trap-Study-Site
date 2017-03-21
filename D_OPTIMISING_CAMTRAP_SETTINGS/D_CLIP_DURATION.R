#############################
# FIRST APPARENT IN FRAME/CLIP DURATION #
#############################
rm(list=ls())

# LOAD PACKAGES
require(manipulate)
require(Hmisc)

# DATAFRAMES OF INTEREST
#4_FAF_PRIM (FOR IDENTIFYING SEX USING PRIMARY CHARACTERISTICS)
#4_FAF_PRIMANDSEC (FOR IDENTIFYING SEX USING PRIMARY AND SECONDARY CHARACTERISTICS)
#4_FAF_SCENT (SPRAINT OR URINE)

main<-read.csv("D_FAF_PRIM.csv")
main1<-read.csv("D_FAF_PRIMANDSEC.csv")
main2<-read.csv("D_FAFSCENT.csv")

#############################################################
# FAF PROBABILITY ANALYSES for PRIMARY SEX CHARACTERISTICS #
############################################################

# GENERATE APPROPRIATE VARIABLES FOR ANALYSIS 
#eg id sex from primary characteristics
 
faf<-sort(na.omit(main$PRIM.SEX))

#for analysis for primary and/or secondary, clear lists using 
#rm(list=ls())
#replace main with main1, and FAF.PRIM with FAF.PRIMSEC

#for analysis of scent, clear lists using
#rm(list=ls())
#replace main with main2, and FAF.PRIM with FAF.SCENT

#RANK DATA
ranks<-1:length(faf)
cum.prop<-function(x){x/x[length(x)]}
props<-cum.prop(ranks)

# PLOT THE DATA TO SEE HOW THEY LOOK
plot(props~faf,type="l",xlab="title",ylab="Proportion of events",lwd=2)
abline(h=c(0.75,0.90,0.95),lty=2)
minor.tick(nx=10)

#include clips 1-30 sec long
faf.z<-faf[faf>0 & faf<31]
props.z<-props[faf>0 & faf<31]

# PLOT THE DATA 
plot(props.z~faf.z,xlab="Time (s)",ylab="Proportion of events",lwd=2,ylim=c(0,1))
abline(h=c(0.95),lty=2)
minor.tick(nx=10)

###############################################
# ASSIGN VARIABLES TO x and y
x <-faf.z
y <-props.z
x1<-faf
y1<-props

#########################################
# MANIPULATE PACKAGE TO FIND BEST MODEL #
##########################################

#IMPORTANT NOTE
#THE FOLLOWING CHUNK OF CODE WILL GENERATE A GRAPH AND THREE SLIDERS WILL APPEAR
#YOU NEED TO MANUALLY MOVE THESE SLIDERS TO TRY TO MATCH THE CURVE ON THE GRAPH. 
#WHEN YOU GET SOMETHING APPROXIMATING THE SLOPE AND POSITION OF THE CURVE, YOU CAN CONTINUE THROUGH THE CODE
#AND THE SLIDERS WILL DISAPPEAR.
#THE MODEL WILL THEN USE THE PARAMETERS FROM THE VISUAL FIT AS STARTING VALUES TO WORK OUT THE BEST FIT.

start <- list() 
# GENERATE SLIDERS
manipulate( 
  { 
    plot(y ~ x,ylim=c(0,1)) 
    a <- a0;c<-c0;d<-d0
    curve(a*(1-exp(-c*x))+d, add=TRUE) 
    start <<- list(a=a,c=c,d=d) 
  },
  a0 = slider(0, 2, step=0.001, initial = 0),
  c0 = slider(0, 1, step=0.01, initial = 0),
  d0 = slider(0, 1, step=0.01, initial = 0)
) 
#####################################################
# FIT MODEL ISING ESTIMATES FROM SLIDER  
FAFmodel<-nls(y ~ a*(1-exp(-c*x))+d, start = start) 
summary(FAFmodel)

# MAKE PREDICTIONS OVER A SEQUENCE OF X VALUES AND PLOT 
ndata <- data.frame(x = seq(1, max(x), by = 0.1))
wpred <- predict(FAFmodel, newdata = ndata)
plot(y ~ x, pch = 16,xlim=c(0,30),ylim=c(0,1),cex=0.5,las=1,xlab="Time (s)",ylab="Proportion of events")
lines(ndata$x, wpred) 



############95% confidence limits##########################
# LOWER BOUNDS OF PARAMETER ESTIMATES (upper and lower error/uncertainty)
lower<-summary(FAFmodel)$parameters[,1]-summary(FAFmodel)$parameters[,2]
xs<- seq(1, max(x), by = 0.1)
a<-lower[1]
c<-lower[2]
d<-lower[3]
lower.y<-a*(1-exp(-c*xs))+d
points(lower.y~xs,type="l",lty=3)

# UPPER BOUNDS OF PARAMETER ESTIMATES
upper<-summary(FAFmodel)$parameters[,1]+summary(FAFmodel)$parameters[,2]
xs<- seq(1, max(x), by = 0.1)
a<-upper[1]
c<-upper[2]
d<-upper[3]
upper.y<-a*(1-exp(-c*xs))+d
points(upper.y~xs,type="l",lty=3)

# ADD HORIZONTAL LINES AT 95TH PERCENTIILE
abline(h=c(0.95),lty=5)
abline(v=c(24), lty=5)

