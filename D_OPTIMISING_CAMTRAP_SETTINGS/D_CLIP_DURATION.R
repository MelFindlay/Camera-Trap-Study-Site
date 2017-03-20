#############################
# FIRST APPARENT IN FRAME/CLIP DURATION #
#############################
rm(list=ls())

# LOAD PACKAGES
require(manipulate)
require(Hmisc)

# READ IN DATAFRAMES OF INTEREST
#4_FAF_PRIM (FOR IDENTIFYING SEX USING PRIMARY CHARACTERISTICS)
#4_FAF_PRIMANDSEC (FOR IDENTIFYING SEX USING PRIMARY AND SECONDARY CHARACTERISTICS)
#4_FAF_SCENT (SPRAINT OR URINE)

main<-read.csv("D_FAF_PRIMANDSEC.csv")

############################
# FAF PROBABILITY ANALYSES #
############################

# GENERATE APPROPRIATE VARIABLES FOR ANALYSIS 
#eg id sex from primary characteristics
 
fif<-sort(na.omit(main$FAF.PRIM))

#RANK DATA
ranks<-1:length(fif)
cum.prop<-function(x){x/x[length(x)]}
props<-cum.prop(ranks)

# PLOT THE DATA TO SEE HOW THEY LOOK
plot(props~fif,type="l",xlab="title",ylab="Proportion of events",lwd=2)
abline(h=c(0.75,0.90,0.95),lty=2)
minor.tick(nx=10)

#include clips 1-30 sec long
fif.z<-fif[fif>0 & fif<31]
props.z<-props[fif>0 & fif<31]

# PLOT THE DATA 
plot(props.z~fif.z,xlab="Time (s)",ylab="Proportion of events",lwd=2,ylim=c(0,1))
abline(h=c(0.95),lty=2)
minor.tick(nx=10)

###############################################
# ASSIGN VARIABLES TO x and y
x <-fif.z
y <-props.z
x1<-fif
y1<-props

#########################################
# MANIPULATE PACKAGE TO FIND BEST MODEL #
##########################################
start <- list() 
# GENERATE SLIDERS
manipulate( 
  { 
    plot(y ~ x,ylim=c(0,1)) 
    a <- a0;c<-c0;d<-d0
    curve(a*(1-exp(-c*x))+d, add=TRUE) 
    start <<- list(a=a,c=c,d=d) 
  },
  a0 = slider(0, 2, step=0.001, initial = 0),  # MANIPULATE SLIDERS TO FIT CURVE FOR VALUES OF 'initial'
  c0 = slider(0, 1, step=0.01, initial = 0),
  d0 = slider(0, 1, step=0.01, initial = 0)
) 
#####################################################
# FIT MODEL ISING ESTIMATES FROM SLIDERS, THESE ARE USED AS A STARTING POINT  
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
upper<-summary(FIFmodel)$parameters[,1]+summary(FIFmodel)$parameters[,2]
xs<- seq(1, max(x), by = 0.1)
a<-upper[1]
c<-upper[2]
d<-upper[3]
upper.y<-a*(1-exp(-c*xs))+d
points(upper.y~xs,type="l",lty=3)

# ADD HORIZONTAL LINES AT 95TH PERCENTIILE
abline(h=c(0.95),lty=5)
abline(v=c(24), lty=5)

