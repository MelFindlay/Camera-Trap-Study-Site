###################
#MINIMUM SURVEY DURATION #
###################

#LOAD PACKAGES
require(manipulate)
require(Hmisc)

# TO INVESTIGATE PRESENCE, I.E. ALL REGISTRATIONS READ IN 3_MSD PRESENCE.
#THEN REPEAT TO INVESTIGATE RESTS, READ IN 3_MSD_RESTS
main<-read.csv("3_MSD PRESENCE.csv")

#TEST FOR ANY RELATIONSHIP BETWEEN INTERVALS IN DAYS AND PRESENCE
#LOAD PACKAGE
require(lme4)

#CREATE MODEL
model<-glm(main$INTERVAL~main$FUNCTION,family=poisson)

anova(model,test="Chi")

#EXTRACT PARAMETER ESTIMATES
summary(model) 

