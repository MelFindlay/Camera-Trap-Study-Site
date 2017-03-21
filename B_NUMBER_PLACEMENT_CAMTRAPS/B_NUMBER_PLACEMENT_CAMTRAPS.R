rm(list=ls())

#LOAD DATA
main<-read.csv("B_NUMBER_PLACEMENT_CAMTRAPS.csv")

#LOAD MIXED MODELS
require(lme4)

#########################################
# NUMBER OF OTTERS  (DUAL VS. SINGLE) #
########################################
#OMIT NA'S
sub<-main[!is.na(main$S.COUNT),]

#RENAME COUNTS ON SINGLE CAM TRAP AS SUCCESSES, AND FAILURES AS THE COUNT 
#FROM THE COMBINED CAM-TRAPS MINUS THE COUNT FROM THE SINGLE CAM TRAP
successes<-sub$S.COUNT
failures<-sub$D.COUNT-sub$S.COUNT

# CREATE DUAL VECTOR OF SUCCESSES AND FAILURES WHICH WILL BE RESPONSE VARIABLE
y<-cbind(successes,failures)
  
# GENERATE NULL MODEL TO ESTIMATE MEAN PROBABILITY OF SUCCESSES
null.model<-glmer(formula=y~1+(1|CODE),family=binomial,data=sub)

# FURTHER MODELS
model1<-glmer(formula=y~SINGLE.CAM+(1|CODE),family=binomial,data=sub)
model2<-glmer(formula=y~SINGLE.CAM+D.COUNT+(1|CODE),family=binomial,data=sub)
model3<-glmer(formula=y~SINGLE.CAM+D.COUNT+SINGLE.CAM:D.COUNT+(1|CODE),family=binomial,data=sub)

# MODEL COMPARISON USING LIKELIHOOD RATIO TESTS
#TEST INTERACTION
anova(model3,model2,test="Chi")
#TEST MAIN EFFECTS
anova(model1,model2,test="Chi")
#IF NEEDED
anova(null.model,model1,test="Chi")

#################################################
# TEST IF SUCCESS OF RECORDING BEHAVIOURS AFFECTED 
#BY CAMERA POSITION AND GROUP SIZE OR INTERACTION
################################################

main<-read.csv("B_NUMBER_PLACEMENT_CAMTRAPS.csv")

#CREATE A DUAL VECTOR (AS RESPONSE VARIABLE) OF SUCCESSES AND FAILURES RENAMING s.beh.count AS SUCCESS 
#AND d.beh.count-s.beh.count AS FAILURES#
successes<-main$S.BEH.COUNT
failures<-main$D.BEH.COUNT-main$S.BEH.COUNT
y<-cbind(successes,failures)


#create models#
null.model<-glmer(formula=y~1+(1|CODE),family=binomial,data=main)
model1<-glmer(formula=y~SINGLE.CAM+(1|CODE),family=binomial,data=main)
model2<-glmer(formula=y~SINGLE.CAM+D.COUNT+(1|CODE),family=binomial,data=main)
model3<-glmer(formula=y~SINGLE.CAM*D.COUNT+(1|CODE),family=binomial,data=main)

# MODEL COMPARISON USING LIKELIHOOD RATIO TESTS
#TEST INTERACTION
anova(model3,model2,test="Chi")
#TEST MAIN EFFECTS
anova(model1,model2,test="Chi")
#IF NEEDED
anova(null.model,model1,test="Chi")

#####################
# TEST IF ABILITY TO ID SEX ID IS AFFECTED BY CAMERA POSITION
#OR GROUP SIZE OR INTERACTION
#####################

# CREATE A NEW VARIABLE (A COPY) FOR BINARY VERSION OF main$D.SEX

D.SEX.ID<-main$D.SEX
S.SEX.ID<-main$S.AD.SEX

# FIND OUT THE ORDERING OF THE LEVELS OF VARIABLE
levels(D.SEX.ID)
levels(S.SEX.ID)

# HIJAC LEVELS, REPLACING THEM WITH Y IF SEX WAS IDENTIFIED AND N IF NOT.
#IN CSV FILE U DENOTES UNKNOWN AND IS REPLACED WITH N
levels(D.SEX.ID)<-c("Y","Y","Y","N")
levels(S.SEX.ID)<-c("Y","Y","Y","N")
D.SEX.ID<-factor(D.SEX.ID, levels=rev(levels(D.SEX.ID)))
S.SEX.ID<-factor(S.SEX.ID, levels=rev(levels(S.SEX.ID)))

# ADD THIS TO MAIN DATAFRAME
main<-data.frame(main,D.SEX.ID,S.SEX.ID)

# FILTER DATAFRAME USING SQUARE BRACKETS AND A LOGICAL STATEMENT
subs<-main[main$D.SEX.ID=="Y",]

modeln<-glmer(S.SEX.ID~1+(1|CODE),family=binomial,data=subs)
model1<-glmer(S.SEX.ID~SINGLE.CAM+(1|CODE),family=binomial,data=subs)
model2<-glmer(S.SEX.ID~SINGLE.CAM+S.COUNT+(1|CODE),family=binomial,data=subs)
model3<-glmer(S.SEX.ID~SINGLE.CAM*S.COUNT+(1|CODE),family=binomial,data=subs)

#TEST MODELS
anova(model3,model2,test="Chi")
anova(model2,model1,test="Chi")
anova(model1,modeln,test="Chi")


##########################
#TO PLOT PROBABILITY ON y AGAINST NUMBER OF OTTERS ON X
#########################

#CREATE FUNCTION TO CONVERT FROM LOGITS
convert<-function(x){exp(x)/(1+exp(x))}

#GIVE VALUES TO X AXIS
x<-seq(1,4,1)

#y<-convert((slope*x)+intercept), USE MODEL OUTPUTS

#SEX
yclose<-convert((1.3030*x)+0.5555)
ydistant<-convert((1.303*x)+0.555-2.0922)

#COUNT
yclose<-convert((0.6144*x)+1.0874)
yrose<-convert((0.6144*x)+(1.0874-1.3030))

#PLOT
plot(x,yclose,xlab="title",ylab="title",cex.lab=0.9,ylim=c(0,1),las=1,type="l",xaxt="n")
axis(1, at = seq(-5, 15, by = 1))
lines(x,ydistant,lty=2,col="black")
legend(2,0.8,legend=c("Close camera-trap","Distant camera-trap"),lty=1:2, cex=0.7, box.lty=0)
#end#
