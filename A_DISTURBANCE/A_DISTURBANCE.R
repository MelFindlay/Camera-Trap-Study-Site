rm(list=ls())

#READ IN DATA 
master<-read.csv("A_DISTURBANCE.csv")

#CONVERT CHECK id FROM NUMBER TO FACTOR. 
#NOTES: Check ID is a numerical identifier for each event
#variable called "rest" denotes rest of over 15 mins, 1= yes 0=No
#where mins in holt = 700, this is a dummy to represent instances when otter emerges at dusk with a dry coat after 
#a diurnal rest but time of entry isnt known, so included as a rest
master$CHECK.ID<-as.factor(master$CHECK.ID)

#LOAD MIXED MODELS
require(lme4)

#EXCLUDE OUTLIERS BY CREATING SUBSET OF DATA
master.sub <- subset(master,DAYS.SINCE.CHECK < 20)


# GENRATE VARIABLE THAT GROUPS NATAL AND EARLY REARING FUNCTIONS VS ALL OTHER FUNCTIONS

#FIRST CREATE IDENTICAL VECTOR SO THAT FUNCTION CAN BE AMENDED
natandearly<-master.sub$FUNCTION
# LOOK AT LEVELS OF ORIGINAL VECTOR
levels(master.sub$FUNCTION)
#REPLACE WITH NE MEANING NATAL OR EARLY BREEDING FUNCTION, AND C FOR ALL OTHER FUNCTIONS
levels(natandearly)<-c("C","NE","C","C","NE","C")
levels(natandearly)
#APPEND TO DATAFRAME
master.sub<-data.frame(master.sub,natandearly)

#SIMILARLY GENERATE VARIABLE REPRESENTING ALL BREEDING VS NON BREEDING AND APPEND
NB.only<-master.sub$FUNCTION
levels(master.sub$FUNCTION)
levels(NB.only)<-c("NB","B","B","B","B","B")
levels(NB.only)
master.sub<-data.frame(master.sub,NB.only)

####CREATE MODELS AND TEST########
#ANALYSIS 1. TESTING NATAL AND EARLY COMBINED VS OTHER FUNCTIONS
modelNER1<-glm(rest~natandearly*DAYS.SINCE.CHECK,family="binomial",data=master.sub)
modelNER<-glm(rest~natandearly+DAYS.SINCE.CHECK,family="binomial",data=master.sub)
modelNER1
anova(modelNER1, test="Chi")

##ANALYSIS 2. TESTING ALL BREEDING VS NON BREEDING
modelNB1<-glm(rest~NB.only*DAYS.SINCE.CHECK,family="binomial",data=master.sub)
modelNB<-glm(rest~NB.only+DAYS.SINCE.CHECK,family="binomial",data=master.sub)
modelNB1
anova(modelNB, test="Chi")













