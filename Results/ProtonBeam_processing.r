###################
# Load libraries
###################
library(ggplot2)
library(plyr)

###################
# Load data
###################
Results <- read.csv("~/PhD/Brown/Papers/BMj2015/Results/ProtonBeam.csv")

###################
# Summarise answers
###################
Question1Results <- ddply(Results,~AbstractId,summarise, Yes=sum(Question1=="Yes", na.rm=TRUE), No=sum(Question1=="No", na.rm=TRUE), CantTell=sum(Question1=="CantTell", na.rm=TRUE), NoAnswer=sum(is.na(Question1) | Question1=='-'), Relevant=mean(Relevant))

Question2Results <- ddply(Results,~AbstractId,summarise, Yes=sum(Question2=="Yes", na.rm=TRUE), No=sum(Question2=="No", na.rm=TRUE), CantTell=sum(Question2=="CantTell", na.rm=TRUE), NoAnswer=sum(is.na(Question2) | Question2=='-'), Relevant=mean(Relevant))

Question3Results <- ddply(Results,~AbstractId,summarise, Yes=sum(Question3=="Yes", na.rm=TRUE), No=sum(Question3=="No", na.rm=TRUE), CantTell=sum(Question3=="CantTell", na.rm=TRUE), NoAnswer=sum(is.na(Question3) | Question3=='-'), Relevant=mean(Relevant))

Question4Results <- ddply(Results,~AbstractId,summarise, MoreThan=sum(!is.na(Question4) & Question4!='-' & Question4!='NoInfo' & as.numeric(Question4)>=10), LessThan=sum(!is.na(Question4) & Question4!='-' & Question4!='NoInfo' & as.numeric(Question4)<10), NoInfo=sum(Question4=='NoInfo'), NoAnswer=sum(is.na(Question4) | Question4=='-'), Relevant=mean(Relevant))


###################
# Majority rule
###################

Question1Positive <- subset(Question1Results, (Yes+CantTell) >= (Yes+No+CantTell+NoAnswer)/2)
Question2Positive <- subset(Question2Results, (Yes+CantTell) >= (Yes+No+CantTell+NoAnswer)/2)
Question3Positive <- subset(Question3Results, (Yes+CantTell) >= (Yes+No+CantTell+NoAnswer)/2)
Question4Positive <- subset(Question4Results, (MoreThan+NoInfo) >= (MoreThan+LessThan+NoInfo+NoAnswer)/2)

###################
# 1p / SI rule
###################

Question1Positive <- subset(Question1Results, (Yes+CantTell) >= 1)
Question2Positive <- subset(Question2Results, (Yes+CantTell) >= 1)
Question3Positive <- subset(Question3Results, (Yes+CantTell) >= 1)
Question4Positive <- subset(Question4Results, MoreThan+NoInfo >= 1)

###################
# 2p rule
###################

Question1Positive <- subset(Question1Results, (Yes+CantTell) >= pmin(2,(Yes+No+CantTell+NoAnswer)))
Question2Positive <- subset(Question2Results, (Yes+CantTell) >= pmin(2,(Yes+No+CantTell+NoAnswer)))
Question3Positive <- subset(Question3Results, (Yes+CantTell) >= pmin(2,(Yes+No+CantTell+NoAnswer)))
Question4Positive <- subset(Question4Results, MoreThan+NoInfo >= pmin(2,(MoreThan+LessThan+NoInfo+NoAnswer)))

###################
# 3p rule
###################

Question1Positive <- subset(Question1Results, (Yes+CantTell) >= pmin(3,(Yes+No+CantTell+NoAnswer)))
Question2Positive <- subset(Question2Results, (Yes+CantTell) >= pmin(3,(Yes+No+CantTell+NoAnswer)))
Question3Positive <- subset(Question3Results, (Yes+CantTell) >= pmin(3,(Yes+No+CantTell+NoAnswer)))
Question4Positive <- subset(Question4Results, MoreThan+NoInfo >= pmin(3,(MoreThan+LessThan+NoInfo+NoAnswer)))

###################
# 4p rule
###################

Question1Positive <- subset(Question1Results, (Yes+CantTell) >= pmin(4,(Yes+No+CantTell+NoAnswer)))
Question2Positive <- subset(Question2Results, (Yes+CantTell) >= pmin(4,(Yes+No+CantTell+NoAnswer)))
Question3Positive <- subset(Question3Results, (Yes+CantTell) >= pmin(4,(Yes+No+CantTell+NoAnswer)))
Question4Positive <- subset(Question4Results, MoreThan+NoInfo >= pmin(4,(MoreThan+LessThan+NoInfo+NoAnswer)))

###################
# 5p / AI rule
###################

Question1Positive <- subset(Question1Results, (Yes+CantTell) == (Yes+No+CantTell+NoAnswer))
Question2Positive <- subset(Question2Results, (Yes+CantTell) == (Yes+No+CantTell+NoAnswer))
Question3Positive <- subset(Question3Results, (Yes+CantTell) == (Yes+No+CantTell+NoAnswer))
Question4Positive <- subset(Question4Results, (MoreThan+NoInfo) == (MoreThan+LessThan+NoInfo+NoAnswer))

########################
# Majority Question rule
########################

MQres <- ddply(Results,~AbstractId,summarise,q1p=sum(Question1=='Yes' | Question1=='CantTell'),q1t=length(Question1),q2p=sum(Question2=='Yes' | Question2=='CantTell'),q2t=length(Question2),q3p=sum(Question3=='Yes' | Question3=='CantTell'),q3t=length(Question3),q4p=sum(!is.na(Question4) & Question4!='-' & Question4=='NoInfo' | as.numeric(Question4)>=10),q4t=length(Question4), Relevant=mean(Relevant))
MQPositive <- subset(MQres, q1p+q2p+q3p+q4p > ((q1t+q2t+q3t+q4t)/2)) 
nrow(subset(MQPositive, Relevant==1))/nrow(MQPositive)
nrow(subset(MQPositive, Relevant==1))/nrow(subset(ddply(Results,~AbstractId,summarise,relevant=mean(Relevant)), relevant==1))

###################
# Champion rule
###################

Cres <- ddply(Results,~AbstractId,summarise,q1p=sum(Question1=='Yes' | Question1=='CantTell'),q1t=length(Question1[Question1!='-']),q2p=sum(Question2=='Yes' | Question2=='CantTell'),q2t=length(Question2[Question2!='-']),q3p=sum(Question3=='Yes' | Question3=='CantTell'),q3t=length(Question3[Question3!='-']),q4p=sum(!is.na(Question4) & Question4!='-' & (Question4=='NoInfo' | as.numeric(Question4)>=10)),q4t=length(Question4[Question4!='-']), Relevant=mean(Relevant))
Question1Positive <- subset(Cres, q1p >= q1t/2)
Question2Positive <- subset(Question1Positive, q2p >= q2t/2)
Question3Positive <- subset(Question2Positive, q3p >= q3t/2)
Question4Positive <- subset(Question3Positive, q4p >= q4t/2)

###################
# Champion DR rule
###################

Cres <- ddply(Results,~AbstractId,summarise,q1p=sum(Question1=='Yes' | Question1=='CantTell'),q1t=length(Question1[Question1!='-']),q2p=sum(Question2=='Yes' | Question2=='CantTell'),q2t=length(Question2[Question2!='-']),q3p=sum(Question3=='Yes' | Question3=='CantTell'),q3t=length(Question3[Question3!='-']),q4p=sum(!is.na(Question4) & Question4!='-' & (Question4=='NoInfo' | as.numeric(Question4)>=10)),q4t=length(Question4[Question4!='-']), Relevant=mean(Relevant))
Question1Positive <- subset(Cres, q1p >= pmax(pmin(5/2,q1t/2),0))
Question2Positive <- subset(Question1Positive, q2p >= pmax(pmin(4/2,q2t/2),0))
Question3Positive <- subset(Question2Positive, q3p >= pmax(pmin(3/2,q3t/2),0))
Question4Positive <- subset(Question3Positive, q4p >= 1)

###################
# Precision/Recall
###################

nrow(subset(Question1Positive, Relevant==1))/nrow(Question1Positive)
nrow(subset(Question1Positive, Relevant==1))/nrow(subset(ddply(Results,~AbstractId,summarise,relevant=mean(Relevant)), relevant==1))
nrow(subset(Question2Positive, Relevant==1))/nrow(Question2Positive)
nrow(subset(Question2Positive, Relevant==1))/nrow(subset(ddply(Results,~AbstractId,summarise,relevant=mean(Relevant)), relevant==1))
nrow(subset(Question3Positive, Relevant==1))/nrow(Question3Positive)
nrow(subset(Question3Positive, Relevant==1))/nrow(subset(ddply(Results,~AbstractId,summarise,relevant=mean(Relevant)), relevant==1))
nrow(subset(Question4Positive, Relevant==1))/nrow(Question4Positive)
nrow(subset(Question4Positive, Relevant==1))/nrow(subset(ddply(Results,~AbstractId,summarise,relevant=mean(Relevant)), relevant==1))

