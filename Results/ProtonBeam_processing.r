###################
# Load libraries
###################
library(ggplot2)
library(plyr)
library(grid)

###################
# Load data
###################
Results <- read.csv("~/PhD/Brown/Papers/BMj2015/Results/ProtonBeam.csv") # Add _Lvl2 before .csv

###################
# Load data (bad workers removed)
###################
Results <- subset(read.csv("~/PhD/Brown/Papers/BMj2015/Results/ProtonBeam.csv"),  !(WorkerId %in% c("A1BAEFJ2OG7NJ0","A1G2TVO0KVLT9P","A1MMCEU78GYAW8","A1REI6H0566OKD","A1TJE3IG2C77IC","A248P03SBIGVNH","A2DC6TG86OSCRK","A2HR7ZIX42FEPG","A2I7D44F4BA517","A2IICB0FDT3QE2","A2Q6XBT9ZWRWPG","A2SVFFIM1EZI9R","A2XA436ESADU3D","A38HCU1O9OS0C5","A3EVU40JQG68SY","A3GQEWG1AAT30G","A3H35QRODK1C4K","A3HPVFAF1U9WXK","A3NQGUINZBH5CG","A3P52V679D3Y5P","A3S5NJK6P3PGNP","A3TQZUEP9A9014","A3VQLIGEFIPKAP","AS109WTSIA8NU","AU48KLNN53BFS","AWY5WM7BEBZD8","A997OZ3H2B3Q2","A27QCN3GYCZTPE","A31A4YKVSOYRVS","A13W8W81TPORYZ","A2PXPKUKK3E0CW"))) # Add _Lvl2 before .csv


###################
# Summarise answers
###################
Question1Results <- ddply(Results,~AbstractId,summarise, Yes=sum(Question1=="Yes", na.rm=TRUE), No=sum(Question1=="No", na.rm=TRUE), CantTell=sum(Question1=="CantTell", na.rm=TRUE), NoAnswer=sum(is.na(Question1) | Question1=='-'), Relevant=mean(Relevant))

Question2Results <- ddply(Results,~AbstractId,summarise, Yes=sum(Question2=="Yes", na.rm=TRUE), No=sum(Question2=="No", na.rm=TRUE), CantTell=sum(Question2=="CantTell", na.rm=TRUE), NoAnswer=sum(is.na(Question2) | Question2=='-'), Relevant=mean(Relevant))

Question3Results <- ddply(Results,~AbstractId,summarise, Yes=sum(Question3=="Yes", na.rm=TRUE), No=sum(Question3=="No", na.rm=TRUE), CantTell=sum(Question3=="CantTell", na.rm=TRUE), NoAnswer=sum(is.na(Question3) | Question3=='-'), Relevant=mean(Relevant))

numericQ4 <- subset(Results, as.numeric(as.character(Question4))>0)

Question4Results <- merge(ddply(numericQ4,~AbstractId,summarise, MoreThan=sum(as.numeric(as.character(Question4))>=10), LessThan=sum(as.numeric(as.character(Question4))<10)), ddply(Results,~AbstractId, summarise, NoInfo=sum(Question4=='NoInfo'), NoAnswer=sum(is.na(Question4) | Question4=='-'), Relevant=mean(Relevant)), 
all=TRUE)

Question4Results[is.na(Question4Results)] <- 0



###################
# Majority rule
###################

Question1Positive <- subset(Question1Results, (Yes+CantTell) >= (Yes+No+CantTell+NoAnswer)/2)
Question1Negative <- subset(Question1Results, (Yes+CantTell) < (Yes+No+CantTell+NoAnswer)/2)
Question2Positive <- subset(Question2Results, (Yes+CantTell) >= (Yes+No+CantTell+NoAnswer)/2)
Question2Negative <- subset(Question2Results, (Yes+CantTell) < (Yes+No+CantTell+NoAnswer)/2)
Question3Positive <- subset(Question3Results, (Yes+CantTell) >= (Yes+No+CantTell+NoAnswer)/2)
Question3Negative <- subset(Question3Results, (Yes+CantTell) < (Yes+No+CantTell+NoAnswer)/2)
Question4Positive <- subset(Question4Results, (MoreThan+NoInfo) >= (MoreThan+LessThan+NoInfo+NoAnswer)/2)
Question4Negative <- subset(Question4Results, (MoreThan+NoInfo) < (MoreThan+LessThan+NoInfo+NoAnswer)/2)


###################
# 1p / SI rule
###################

Question1Positive <- subset(Question1Results, (Yes+CantTell) >= 1)
Question2Negative <- subset(Question1Results, (Yes+CantTell) < 1)
Question2Positive <- subset(Question2Results, (Yes+CantTell) >= 1)
Question2Negative <- subset(Question2Results, (Yes+CantTell) < 1)
Question3Positive <- subset(Question3Results, (Yes+CantTell) >= 1)
Question3Negative <- subset(Question3Results, (Yes+CantTell) < 1)
Question4Positive <- subset(Question4Results, MoreThan+NoInfo >= 1)
Question4Negative <- subset(Question4Results, MoreThan+NoInfo < 1)


###################
# 2p rule
###################

Question1Positive <- subset(Question1Results, (Yes+CantTell) >= pmin(2,(Yes+No+CantTell+NoAnswer)))
Question1Negative <- subset(Question1Results, (Yes+CantTell) < pmin(2,(Yes+No+CantTell+NoAnswer)))
Question2Positive <- subset(Question2Results, (Yes+CantTell) >= pmin(2,(Yes+No+CantTell+NoAnswer)))
Question2Negative <- subset(Question2Results, (Yes+CantTell) < pmin(2,(Yes+No+CantTell+NoAnswer)))
Question3Positive <- subset(Question3Results, (Yes+CantTell) >= pmin(2,(Yes+No+CantTell+NoAnswer)))
Question3Negative <- subset(Question3Results, (Yes+CantTell) < pmin(2,(Yes+No+CantTell+NoAnswer)))
Question4Positive <- subset(Question4Results, MoreThan+NoInfo >= pmin(2,(MoreThan+LessThan+NoInfo+NoAnswer)))
Question4Negative <- subset(Question4Results, MoreThan+NoInfo < pmin(2,(MoreThan+LessThan+NoInfo+NoAnswer)))


###################
# 3p rule
###################

Question1Positive <- subset(Question1Results, (Yes+CantTell) >= pmin(3,(Yes+No+CantTell+NoAnswer)))
Question1Negative <- subset(Question1Results, (Yes+CantTell) < pmin(3,(Yes+No+CantTell+NoAnswer)))
Question2Positive <- subset(Question2Results, (Yes+CantTell) >= pmin(3,(Yes+No+CantTell+NoAnswer)))
Question2Negative <- subset(Question2Results, (Yes+CantTell) < pmin(3,(Yes+No+CantTell+NoAnswer)))
Question3Positive <- subset(Question3Results, (Yes+CantTell) >= pmin(3,(Yes+No+CantTell+NoAnswer)))
Question3Negative <- subset(Question3Results, (Yes+CantTell) < pmin(3,(Yes+No+CantTell+NoAnswer)))
Question4Positive <- subset(Question4Results, MoreThan+NoInfo >= pmin(3,(MoreThan+LessThan+NoInfo+NoAnswer)))
Question4Negative <- subset(Question4Results, MoreThan+NoInfo < pmin(3,(MoreThan+LessThan+NoInfo+NoAnswer)))


###################
# 4p rule
###################

Question1Positive <- subset(Question1Results, (Yes+CantTell) >= pmin(4,(Yes+No+CantTell+NoAnswer)))
Question1Negative <- subset(Question1Results, (Yes+CantTell) < pmin(4,(Yes+No+CantTell+NoAnswer)))
Question2Positive <- subset(Question2Results, (Yes+CantTell) >= pmin(4,(Yes+No+CantTell+NoAnswer)))
Question2Negative <- subset(Question2Results, (Yes+CantTell) < pmin(4,(Yes+No+CantTell+NoAnswer)))
Question3Positive <- subset(Question3Results, (Yes+CantTell) >= pmin(4,(Yes+No+CantTell+NoAnswer)))
Question3Negative <- subset(Question3Results, (Yes+CantTell) < pmin(4,(Yes+No+CantTell+NoAnswer)))
Question4Positive <- subset(Question4Results, MoreThan+NoInfo >= pmin(4,(MoreThan+LessThan+NoInfo+NoAnswer)))
Question4Negative <- subset(Question4Results, MoreThan+NoInfo < pmin(4,(MoreThan+LessThan+NoInfo+NoAnswer)))


###################
# 5p / AI rule
###################

Question1Positive <- subset(Question1Results, (Yes+CantTell) == (Yes+No+CantTell+NoAnswer))
Question1Negative <- subset(Question1Results, (Yes+CantTell) < (Yes+No+CantTell+NoAnswer))
Question2Positive <- subset(Question2Results, (Yes+CantTell) == (Yes+No+CantTell+NoAnswer))
Question2Negative <- subset(Question2Results, (Yes+CantTell) < (Yes+No+CantTell+NoAnswer))
Question3Positive <- subset(Question3Results, (Yes+CantTell) == (Yes+No+CantTell+NoAnswer))
Question3Negative <- subset(Question3Results, (Yes+CantTell) < (Yes+No+CantTell+NoAnswer))
Question4Positive <- subset(Question4Results, (MoreThan+NoInfo) == (MoreThan+LessThan+NoInfo+NoAnswer))
Question4Negative <- subset(Question4Results, (MoreThan+NoInfo) < (MoreThan+LessThan+NoInfo+NoAnswer))


########################
# Majority Question rule
########################

numericQ4 <- subset(Results, as.numeric(as.character(Question4))>0)
MQres <- merge(ddply(Results,~AbstractId,summarise,q1p=sum(Question1=='Yes' | Question1=='CantTell'),q1t=length(Question1[Question1!='-']),q3p=sum(Question3=='Yes' | Question3=='CantTell'), q3t=length(Question3[Question3!='-']), q2p=sum(Question2=='Yes' | Question2=='CantTell'), q2t=length(Question2[Question2!='-']), Relevant=mean(Relevant), q4p=sum(Question4=='CantTell' | Question4=='NoInfo'), q4t=length(Question4[Question4!='-'])), ddply(numericQ4,~AbstractId,summarise, q4i=sum(as.numeric(as.character(Question4))>=10)), all=TRUE)
MQres[is.na(MQres)] <- 0
MQPositive <- subset(MQres, q1p+q2p+q3p+q4p > ((q1t+q2t+q3t+q4t)/2)) 
MQNegative <- subset(MQres, q1p+q2p+q3p+q4p <= ((q1t+q2t+q3t+q4t)/2)) 
nrow(subset(MQPositive, Relevant==1))/nrow(subset(ddply(Results,~AbstractId,summarise,relevant=mean(Relevant)), relevant==1))
nrow(subset(MQNegative, Relevant==0))/(nrow(subset(MQNegative, Relevant==0))+nrow(subset(MQPositive, Relevant==0)))


###################
# Champion rule
###################

numericQ4 <- subset(Results, as.numeric(as.character(Question4))>0)
Cres <- merge(ddply(Results,~AbstractId,summarise,q1p=sum(Question1=='Yes' | Question1=='CantTell'),q1t=length(Question1[Question1!='-']),q3p=sum(Question3=='Yes' | Question3=='CantTell'), q3t=length(Question3[Question3!='-']), q2p=sum(Question2=='Yes' | Question2=='CantTell'), q2t=length(Question2[Question2!='-']), Relevant=mean(Relevant), q4p=sum(Question4=='CantTell' | Question4=='NoInfo'), q4t=length(Question4[Question4!='-'])), ddply(numericQ4,~AbstractId,summarise, q4i=sum(as.numeric(as.character(Question4))>=10)), all=TRUE)
Cres[is.na(Cres)] <- 0
Question1Positive <- subset(Cres, q1p >= q1t/2)
Question1Negative <- subset(Cres, q1p < pmax(1,q1t/2))
Question2Positive <- subset(Question1Positive, q2p >= q2t/2)
Question2Negative <- rbind(subset(Question1Positive, q2p < pmax(1,q2t/2)),Question1Negative)
Question3Positive <- subset(Question2Positive, q3p >= q3t/2)
Question3Negative <- rbind(subset(Question2Positive, q3p < pmax(1,q3t/2)),Question2Negative)
Question4Positive <- subset(Question3Positive, q4p+q4i >= q4t/2)
Question4Negative <- rbind(subset(Question3Positive, q4p+q4i < pmax(1,q4t/2)),Question3Negative)


###################
# Champion DR rule
###################

numericQ4 <- subset(Results, as.numeric(as.character(Question4))>0)
Cres <- merge(ddply(Results,~AbstractId,summarise,q1p=sum(Question1=='Yes' | Question1=='CantTell'),q1t=length(Question1[Question1!='-']),q3p=sum(Question3=='Yes' | Question3=='CantTell'), q3t=length(Question3[Question3!='-']), q2p=sum(Question2=='Yes' | Question2=='CantTell'), q2t=length(Question2[Question2!='-']), Relevant=mean(Relevant), q4p=sum(Question4=='CantTell' | Question4=='NoInfo'), q4t=length(Question4[Question4!='-'])), ddply(numericQ4,~AbstractId,summarise, q4i=sum(as.numeric(as.character(Question4))>=10)), all=TRUE)
Cres[is.na(Cres)] <- 0
Question1Positive <- subset(Cres, q1p >= pmax(pmin(5/2,q1t/2),0))
Question1Negative <- subset(Cres, q1p < pmax(pmin(5/2,q1t/2),0))
Question2Positive <- subset(Question1Positive, q2p >= pmax(pmin(4/2,q2t/2),0))
Question2Negative <- rbind(subset(Question1Positive, q2p < pmax(pmin(4/2,q2t/2),0)),Question1Negative)
Question3Positive <- subset(Question2Positive, q3p >= pmax(pmin(3/2,q3t/2),0))
Question3Negative <- rbind(subset(Question2Positive, q3p < pmax(pmin(3/2,q3t/2),0)),Question2Negative)
Question4Positive <- subset(Question3Positive, q4p+q4i >= 1)
Question4Negative <- rbind(subset(Question3Positive, q4p+q4i < 1),Question3Negative)


###################
# Sensitivity and Specificity
###################

nrow(subset(Question1Positive, Relevant==1))/nrow(subset(ddply(Results,~AbstractId,summarise,relevant=mean(Relevant)), relevant==1))
nrow(subset(Question1Negative, Relevant==0))/(nrow(subset(Question1Negative, Relevant==0))+nrow(subset(Question1Positive, Relevant==0)))
nrow(subset(Question2Positive, Relevant==1))/nrow(subset(ddply(Results,~AbstractId,summarise,relevant=mean(Relevant)), relevant==1))
nrow(subset(Question2Negative, Relevant==0))/(nrow(subset(Question2Negative, Relevant==0))+nrow(subset(Question2Positive, Relevant==0)))
nrow(subset(Question3Positive, Relevant==1))/nrow(subset(ddply(Results,~AbstractId,summarise,relevant=mean(Relevant)), relevant==1))
nrow(subset(Question3Negative, Relevant==0))/(nrow(subset(Question3Negative, Relevant==0))+nrow(subset(Question3Positive, Relevant==0)))
nrow(subset(Question4Positive, Relevant==1))/nrow(subset(ddply(Results,~AbstractId,summarise,relevant=mean(Relevant)), relevant==1))
nrow(subset(Question4Negative, Relevant==0))/(nrow(subset(Question4Negative, Relevant==0))+nrow(subset(Question4Positive, Relevant==0)))

