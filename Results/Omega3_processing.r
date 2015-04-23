###################
# Load libraries
###################
library(ggplot2)
library(plyr)
library(grid)

###################
# Load data
###################
Results <- subset(read.csv("~/PhD/Brown/Papers/BMj2015/Results/Omega3.csv"), Honeypot==0) # Add _Lvl2 before .csv

###################
# Load data (bad workers removed)
###################
Results <- subset(read.csv("~/PhD/Brown/Papers/BMj2015/Results/Omega3.csv"), Honeypot==0 & !(WorkerId %in% c("A1BAEFJ2OG7NJ0","A1G2TVO0KVLT9P","A1MMCEU78GYAW8","A1TJE3IG2C77IC","A248P03SBIGVNH","A2DC6TG86OSCRK","A2HR7ZIX42FEPG","A2I7D44F4BA517","A2IICB0FDT3QE2","A2Q6XBT9ZWRWPG","A2SVFFIM1EZI9R","A2XA436ESADU3D","A30R62KFQ9RBSA","A38HCU1O9OS0C5","A3EVU40JQG68SY","A3GQEWG1AAT30G","A3H35QRODK1C4K","A3NQGUINZBH5CG","A3P52V679D3Y5P","A3S5NJK6P3PGNP","A3TQZUEP9A9014","A3VQLIGEFIPKAP","AS109WTSIA8NU","AU48KLNN53BFS","AWY5WM7BEBZD8","A27QCN3GYCZTPE","A31A4YKVSOYRVS","A13W8W81TPORYZ","A2PXPKUKK3E0CW")))
# Add _Lvl2 before .csv


###################
# Summarise answers
###################
Question1Results <- ddply(Results,~AbstractId,summarise, Yes=sum(Question1=="Yes", na.rm=TRUE), No=sum(Question1=="No", na.rm=TRUE), CantTell=sum(Question1=="CantTell", na.rm=TRUE), NoAnswer=sum(is.na(Question1) | Question1=='-'), Relevant=mean(Relevant))
Question2Results <- ddply(Results,~AbstractId,summarise, Yes=sum(Question2=="Yes", na.rm=TRUE), No=sum(Question2=="No", na.rm=TRUE), CantTell=sum(Question2=="CantTell", na.rm=TRUE), NoAnswer=sum(is.na(Question2) | Question2=='-'), Relevant=mean(Relevant))
Question3Results <- ddply(Results,~AbstractId,summarise, Yes=sum(Question3=="Yes", na.rm=TRUE), No=sum(Question3=="No", na.rm=TRUE), CantTell=sum(Question3=="CantTell", na.rm=TRUE), NoAnswer=sum(is.na(Question3) | Question3=='-'), Relevant=mean(Relevant))


###################
# Majority rule
###################

Question1Positive <- subset(Question1Results, (Yes+CantTell) >= (Yes+No+CantTell+NoAnswer)/2)
Question1Negative <- subset(Question1Results, (Yes+CantTell) < (Yes+No+CantTell+NoAnswer)/2)
Question2Positive <- subset(Question2Results, (Yes+CantTell) >= (Yes+No+CantTell+NoAnswer)/2)
Question2Negative <- subset(Question2Results, (Yes+CantTell) < (Yes+No+CantTell+NoAnswer)/2)
Question3Positive <- subset(Question3Results, (Yes+CantTell) >= (Yes+No+CantTell+NoAnswer)/2)
Question3Negative <- subset(Question3Results, (Yes+CantTell) < (Yes+No+CantTell+NoAnswer)/2)


###################
# 1p / SI rule
###################

Question1Positive <- subset(Question1Results, (Yes+CantTell) >= 1)
Question1Negative <- subset(Question1Results, (Yes+CantTell) == 0)
Question2Positive <- subset(Question2Results, (Yes+CantTell) >= 1)
Question2Negative <- subset(Question2Results, (Yes+CantTell) == 0)
Question3Positive <- subset(Question3Results, (Yes+CantTell) >= 1)
Question3Negative <- subset(Question3Results, (Yes+CantTell) == 0)


###################
# 2p rule
###################

Question1Positive <- subset(Question1Results, (Yes+CantTell) >= pmin(2,(Yes+No+CantTell+NoAnswer)))
Question1Negative <- subset(Question1Results, (Yes+CantTell) < pmin(2,(Yes+No+CantTell+NoAnswer)))
Question2Positive <- subset(Question2Results, (Yes+CantTell) >= pmin(2,(Yes+No+CantTell+NoAnswer)))
Question2Negative <- subset(Question2Results, (Yes+CantTell) < pmin(2,(Yes+No+CantTell+NoAnswer)))
Question3Positive <- subset(Question3Results, (Yes+CantTell) >= pmin(2,(Yes+No+CantTell+NoAnswer)))
Question3Negative <- subset(Question3Results, (Yes+CantTell) < pmin(2,(Yes+No+CantTell+NoAnswer)))


###################
# 3p rule
###################

Question1Positive <- subset(Question1Results, (Yes+CantTell) >= pmin(3,(Yes+No+CantTell+NoAnswer)))
Question1Negative <- subset(Question1Results, (Yes+CantTell) < pmin(3,(Yes+No+CantTell+NoAnswer)))
Question2Positive <- subset(Question2Results, (Yes+CantTell) >= pmin(3,(Yes+No+CantTell+NoAnswer)))
Question2Negative <- subset(Question2Results, (Yes+CantTell) < pmin(3,(Yes+No+CantTell+NoAnswer)))
Question3Positive <- subset(Question3Results, (Yes+CantTell) >= pmin(3,(Yes+No+CantTell+NoAnswer)))
Question3Negative <- subset(Question3Results, (Yes+CantTell) < pmin(3,(Yes+No+CantTell+NoAnswer)))


###################
# 4p rule
###################

Question1Positive <- subset(Question1Results, (Yes+CantTell) >= pmin(4,(Yes+No+CantTell+NoAnswer)))
Question1Negative <- subset(Question1Results, (Yes+CantTell) < pmin(4,(Yes+No+CantTell+NoAnswer)))
Question2Positive <- subset(Question2Results, (Yes+CantTell) >= pmin(4,(Yes+No+CantTell+NoAnswer)))
Question2Negative <- subset(Question2Results, (Yes+CantTell) < pmin(4,(Yes+No+CantTell+NoAnswer)))
Question3Positive <- subset(Question3Results, (Yes+CantTell) >= pmin(4,(Yes+No+CantTell+NoAnswer)))
Question3Negative <- subset(Question3Results, (Yes+CantTell) < pmin(4,(Yes+No+CantTell+NoAnswer)))


###################
# 5p / AI rule
###################

Question1Positive <- subset(Question1Results, (Yes+CantTell) == (Yes+No+CantTell+NoAnswer))
Question1Negative <- subset(Question1Results, (Yes+CantTell) < (Yes+No+CantTell+NoAnswer))
Question2Positive <- subset(Question2Results, (Yes+CantTell) == (Yes+No+CantTell+NoAnswer))
Question2Negative <- subset(Question2Results, (Yes+CantTell) < (Yes+No+CantTell+NoAnswer))
Question3Positive <- subset(Question3Results, (Yes+CantTell) == (Yes+No+CantTell+NoAnswer))
Question3Negative <- subset(Question3Results, (Yes+CantTell) < (Yes+No+CantTell+NoAnswer))


########################
# Majority Question rule
########################

MQres <- ddply(Results,~AbstractId,summarise,q1p=sum(Question1=='Yes' | Question1=='CantTell'),q1t=length(Question1),q2p=sum(Question2=='Yes' | Question2=='CantTell'),q2t=length(Question2),q3p=sum(Question3=='Yes' | Question3=='CantTell'),q3t=length(Question3), Relevant=mean(Relevant))
MQPositive <- subset(MQres, q1p+q2p+q3p > ((q1t+q2t+q3t)/2)) 
MQNegative <- subset(MQres, q1p+q2p+q3p <= ((q1t+q2t+q3t)/2)) 
nrow(subset(MQPositive, Relevant==1))/nrow(subset(ddply(Results,~AbstractId,summarise,relevant=mean(Relevant)), relevant==1))
nrow(subset(MQNegative, Relevant==0))/(nrow(subset(MQNegative, Relevant==0))+nrow(subset(MQPositive, Relevant==0)))


###################
# Champion rule
###################

Cres <- ddply(Results,~AbstractId,summarise,q1p=sum(Question1=='Yes' | Question1=='CantTell'),q1t=length(Question1[Question1!='-']),q2p=sum(Question2=='Yes' | Question2=='CantTell'),q2t=length(Question2[Question2!='-']),q3p=sum(Question3=='Yes' | Question3=='CantTell'),q3t=length(Question3[Question3!='-']), Relevant=mean(Relevant))
Question1Positive <- subset(Cres, q1p >= q1t/2)
Question1Negative <- subset(Cres, q1p < pmax(1,q1t/2))
Question2Positive <- subset(Question1Positive, q2p >= q2t/2)
Question2Negative <- rbind(subset(Question1Positive, q2p < pmax(1,q2t/2)),Question1Negative)
Question3Positive <- subset(Question2Positive, q3p >= q3t/2)
Question3Negative <- rbind(subset(Question2Positive, q3p < pmax(1,q3t/2)),Question2Negative)


###################
# Champion DR rule
###################

Cres <- ddply(Results,~AbstractId,summarise,q1p=sum(Question1=='Yes' | Question1=='CantTell'),q1t=length(Question1[Question1!='-']),q2p=sum(Question2=='Yes' | Question2=='CantTell'),q2t=length(Question2[Question2!='-']),q3p=sum(Question3=='Yes' | Question3=='CantTell'),q3t=length(Question3[Question3!='-']), Relevant=mean(Relevant))
Question1Positive <- subset(Cres, q1p >= pmax(pmin(5/2,q1t/2),0))
Question1Negative <- subset(Cres, q1p < pmax(pmin(5/2,q1t/2),0))
Question2Positive <- subset(Question1Positive, q2p >= pmax(pmin(4/2,q2t/2),0))
Question2Negative <- rbind(subset(Question1Positive, q2p < pmax(pmin(4/2,q2t/2),0)),Question1Negative)
Question3Positive <- subset(Question2Positive, q3p >= pmax(pmin(3/2,q3t/2),0))
Question3Negative <- rbind(subset(Question2Positive, q3p < pmax(pmin(3/2,q3t/2),0)),Question2Negative)


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
