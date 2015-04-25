#############################
## Calculate experiment time
#############################

Results <- read.csv("~/PhD/Brown/Papers/BMj2015/Results/ProtonBeam_Timing.csv")
difftime(tail(Results[,2], n=1),Results[1,2])


#############################
## Time graph plotting
#############################

PB <- read.csv("~/PhD/Brown/Papers/BMj2015/Results/ProtonBeam_Timing.csv")
PB$Dataset <- rep('Proton beam',nrow(PB))
PB$SubmitTime <- t(rbind(as.numeric(as.POSIXct(PB[,2])-min(as.POSIXct(PB[,2])))))
AP <- read.csv("~/PhD/Brown/Papers/BMj2015/Results/Appendicitis_Timing.csv")
AP$Dataset <- rep('Appendicitis',nrow(AP))
AP$SubmitTime <- t(rbind(as.numeric(as.POSIXct(AP[,2])-min(as.POSIXct(AP[,2])))))
DST <- read.csv("~/PhD/Brown/Papers/BMj2015/Results/DST_Timing.csv")
DST$Dataset <- rep('DST',nrow(DST))
DST$SubmitTime <- t(rbind(as.numeric(as.POSIXct(DST[,2])-min(as.POSIXct(DST[,2])))))
O3 <- read.csv("~/PhD/Brown/Papers/BMj2015/Results/Omega3_Timing.csv")
O3$Dataset <- rep('Omega-3',nrow(O3))
O3$SubmitTime <- t(rbind(as.numeric(as.POSIXct(O3[,2])-min(as.POSIXct(O3[,2])))))
res1 <- rbind(PB,AP)
res2 <- rbind(DST,O3)
Results <- rbind(res1,res2)

minPoints <- ddply(Results,~Dataset,subset,SubmitTime==max(SubmitTime))

ggplot(Results, aes(x = SubmitTime/3600, y = Responses, group=Dataset)) + geom_line(size=1.5,aes(color=Dataset,linetype=Dataset)) + geom_point(data = minPoints,size=8,shape=10) + scale_x_continuous("Number of hours") + scale_y_continuous("Crowd responses") + theme_bw() + theme(axis.text=element_text(size=17), axis.title.y=element_text(size=22, vjust=1.3), axis.title.x=element_text(size=22, vjust=-0.3), legend.title=element_text(size=20), legend.text=element_text(size=17)) + scale_linetype_manual(values=c("solid", "dotted","dashed","twodash"))

# Remember to change limits to accomodate different result sets
