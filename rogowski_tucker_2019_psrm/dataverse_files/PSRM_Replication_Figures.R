library(RColorBrewer)
library(riverplot)
library(descr)
library(foreign)


##############
## Figure 1 ##
##############


###########################################
## BEFORE RUNNING THE CODE BELOW,        ##
## SET WORKING DIRECTORY APPROPRIATELY   ##
###########################################

valuable_gun<-read.csv("gun_values_psrm.csv")
new_nodes_gun<-read.csv("gun_nodes.csv")

palette = paste0(brewer.pal(9, "Greys"), "60")

styles = lapply(new_nodes_gun$y, function(n) {list(col = palette[n+7], lty = 0,textcol = "black")})

names(styles) = new_nodes_gun$ID

rp <- list(nodes = new_nodes_gun, edges =valuable_gun, styles=styles)
class(rp) <- c(class(rp), "riverplot")
par(mfrow=c(1,1), font.main=2, cex=1,cex.axis=.9, mgp=c(1,0,0), tcl=0, mar=c(3,2,1,2))

png("PSRM_Figure1.png",width=948,height=725)
plot(rp, plot_area = 0.95, yscale=0.06, srt=360)
text(.03,-.04, "11.1%", cex=.9)
text(.03,.14, "12.5%", cex=.9)
text(.03,.31, "13.7%", cex=.9)
text(.03,.53, "24.0%", cex=.9)
text(.03,.83, "38.7%", cex=.9)
text(.97, .83, "37.9%", cex=.9)
text(.97, .5, "29.4%", cex=.9)
text(.97,.26, "14.0%", cex=.9)
text(.97,.125, "9.9%", cex=.9)
text(.97,-.04, "8.9%", cex=.9)
dev.off()

##############
## Figure 2 ##
##############

par(mar=c(3.5,7.5,1,1), mgp=c(1.5,.25,0),mfrow=c(1,2))

## Cross-sectional
effect <- c(-.0039,.0033,-.0136,-.0222,-.0852,.0324,.0028,.0105,-.0271,.0581,-.0580,-.0597, -.0168, -.0059)
se <- c(.0326,.0455,.0461,.0341,.0537,.0473,.0598,.0388,.058,.0689,.0665,.0401, .0920, .0336)
group <- c(14,7,6,3,5,4,13,11,12,10,9,8, 1, 2)
plot(effect,group,xlim=c(-.3,.3),ylim=c(0.5,14.5),tck=0,cex.main=1,main="Cross-sectional results (Dec 2012)",cex.lab=1,cex.axis=.9,pch=20,cex=2,ylab="",yaxt="n",xlab="Difference in support for gun control")
abline(v=0,lty=2,col="DARKGREY")
points(effect,group,pch=20,cex=2)
segments(-.3125,group,.3125,group,col="LIGHTGREY")
segments(effect-se*1.96,group,effect+1.96*se,group)
par(mgp=c(2,.1,0))
axis(2,at=c(14,7,6,3,5,4,13,11,12,10,9,8, 1, 2),las=2,cex.axis=1,labels=c("All respondents","Women","Men","Non-NRA members","Parents","Non-parents","Democrats","Republicans","Independents","Liberals","Moderates","Conservatives", "Northeast", "Non-Northeast"),tck=0)


## Panel
par(mgp=c(1.5,.25,0))
effect <- c(-.0393,-.0604,-.0176,-.0359,.011,-.0837,-.0514,.0321,-.0838,-.0817,.0098,-.0715, -.0459, -.0380)
se <- c(.0229,.0329,.0316,.0255,.0506,.0248,.0409,.0293,.0418,.0265,.0446,.035, .0275, .0269)
group <- c(14,7,6,3,5,4,13,11,12,10,9,8, 1, 2)
plot(effect,group,xlim=c(-.2,.2),ylim=c(0.5,14.5),tck=0,cex.main=1,main="Panel results (Dec 2012 to Jan 2013)",cex.lab=1,cex.axis=.9,pch=20,cex=2,ylab="",yaxt="n",xlab="Change in support for gun control")
abline(v=0,lty=2,col="DARKGREY")
points(effect,group,pch=20,cex=2)
segments(-.2125,group,.2125,group,col="LIGHTGREY")
segments(effect-se*1.96,group,effect+1.96*se,group)
par(mgp=c(2,.1,0))
axis(2,at=c(14,7,6,3,5,4,13,11,12,10,9,8, 1, 2),las=2,cex.axis=1,labels=c("All respondents","Women","Men","Non-NRA members","Parents","Non-parents","Democrats","Republicans","Independents","Liberals","Moderates","Conservatives", "Northeast", "Non-Northeast"),tck=0)
dev.copy(pdf,"PSRM_Figure2.pdf",width=12,height=8)
dev.off()

##############
## Figure 3 ##
##############

par(mar=c(3.5,6,1,1), mgp=c(1.5,.25,0),mfrow=c(1,1))

effect <- c(3.11,1.99,1.23,1.08,1.06,1.11)
se <- c(.06,.139,.142,.159,.164,.124)
group <- c(3.1,2.9,2.1,1.9,1.1,.9)
plot(effect,group,xlim=c(0,3.5),ylim=c(0.5,3.5),tck=0,cex.main=1,main="",cex.lab=1,cex.axis=1,pch=20,cex=2,ylab="",xaxt="n",yaxt="n",xlab="Polarization in attitudes toward gun control")
axis(1,at=c(0,1,2,3),cex.axis=1,labels=c("0","1","2","3"),tck=0)
segments(effect-se*1.96,group,effect+1.96*se,group)
#par(mgp=c(2,.1,0))
#axis(2,at=c(3.1,2.9,2.1,1.9,1.1,.9),las=2,cex.axis=.6,labels=c("DEC12","JAN13","DEC12","JAN13","DEC12","JAN13"),tck=0)
par(mgp=c(2,.1,0))
axis(2,at=c(3,2,1),las=2,cex.axis=1,labels=c("Supporters  \n vs.        \nOpponents  ","Democrats   \n vs.         \nRepublicans  ","Liberals     \n vs.         \nConservatives"),tck=0)
text(0.1,3.1,"DEC12",cex=.6)
text(0.1,2.1,"DEC12",cex=.6)
text(0.1,1.1,"DEC12",cex=.6)
text(0.1,2.9,"JAN13",cex=.6)
text(0.1,1.9,"JAN13",cex=.6)
text(0.1,.9,"JAN13",cex=.6)
dev.copy(pdf,"PSRM_Figure3.pdf")
dev.off()


##################################
## Appendix Plot for 5-pt scale ##
##################################

par(mar=c(3.5,7.5,1,1), mgp=c(2.5,.25,0),mfrow=c(1,2))

## Cross-sectional
effect <- c(.05,-.05,.04,.15,.02,.03,.04,-.03,.14,-.23,.11,-.02, -.02, .06)
se <- c(.11,.19,.18,.14,.22,.21,.16,.15,.15,.18,.15,.11, .29, .11)
group <- c(14,13,12,11,10,9,8,7,6,5,4,3, 1, 2)
plot(effect,group,xlim=c(-1,1),ylim=c(0.5,14.5),tck=0,cex.main=1,main="Cross-sectional results (Dec 2012)",cex.lab=1,cex.axis=1,pch=20,cex=2,ylab="",yaxt="n",xlab="Difference in support for gun control\n(5-point scale)")
abline(v=0,lty=2,col="DARKGREY")
points(effect,group,pch=20,cex=2)
segments(-.6,group,.6,group,col="LIGHTGREY")
segments(effect-se*1.96,group,effect+1.96*se,group)
par(mgp=c(2,.1,0))
axis(2,at=c(14,7,6,3,5,4,13,11,12,10,9,8, 1, 2),las=2,cex.axis=1,labels=c("All respondents","Women","Men","Non-NRA members","Parents","Non-parents","Democrats","Republicans","Independents","Liberals","Moderates","Conservatives", "Northeast", "Non-Northeast"),tck=0)



## Panel
par(mgp=c(2.5,.25,0))
effect <- c(-.11,-.11,-.21,.03,-.15,-.02,-.2,-.19,-.02,-.01,-.23,-.11, -.13, -.01)
se <- c(.05,.07,.12,.07,.07,.09,.1,.07,.08,.1,.07,.06, .06, .1)
group <- c(14,13,12,11,10,9,8,7,6,5,4,3, 2, 1)
plot(effect,group,xlim=c(-.6,.6),ylim=c(0.5,14.5),tck=0,cex.main=1,main="Panel results (Dec 2012 to Jan 2013)",cex.lab=1,cex.axis=.9,pch=20,cex=2,ylab="",yaxt="n",xlab="Change in support for gun control\n(5-point scale)")
abline(v=0,lty=2,col="DARKGREY")
points(effect,group,pch=20,cex=2)
segments(-.6,group,.6,group,col="LIGHTGREY")
segments(effect-se*1.96,group,effect+1.96*se,group)
par(mgp=c(2,.1,0))
axis(2,at=c(14,7,6,3,5,4,13,11,12,10,9,8, 1, 2),las=2,cex.axis=1,labels=c("All respondents","Women","Men","Non-NRA members","Parents","Non-parents","Democrats","Republicans","Independents","Liberals","Moderates","Conservatives", "Northeast", "Non-Northeast"),tck=0)
dev.copy(pdf,"PSRM_FigureA1.pdf",width=12,height=8)
dev.off()


