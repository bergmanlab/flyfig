#!/usr/bin/Rscript --vanilla --slave
# chmod u+x expressionMapping.R
# 2:16:25 PM
# TODO: Add comment 
# Author: raquel
###############################################################################

#R --no-save <  expressionMapping.R Flylist correlationMap.txt test.pdf yellow red 100

Args <- commandArgs();
print(Args)
flyList<-Args[[3]] 
correlation<-Args[[4]]
pdfName<-Args[[5]]
firstColor<-Args[[6]]
secondColor<-Args[[7]]
nColors<-Args[[8]]


source("functions.R")

aList<-ReadList(flyList)
aMat<-read.table(correlation, header = TRUE)


#an example of one image per page/file
pdf(pdfName, width=11.7, height = 6.2)
par(mar = c(1,1,1,1), mgp = c(3,0.5,0))	
#pdf(pdfName, width=11.7, height = 6.5)#if there is a title
#par(mar = c(1,1,4,1), mgp = c(3,0.5,0))#if there is a title
PlotFly(aList, aMat$Tissue,  aMat$Ratio, firstColor = firstColor, secondColor =  secondColor ,ncolors = nColors, textCex = 0.8)
dev.off()

#In here it is important to change the size of the text since the figure is much smaller (textCex)
#as long has the proportion of the images are maintaned then the text positions should be maintained 
#this is an example of multirow data 
pdf(paste(pdfName,"_multirow.pdf",sep = ""), height=11, width = 6.2)
par(mfrow = c(3,1))
par(mar = c(1,1,2,1), mgp = c(3,0.5,0))	

PlotFly(aList, aMat$Tissue,  aMat$Ratio, firstColor = firstColor, secondColor =  secondColor ,ncolors = nColors, textCex = 0.6)
PlotFly(aList, aMat$Tissue,  aMat$Ratio, firstColor = "blue", secondColor =  "green" ,ncolors = nColors, textCex = 0.6)
PlotFly(aList, aMat$Tissue,  aMat$Ratio, firstColor = "white", secondColor =  "black" ,ncolors = nColors, textCex = 0.6)

dev.off()