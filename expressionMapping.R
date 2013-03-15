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

ReadList<-function(file)
{
	aFile<-scan(file, what = "character", sep = "\n")
	nVar<-length(aFile)
	aList<-vector("list", nVar/2)
	aSequence<-seq(2, nVar, 2)
	for( i in 1:(nVar/2))
	{
		aList[[i]]<-strsplit(aFile[aSequence[i]], split = "\t")[[1]]
	}
	names(aList)<-aFile[seq(1, nVar, 2)]
	return(aList)
}



PlotFly<-function(flyImageData, myNames, myCorrelation, firstColor = "yellow", secondColor = "red",
		ncolors = 50)
{
	ncolors<-as.numeric(ncolors)
	colorsToPlot<-flyImageData$colors
	lineColors<-flyImageData$lines
	aMatch<-match(flyImageData$colorName, myNames)
	if(is.character(myCorrelation) == TRUE)
	{
		myCorrelation<-as.numeric(myCorrelation)
	}
	aStart<-round(min(myCorrelation[aMatch], na.rm = TRUE), 1)
	anEnd<-round(max(myCorrelation[aMatch], na.rm = TRUE), 1)
	newCor<-myCorrelation[aMatch]-min(myCorrelation[aMatch], na.rm = TRUE)
	newCor<-(newCor/max(newCor, na.rm = TRUE))*ncolors
	newCor<-round(newCor)
	newCor[which(newCor == 0)]<-1
	myColors<-colorRampPalette(c(firstColor, secondColor))(ncolors)
	newColors<-myColors[newCor]
	newColors[which(is.na(newColors) == TRUE)]<-"white"
	for(i in 1:length(newColors))
	{
		colorsToPlot[which(colorsToPlot == flyImageData$colorCode[i])]<-newColors[i]
	}
	if(newColors[which(flyImageData$colorName == "Salivary_Gland")] == "white")
	{
		tempo<-flyImageData$colorCode[which(flyImageData$colorName == "Salivary_Gland")]
		lineColors[which(lineColors == tempo)]<-"grey90"
	}
	else
	{
		tempo<-flyImageData$colorCode[which(flyImageData$colorName == "Salivary_Gland")]
		test<-newCor[which(flyImageData$colorName == "Salivary_Gland")] -round(ncolors/5, 1)
		if(test >0)
		{
			lineColors[which(lineColors == tempo)]<-myColors[test]	
		}
		else
		{
			lineColors[which(lineColors == tempo)]<-myColors[1]	
		}
	}
	if(newColors[which(flyImageData$colorName == "Eye")] == "white")
	{
		tempo<-flyImageData$colorCode[which(flyImageData$colorName == "Eye")]
		lineColors[which(lineColors == tempo)]<-"grey90"
	}
	else
	{
		tempo<-flyImageData$colorCode[which(flyImageData$colorName == "Eye")]
		test<-newCor[which(flyImageData$colorName == "Eye")] -round(ncolors/5, 1)
		if(test>0)
		{
			lineColors[which(lineColors == tempo)]<-myColors[test]
		}
		else
		{
			lineColors[which(lineColors == tempo)]<-myColors[1]			
		}
	}
	for(i in 1:length(flyImageData$starts))
	{
		if(i == 1)
		{
			plot(flyImageData$x[1:(flyImageData$starts[i+1]-1)], flyImageData$y[1:(flyImageData$starts[i+1]-1)],type = "n",
					xlim = c(min(flyImageData$x), max(flyImageData$x)), ylim = c(0, max(flyImageData$y)),
					main = "", xlab = c(""), ylab = c(""), axes = FALSE, frame.plot = FALSE)	
			polygon(flyImageData$x[1:(flyImageData$starts[i+1]-1)], flyImageData$y[1:(flyImageData$starts[i+1]-1)], 
					col =colorsToPlot[i], border =  lineColors[i], lwd = flyImageData$lineWidth)
		}
		else if(i == length(flyImageData$starts))
		{
			polygon(flyImageData$x[(flyImageData$starts[i]+1): length(flyImageData$x)],
					flyImageData$y[(flyImageData$starts[i]+1): length(flyImageData$y)], 
					col = colorsToPlot[i], border =  lineColors[i], lwd = flyImageData$lineWidth)	
		}
		else
		{
			polygon(flyImageData$x[(flyImageData$starts[i]+1): (flyImageData$starts[i+1]-1)],
					flyImageData$y[(flyImageData$starts[i]+1): (flyImageData$starts[i+1]-1)], 
					col = colorsToPlot[i], border =  lineColors[i], lwd = flyImageData$lineWidth)
		}	
	}
	text(flyImageData$textX, flyImageData$textY, flyImageData$textNames, pos =flyImageData$textPos, cex = flyImageData$textCex)
	aMat<-matrix(c(seq(1:ncolors), seq(1:ncolors)), nrow = ncolors, ncol =2)
	aMin<-min(flyImageData$x)+(max(flyImageData$x)-min(flyImageData$x))/4
	aMax<-max(flyImageData$x)-(max(flyImageData$x)-min(flyImageData$x))/4
	x<-seq(aMin,aMax, (aMax-aMin)/ncolors)
	y<-c(85,  (max(flyImageData$y)/30)-6)
	image(x, y, aMat, col = myColors, add = TRUE, useRaster = TRUE)
	polygon(c(aMin, aMin, aMax, aMax), c(0,max(flyImageData$y)/24, max(flyImageData$y)/24, 0), border = "black")
	axis(1,c(aMin, aMax), c(aStart,anEnd), lwd.ticks = 1, lwd = 0, line =-1, cex = flyImageData$textCex[1])
}

aList<-ReadList(flyList)
aList$lineWidth<-as.numeric(aList$lineWidth)
aList$x<-as.numeric(aList$x)
aList$y<-as.numeric(aList$y)
aList$starts<-as.numeric(aList$starts)
aList$organs<-as.numeric(aList$organs)
aList$textX<-as.numeric(aList$textX)
aList$textY<-as.numeric(aList$textY)
aList$textPos<-as.numeric(aList$textPos)
aList$textCex<-as.numeric(aList$textCex)

pdf(pdfName, width=11.7, height = 6.2)
par(mar = c(1,1,1,1), mgp = c(3,0.5,0))	
#pdf(pdfName, width=11.7, height = 6.5)#if there is a title
#par(mar = c(1,1,4,1), mgp = c(3,0.5,0))#if there is a title

aMat<-read.table(correlation, header = TRUE)

PlotFly(aList, aMat$Tissue,  aMat$Ratio, firstColor = firstColor, secondColor =  secondColor ,ncolors = nColors)

dev.off()

