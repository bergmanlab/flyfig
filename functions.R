#!/usr/bin/Rscript --vanilla --slave
# chmod u+x functions.R
# 3:52:47 PM
# TODO: Add comment 
# Author: raquel
###############################################################################



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
	aList$lineWidth<-as.numeric(aList$lineWidth)
	aList$x<-as.numeric(aList$x)
	aList$y<-as.numeric(aList$y)
	aList$starts<-as.numeric(aList$starts)
	aList$organs<-as.numeric(aList$organs)
	aList$textX<-as.numeric(aList$textX)
	aList$textY<-as.numeric(aList$textY)
	aList$textPos<-as.numeric(aList$textPos)
	aList$textCex<-as.numeric(aList$textCex)
	return(aList)
}


PlotFly<-function(flyImageData, myNames, myCorrelation, firstColor = "yellow", secondColor = "red", ncolors = 50,textCex, tickStyle=TRUE, maxValue, minValue)
{
	ncolors<-as.numeric(ncolors)
	colorsToPlot<-flyImageData$colors
	lineColors<-flyImageData$lines
	aMatch<-match(flyImageData$colorName, myNames)
	if(is.character(myCorrelation) == TRUE)
	{
		myCorrelation<-as.numeric(myCorrelation)
	}
	if(missing(maxValue))
	{
		aStart<-round(min(myCorrelation[aMatch], na.rm = TRUE), 1)
		anEnd<-round(max(myCorrelation[aMatch], na.rm = TRUE), 1)
		newCor<-myCorrelation[aMatch]-min(myCorrelation[aMatch], na.rm = TRUE)
		newCor<-(newCor/max(newCor, na.rm = TRUE))*ncolors
		newCor<-round(newCor)
		newCor[which(newCor == 0)]<-1
	}
	else
	{
		if (missing(minValue))
		{
			return("needs a max and min values")
		}
		aStart<-minValue
		anEnd<-maxValue
		newCor<-myCorrelation[aMatch]-minValue
		newCor<-(newCor/(maxValue-minValue))*ncolors
		newCor<-round(newCor)
		newCor[which(newCor == 0)]<-1
	}
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
	if(missing(textCex))
	{
		text(flyImageData$textX, flyImageData$textY, flyImageData$textNames, pos =flyImageData$textPos, cex = flyImageData$textCex)
	}
	else
	{	
		text(flyImageData$textX, flyImageData$textY, flyImageData$textNames, pos =flyImageData$textPos, cex =textCex)
	}
	aMat<-matrix(c(seq(1:100), seq(1:100)), nrow = 100, ncol =2)
	aMin<-min(flyImageData$x)+(max(flyImageData$x)-min(flyImageData$x))/4
	aMax<-max(flyImageData$x)-(max(flyImageData$x)-min(flyImageData$x))/4
	x<-seq(aMin,aMax, (aMax-aMin)/100)
	y<-c(85,  (max(flyImageData$y)/30)-6)
	image(x, y, aMat, col = myColors, add = TRUE, useRaster = TRUE)
	polygon(c(aMin, aMin, aMax, aMax), c(0,max(flyImageData$y)/24, max(flyImageData$y)/24, 0), border = "black")
	if(missing(textCex))
	{
		axis(1,c(aMin, aMax), c(aStart,anEnd), lwd.ticks = 1, lwd = 0, line =-1, cex = flyImageData$textCex[1], tick = tickStyle)
	}
	else
	{
		axis(1,c(aMin, aMax), c(aStart,anEnd), lwd.ticks = 1, lwd = 0, line =-1, cex = textCex, tick = tickStyle)	
	}
}

