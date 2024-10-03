# Make Exceedence Curves for continuous WQ data, several sites on one plot.
siteNumber <- "13310800"
startDate <- "2011-09-01"
endDate <- "2014-10-02"
parameterCd = "00010"
library(GSqwsr)
library(EGRET)
library(smwrGraphs)
UVpCodes<-c("00010","00095","00060")
site1WQUV<-getMultipleUV(siteNumber,startDate,endDate,UVpCodes)
site2WQUV<-getMultipleUV("13310850",startDate,endDate,UVpCodes)
site3WQUV<-getMultipleUV("13311000",startDate,endDate,UVpCodes)
site4WQUV<-getMultipleUV("13311250",startDate,endDate,UVpCodes)
site5WQUV<-getMultipleUV("13311450",startDate,endDate,UVpCodes)
#AA.lo<- setLayout(num.cols=2,num.rows=3,num.graphs=5,shared.x=1,shared.y=1)
#AA.gr<-setGraph(1,AA.lo)
AA.plt2 <- probPlot(site2WQUV$Wtemp, distribution='uniform', FLIP=TRUE, CDF=TRUE, yaxis.log=FALSE)
AA.plt3 <- probPlot(site3WQUV$Wtemp, distribution='uniform', FLIP=TRUE, CDF=TRUE, yaxis.log=FALSE)
AA.plt4 <- probPlot(site4WQUV$Wtemp, distribution='uniform', FLIP=TRUE, CDF=TRUE, yaxis.log=FALSE)
AA.plt5 <- probPlot(site5WQUV$Wtemp, distribution='uniform', FLIP=TRUE, CDF=TRUE, yaxis.log=FALSE)
setPage("sq", device="windows")
AA.plt<-probPlot(site1WQUV$Wtemp, distribution='uniform', FLIP=TRUE, CDF=TRUE, 
         Plot=list(name="Water Temperature", what="lines", color="blue"),
         yaxis.log=FALSE, yaxis.range=c(0,20), ylabels=20*(0:20) / 20,xlabels=100 * (0:10) / 10,
         ytitle="Water temperature, in degrees Celsius", 
         xtitle="Percent of Time Water Temperature is Exceeded", 
         caption="Temperature Exceedence Curves for streams in the Stibnite mining area of Idaho, 2012-14")
addXY(x=AA.plt2$x, y=AA.plt2$y, Plot=list(color="red"))
addXY(x=AA.plt3$x, y=AA.plt3$y, Plot=list(color="green"))
addXY(x=AA.plt4$x, y=AA.plt4$y, Plot=list(color="orange"))
addXY(x=AA.plt5$x, y=AA.plt5$y, Plot=list(color="black"))
AA.plt<-with(refLine(horizontal=c(9,13,19),vertical=NA,coefficients=NA,Plot=list(name="Idaho temperature criteria", what="lines",type="dotted",width="standard",symbol="none",filled=FALSE,size=1.5,color="black"),current=AA.plt,xrange=c(NA,NA),yrange=c(NA,NA),log10=TRUE))

# NOT USED
# addExplanation(AA.plt, where="ur", title="")
# sampledQ$Prob<-interpLine(AA.plt,xfromy=sampledQ$Qcfs,warn=FALSE)

# Now obtain various n-day stats
library(DVstats)
library(hydroTSM)
# SITE 1
WT1<-subset(site1WQUV,select=c(datetime,Wtemp))
WT1$date<-as.Date(WT1$datetime)
WT1$maseven<-as.numeric(ma(WT1$Wtemp,win.len=673,FUN=mean))
# WT1a<-na.omit(WT1)
WTdv1<-readNWISdv("13310800",startDate,endDate,param="00010",stat="mean",convert.type=TRUE)
names(WTdv1)[4] <- ("dmWT")
dmWT.plt1 <- probPlot(WTdv1$dmWT, distribution='uniform', FLIP=TRUE, CDF=TRUE, yaxis.log=FALSE)
sevenD.plt1 <- probPlot(WT1$maseven, distribution='uniform', FLIP=TRUE, CDF=TRUE, yaxis.log=FALSE)
setPage("sq", device="windows")
BB.plt<-probPlot(site1WQUV$Wtemp, distribution='uniform', FLIP=TRUE, CDF=TRUE, 
         Plot=list(name="Water Temperature", what="lines", color="blue"),
         yaxis.log=FALSE, yaxis.range=c(0,20), ylabels=20*(0:20) / 20,xlabels=100 * (0:10) / 10,
         ytitle="Water temperature, in degrees Celsius", 
         xtitle="Percent of Time Water Temperature is Exceeded", 
         caption="Temperature Exceedence Curves for streams in the Stibnite mining area of Idaho, 2012-14")
addXY(x=sevenD.plt1$x, y=sevenD.plt1$y, Plot=list(color="red"))
addXY(x=dmWT.plt1$x, y=dmWT.plt1$y, Plot=list(color="black"))
BB.plt<-with(refLine(horizontal=c(9,13,19),vertical=NA,coefficients=NA,Plot=list(name="Idaho temperature criteria", what="lines",type="dotted",width="standard",symbol="none",filled=FALSE,size=1.5,color="black"),current=BB.plt,xrange=c(NA,NA),yrange=c(NA,NA),log10=TRUE))
approx(x=dmWT.plt1$y,y=dmWT.plt1$x,xout=9,ties=max)
approx(x=dmWT.plt1$y,y=dmWT.plt1$x,xout=19,ties=max)
approx(x=sevenD.plt1$y,y=sevenD.plt1$x,xout=13,ties=max)
approx(x=BB.plt$y,y=BB.plt$x,xout=13,ties=max)
approx(x=BB.plt$y,y=BB.plt$x,xout=22,ties=max)
# SITE 2
WT2<-subset(site2WQUV,select=c(datetime,Wtemp))
WT2$date<-as.Date(WT2$datetime)
WT2$maseven<-as.numeric(ma(WT2$Wtemp,win.len=673,FUN=mean))
# WT2a<-na.omit(WT2)
WTdv2<-readNWISdv("13310850",startDate,endDate,param="00010",stat="mean",convert.type=TRUE)
names(WTdv2)[4] <- ("dmWT")
dmWT2.plt1 <- probPlot(WTdv2$dmWT, distribution='uniform', FLIP=TRUE, CDF=TRUE, yaxis.log=FALSE)
sevenD2.plt1 <- probPlot(WT2$maseven, distribution='uniform', FLIP=TRUE, CDF=TRUE, yaxis.log=FALSE)
setPage("sq", device="windows")
BB2.plt<-probPlot(site2WQUV$Wtemp, distribution='uniform', FLIP=TRUE, CDF=TRUE, 
                 Plot=list(name="Water Temperature", what="lines", color="blue"),
                 yaxis.log=FALSE, yaxis.range=c(0,20), ylabels=20*(0:20) / 20,xlabels=100 * (0:10) / 10,
                 ytitle="Water temperature, in degrees Celsius", 
                 xtitle="Percent of Time Water Temperature is Exceeded", 
                 caption="Temperature Exceedence Curves for streams in the Stibnite mining area of Idaho, 2012-14")
addXY(x=sevenD2.plt1$x, y=sevenD2.plt1$y, Plot=list(color="red"))
addXY(x=dmWT2.plt1$x, y=dmWT2.plt1$y, Plot=list(color="black"))
BB2.plt<-with(refLine(horizontal=c(9,13,19),vertical=NA,coefficients=NA,Plot=list(name="Idaho temperature criteria", what="lines",type="dotted",width="standard",symbol="none",filled=FALSE,size=1.5,color="black"),current=BB2.plt,xrange=c(NA,NA),yrange=c(NA,NA),log10=TRUE))
approx(x=dmWT2.plt1$y,y=dmWT2.plt1$x,xout=9,ties=max)
approx(x=dmWT2.plt1$y,y=dmWT2.plt1$x,xout=19,ties=max)
approx(x=sevenD2.plt1$y,y=sevenD2.plt1$x,xout=13,ties=max)
approx(x=BB2.plt$y,y=BB2.plt$x,xout=13,ties=max)
approx(x=BB2.plt$y,y=BB2.plt$x,xout=22,ties=max)
# SITE 3
WT3<-subset(site3WQUV,select=c(datetime,Wtemp))
WT3$date<-as.Date(WT3$datetime)
WT3$maseven<-as.numeric(ma(WT3$Wtemp,win.len=673,FUN=mean))
# WT3a<-na.omit(WT3)
WTdv3<-readNWISdv("13311000",startDate,endDate,param="00010",stat="mean",convert.type=TRUE)
names(WTdv3)[4] <- ("dmWT")
dmWT3.plt1 <- probPlot(WTdv3$dmWT, distribution='uniform', FLIP=TRUE, CDF=TRUE, yaxis.log=FALSE)
sevenD3.plt1 <- probPlot(WT3$maseven, distribution='uniform', FLIP=TRUE, CDF=TRUE, yaxis.log=FALSE)
setPage("sq", device="windows")
BB3.plt<-probPlot(site3WQUV$Wtemp, distribution='uniform', FLIP=TRUE, CDF=TRUE, 
                  Plot=list(name="Water Temperature", what="lines", color="blue"),
                  yaxis.log=FALSE, yaxis.range=c(0,20), ylabels=20*(0:20) / 20,xlabels=100 * (0:10) / 10,
                  ytitle="Water temperature, in degrees Celsius", 
                  xtitle="Percent of Time Water Temperature is Exceeded", 
                  caption="Temperature Exceedence Curves for streams in the Stibnite mining area of Idaho, 2012-14")
addXY(x=sevenD3.plt1$x, y=sevenD3.plt1$y, Plot=list(color="red"))
addXY(x=dmWT3.plt1$x, y=dmWT3.plt1$y, Plot=list(color="black"))
BB3.plt<-with(refLine(horizontal=c(9,13,19),vertical=NA,coefficients=NA,Plot=list(name="Idaho temperature criteria", what="lines",type="dotted",width="standard",symbol="none",filled=FALSE,size=1.5,color="black"),current=BB3.plt,xrange=c(NA,NA),yrange=c(NA,NA),log10=TRUE))
approx(x=dmWT3.plt1$y,y=dmWT3.plt1$x,xout=9,ties=max)
approx(x=dmWT3.plt1$y,y=dmWT3.plt1$x,xout=19,ties=max)
approx(x=sevenD3.plt1$y,y=sevenD3.plt1$x,xout=13,ties=max)
approx(x=BB3.plt$y,y=BB3.plt$x,xout=13,ties=max)
approx(x=BB3.plt$y,y=BB3.plt$x,xout=22,ties=max)
# SITE 4
WT4<-subset(site4WQUV,select=c(datetime,Wtemp))
WT4$date<-as.Date(WT4$datetime)
WT4$maseven<-as.numeric(ma(WT4$Wtemp,win.len=673,FUN=mean))
# WT4a<-na.omit(WT4)
WTdv4<-readNWISdv("13311250",startDate,endDate,param="00010",stat="mean",convert.type=TRUE)
names(WTdv4)[4] <- ("dmWT")
dmWT4.plt1 <- probPlot(WTdv4$dmWT, distribution='uniform', FLIP=TRUE, CDF=TRUE, yaxis.log=FALSE)
sevenD4.plt1 <- probPlot(WT4$maseven, distribution='uniform', FLIP=TRUE, CDF=TRUE, yaxis.log=FALSE)
setPage("sq", device="windows")
BB4.plt<-probPlot(site4WQUV$Wtemp, distribution='uniform', FLIP=TRUE, CDF=TRUE, 
                  Plot=list(name="Water Temperature", what="lines", color="blue"),
                  yaxis.log=FALSE, yaxis.range=c(0,20), ylabels=20*(0:20) / 20,xlabels=100 * (0:10) / 10,
                  ytitle="Water temperature, in degrees Celsius", 
                  xtitle="Percent of Time Water Temperature is Exceeded", 
                  caption="Temperature Exceedence Curves for streams in the Stibnite mining area of Idaho, 2012-14")
addXY(x=sevenD4.plt1$x, y=sevenD4.plt1$y, Plot=list(color="red"))
addXY(x=dmWT4.plt1$x, y=dmWT4.plt1$y, Plot=list(color="black"))
BB4.plt<-with(refLine(horizontal=c(9,13,19),vertical=NA,coefficients=NA,Plot=list(name="Idaho temperature criteria", what="lines",type="dotted",width="standard",symbol="none",filled=FALSE,size=1.5,color="black"),current=BB4.plt,xrange=c(NA,NA),yrange=c(NA,NA),log10=TRUE))
approx(x=dmWT4.plt1$y,y=dmWT4.plt1$x,xout=9,ties=max)
approx(x=dmWT4.plt1$y,y=dmWT4.plt1$x,xout=19,ties=max)
approx(x=sevenD4.plt1$y,y=sevenD4.plt1$x,xout=13,ties=max)
approx(x=BB4.plt$y,y=BB4.plt$x,xout=13,ties=max)
approx(x=BB4.plt$y,y=BB4.plt$x,xout=22,ties=max)
# SITE 5
WT5<-subset(site5WQUV,select=c(datetime,Wtemp))
WT5$date<-as.Date(WT5$datetime)
WT5$maseven<-as.numeric(ma(WT5$Wtemp,win.len=673,FUN=mean))
# WT5a<-na.omit(WT5)
WTdv5<-readNWISdv("13311250",startDate,endDate,param="00010",stat="mean",convert.type=TRUE)
names(WTdv5)[4] <- ("dmWT")
dmWT5.plt1 <- probPlot(WTdv5$dmWT, distribution='uniform', FLIP=TRUE, CDF=TRUE, yaxis.log=FALSE)
sevenD5.plt1 <- probPlot(WT5$maseven, distribution='uniform', FLIP=TRUE, CDF=TRUE, yaxis.log=FALSE)
setPage("sq", device="windows")
BB5.plt<-probPlot(site5WQUV$Wtemp, distribution='uniform', FLIP=TRUE, CDF=TRUE, 
                  Plot=list(name="Water Temperature", what="lines", color="blue"),
                  yaxis.log=FALSE, yaxis.range=c(0,20), ylabels=20*(0:20) / 20,xlabels=100 * (0:10) / 10,
                  ytitle="Water temperature, in degrees Celsius", 
                  xtitle="Percent of Time Water Temperature is Exceeded", 
                  caption="Temperature Exceedence Curves for streams in the Stibnite mining area of Idaho, 2012-14")
addXY(x=sevenD5.plt1$x, y=sevenD5.plt1$y, Plot=list(color="red"))
addXY(x=dmWT5.plt1$x, y=dmWT5.plt1$y, Plot=list(color="black"))
BB5.plt<-with(refLine(horizontal=c(9,13,19),vertical=NA,coefficients=NA,Plot=list(name="Idaho temperature criteria", what="lines",type="dotted",width="standard",symbol="none",filled=FALSE,size=1.5,color="black"),current=BB5.plt,xrange=c(NA,NA),yrange=c(NA,NA),log10=TRUE))
approx(x=dmWT5.plt1$y,y=dmWT5.plt1$x,xout=9,ties=max)
approx(x=dmWT5.plt1$y,y=dmWT5.plt1$x,xout=19,ties=max)
approx(x=sevenD5.plt1$y,y=sevenD5.plt1$x,xout=13,ties=max)
approx(x=BB5.plt$y,y=BB5.plt$x,xout=13,ties=max)
approx(x=BB5.plt$y,y=BB5.plt$x,xout=22,ties=max)
