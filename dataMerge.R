library(EGRET)
library(dataRetrieval)
library(remotes)
library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
remotes::install_gitlab("water/analysis-tools/smwrData",
                        host = "code.usgs.gov", force=TRUE)
remotes::install_gitlab("water/analysis-tools/smwrBase",
                        host = "code.usgs.gov", force=TRUE)
remotes::install_gitlab("water/analysis-tools/smwrGraphs",
                        host = "code.usgs.gov", force=TRUE)
library(smwrBase)
library(smwrGraphs)
library(readr)
fCHL_LO <- read_csv("LOswego.csv")
chlA_LO <-read_csv("LOswegoSamples.csv")
#chlA_LO$date<- as_date(chlA_LO$dateTime)
LOcal<-mergeNearest(chlA_LO,dates.left="dateTime",all.left = FALSE,suffix.left = "lf",
                           fCHL_LO,dates.right = "dateTime",suffix.right = "rt",Date.noon = FALSE, max.diff = "10 mins")

#remove NAs
LOcal2<-na.omit(LOcal, cols="fCHL")
exportCSV(LOcal,file.name="LOcal.csv")

fCHL.plt1<-probPlot(fCHL_LO$fCHL, distribution='uniform', FLIP=TRUE, CDF=TRUE, 
                      Plot=list(name="Chlorophyll fluorescence, in ug/L", what="lines", color="black"), 
                      yaxis.log=TRUE, xlabels=100 * (0:10)/10)
LOcal$ProbfCHL<-interpLine(fCHL.plt,xfromy=LOcal$fCHL,warn=FALSE)
setPage("sq", device="windows")
fCHL.plt<-probPlot(fCHL_LO$fCHL, distribution='uniform', FLIP=TRUE, CDF=TRUE,
                     Plot=list(what="none"),
                     yaxis.log=TRUE, ytitle="Chlorophyll fluorescence, in ug/L", xlabels=100 * (0:10)/10,
                     xtitle="Percent of Time in situ Chlorophyll fluorescence Value is Exceeded", 
                     caption="Chlorophyll Fluorescence Exceedence Curve for Lake Oswego, OR, POR")
fCHL.plt<-addGrid(fCHL.plt,Ygrid=list(grid="gray75",finegrid="none"))
addXY(x=fCHL.plt1$x, y=fCHL.plt1$y, Plot=list(color="black", what = "lines"))
#### Obtain N values for legend - in this example, interested in ####
#### how many samples are paired with TurbEXO and TurbEXObgc readings ####
colSums(!is.na(calSet)) #note them now for updating labels on the plot
fCHL.plt<-with(LOcal,addXY(ProbfCHL,fCHL,
                              Plot=list(name="Sampled Chlorophyll Fluorescence N=134",what="points", color="black"), 
                              current=fCHL.plt))
addExplanation(fCHL.plt,"ur")
LOchl.lm<-lm(ResultMeasureValue~fCHL, data=LOcal)
summary(LOchl.lm)
