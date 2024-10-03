#### Make Exceedence Curves for continuous WQ data, several sites on one plot.####
#### Four hashtags indicate instructions for this script example
# One hashtag is ancillary information for other scripts that could be developed 
# from this one for other WQ constituents
#### Load package libraries ####
library(EGRET)
library(smwrGraphs)
library(hydroTSM) 
library(dataRetrieval)
#### Set up data retreival inputs and change as needed for other sites ####
siteNumber <- "11337190" #update as needed
startDate <- "" #updatea s needed, blank = beginning of POR (assuming CRP)
endDate <- "2020-09-30" #update as needed, blank = end of POR
parameterCd <- "63680" #for TS to which samples are typically related in models
#### Obtain the time series data of interest from NWIS-TS through webservices API ####
siteTurbTS<-readNWISuv(siteNumber,parameterCd,startDate,endDate,tz="UTC")
#### cleaning up column names to intelligible information ####
names(siteTurbTS)[4]<-"Turb6series"
names(siteTurbTS)[5]<-"Turb6series_cd"
names(siteTurbTS)[6]<-"TurbEXO"
names(siteTurbTS)[7]<-"TurbEXO_cd"
names(siteTurbTS)[8]<-"TurbEXObgc"
names(siteTurbTS)[9]<-"TurbEXObgc_cd"
#### If using time series in a report or analysis for publication, check *_cd = A for approved ####
#### Now retrieve sample results of interest from NWIS / QWDATA webservices API ####
parameterCd<-"80154"
Sample<- readNWISqw(siteNumber, parameterCd, startDate,endDate,expanded=TRUE,reshape=TRUE,tz="UTC")
# Example of pulling multiple constituents
# parameterCd<-c("62360","70953","00631","00608","00613","00671","62854","52905","52906","52907")
# Reference list of other WQ constituents of comparison interest to TS sensor data
# 80154 = SSC, 62360 = pheophytin ug/L, 70953=chlA ug/L, 00631 = nitrate+nitrite as N, 00608=ammonia, 
# 00613=nitrite as N, 00671=orthoP as P, 62854=TDN
# 52905=phycocyanins ug/L, 52906=allophycocyanin, 52907=phycoerythrin
# note: accessory pigments of pCODE 529* are not available through webservices at this time - this is a QWDATA problem
# for more information on why specific WQ constituents are not interoperable with R via NWIS/QWDATA contact Dave Dupre
# Code useful when retrieving multiple constituents, or any with censored results
# Sample1<-subset(Sample,select=c(startDateTime,
#                                result_va_00608, remark_cd_00608, rpt_lev_va_00608, 
#                                result_va_00613, remark_cd_00613, rpt_lev_va_00613,
#                                result_va_00631, remark_cd_00631, rpt_lev_va_00631,
#                                result_va_00671, remark_cd_00671, rpt_lev_va_00671,
#                                result_va_62360, remark_cd_62360, rpt_lev_va_62360,
#                                result_va_62854, remark_cd_62854, rpt_lev_va_62854,
#                                result_va_70953, remark_cd_70953, rpt_lev_va_70953))
# names(Sample1)[2]<-"ammonia"
# names(Sample1)[5]<-"nitrite"
# names(Sample1)[8]<-"nitrateNitriteN"
# names(Sample1)[11]<-"OP"
# names(Sample1)[14]<-"pheophytin"
# names(Sample1)[17]<-"TDN"
# names(Sample1)[20]<-"chlA"
# lcens class will combine remark code and value for censored data and provide 
# values that can be used in stats even when censored
# Sample1$ammoniaLC<-as.lcens(Sample1$ammonia,Sample1$rpt_lev_va_00608,Sample1$remark_cd_00608)
# Sample1$nitriteLC<-as.lcens(Sample1$nitrite,Sample1$rpt_lev_va_00613,Sample1$remark_cd_00613)
# Sample1$nitrateNitriteNLC<-as.lcens(Sample1$nitrateNitriteN,Sample1$rpt_lev_va_00631,Sample1$remark_cd_00631)
# Sample1$OPLC<-as.lcens(Sample1$OP,Sample1$rpt_lev_va_00671,Sample1$remark_cd_00671)
# Sample1$pheophytinLC<-as.lcens(Sample1$pheophytin,Sample1$rpt_lev_va_62360,Sample1$remark_cd_62360)
# Sample1$TDNLC<-as.lcens(Sample1$TDN,Sample1$rpt_lev_va_62854,Sample1$remark_cd_62854)
# Sample1$chlALC<-as.lcens(Sample1$chlA,Sample1$rpt_lev_va_70953,Sample1$remark_cd_70953)
#### Subset to data of interest ####
Sample1<-subset(Sample, select=c(startDateTime, result_va_80154, remark_cd_80154, dqi_cd_80154))
#### If using samples to approve data, or run models, ensure dqi = R = approved #### 
names(Sample1)[2]<-"SSC"
calSet<-mergeNearest(Sample1,dates.left="startDateTime",all.left = TRUE,suffix.left = "lf",
                      siteTurbTS,dates.right = "dateTime",suffix.right = "rt",Date.noon = FALSE, max.diff = "0 hours")

#### above data in calSet can be used in regression analysis also #### 
#### Create the plot for distribution of samples relative to time series data of interest ####
# this example combines two time periods of EXO turbidity data under operation from 
# two different groups and set up in two separate time series "c(x1,x2)," 
# would change to "x1," if only one ts was of interest
EXOts.plt2<-probPlot(c(siteTurbTS$TurbEXO,siteTurbTS$TurbEXObgc), distribution='uniform', FLIP=TRUE, CDF=TRUE, 
                     Plot=list(name="EXO Turbidity in FNU", what="lines", color="black"), 
                     yaxis.log=TRUE, xlabels=100 * (0:10)/10)
calSet$ProbTurbEXO<-interpLine(EXOts.plt2,xfromy=calSet$TurbEXO,warn=FALSE)
calSet$ProbTurbEXObgc<-interpLine(EXOts.plt2,xfromy=calSet$TurbEXObgc,warn=FALSE)
setPage("sq", device="windows")
EXOts.plt<-probPlot(c(siteTurbTS$TurbEXO,siteTurbTS$TurbEXObgc), distribution='uniform', FLIP=TRUE, CDF=TRUE,
                    Plot=list(what="none"),
                    yaxis.log=TRUE, ytitle="Turbidity, in FNU", xlabels=100 * (0:10)/10,
                    xtitle="Percent of Time in situ Turbidity Value is Exceeded", 
                    caption="Turbidity Exceedence Curve for San Joaquin R at Jersey Point, CA, POR")
EEXOts.plt<-addGrid(EXOts.plt,Ygrid=list(grid="gray75",finegrid="none"))
addXY(x=EXOts.plt2$x, y=EXOts.plt2$y, Plot=list(color="black", what = "lines"))
#### Obtain N values for legend - in this example, interested in ####
#### how many samples are paired with TurbEXO and TurbEXObgc readings ####
colSums(!is.na(calSet)) #note them now for updating labels on the plot
EXOts.plt<-with(calSet,addXY(ProbTurbEXO,TurbEXO,
                             Plot=list(name="Sampled EXO Turbidity N=13",what="points", color="black"), 
                             current=EXOts.plt))
EXOts.plt<-with(calSet,addXY(ProbTurbEXObgc,TurbEXObgc,
                             Plot=list(name="Sampled BGC EXO Turbidity N=10",what="points", color="blue"), 
                             current=EXOts.plt))

addExplanation(EXOts.plt,"ur")
