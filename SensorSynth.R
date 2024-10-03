library(dataRetrieval)
library(EGRET)
library(smwrBase)
library(smwrQW)
library(readr)
siteNumber <- "11337190"
startDate <- "2016-07-20"
endDate <- "2021-02-16"
#import from Aquarius download after modifying header, unpub or >3yr old, not AAA'ed 
# default tz is UTC-8, cut header, rename date, dateTime first to col, format dateTime to swedish/sweden YYYY-MM-DD HH:MM 
#### date col imports in UTC, dateTime imports in UTC-8 (PST) ###
uvfCHL <- read_csv("fCHL.csv")
uvfCHL<-subset(uvfCHL, select  = c(date,fCHL))
uvSal<-read_csv("Salinityppt.csv")
uvSal<-subset(uvSal, select  = c(date,Salppt))
#uvfDOMcorr<-subset(uvfDOMcorr, select  = c(date,Salppt))
uvfDOMcorr<-read_csv("fDOMCorr.csv")
uvfDOMcorr<-subset(uvfDOMcorr, select  = c(date,fDOMcorr))
uvfDOMwk<-read_csv("fDOMwork.csv")
uvfDOMwk<-subset(uvfDOMwk, select  = c(date,fDOMwork))
uvfDOMwt<-read_csv("fDOMwt.csv")
uvfDOMwt<-subset(uvfDOMwt, select  = c(date,fDOMwt))
uvfDOMwtTurb<-read_csv("fDOMwtTurb.csv")
uvfDOMwtTurb<-subset(uvfDOMwtTurb, select  = c(date,fDOMwtTurb))
#UV retrieval - Use when in CRP; <3yr since AAA 
parameterCd<-c("00010","00095","63680")#"00480", (wasn't working, was just approved)
#WT degC, SC uS/cm, salinity in ppt, turb FNU
uvWQexpvar <- readNWISuv(siteNumber,parameterCd,startDate,endDate,tz="UTC")
uvWQexpvar<-subset(uvWQexpvar, select=c(dateTime,X_LEFT.BANK.BGC.PROJECT_00010_00000,X_LEFT.BANK.BGC.PROJECT_00095_00000,X_LEFT.BANK.BGC.PROJECT_63680_00000))#X_00480_00000,
names(uvWQexpvar)[2]<-"WT"
names(uvWQexpvar)[3]<-"SC"
#names(uvWQexpvar)[4]<-"Salppt"
names(uvWQexpvar)[4]<-"turb"
#merge all TS data
uv<-merge(uvWQexpvar,uvfCHL, by.x="dateTime",by.y="date", all.x = TRUE)
uv<-merge(uv,uvfDOMcorr, by.x="dateTime",by.y="date", all.x = TRUE)
uv<-merge(uv,uvfDOMwk, by.x="dateTime",by.y="date", all.x = TRUE)
uv<-merge(uv,uvfDOMwt, by.x="dateTime",by.y="date", all.x = TRUE)
uv<-merge(uv,uvfDOMwtTurb, by.x="dateTime",by.y="date", all.x = TRUE)
uv<-merge(uv,uvSal, by.x="dateTime",by.y="date", all.x = TRUE)
#Read in discrete NWIS sample data
parameterCd<-c("62360","70953","00631","00608","00613","00671","62854","52905","52906","52907")
# 62360 = pheophytin ug/L, 70953=chlA ug/L, 00631 = nitrate+nitrite as N, 00608=ammonia, 00613=nitrite as N, 
#00671=orthoP as P, 62854=TDN, 52905=phycocyanins ug/L, 52906=allophycocyanin, 52907=phycoerythrin
Sample<- readNWISqw(siteNumber, parameterCd, startDate,endDate,expanded=TRUE,reshape=TRUE,tz="UTC")
Sample1<-subset(Sample,select=c(startDateTime,
                               result_va_00608, remark_cd_00608, rpt_lev_va_00608, 
                               result_va_00613, remark_cd_00613, rpt_lev_va_00613,
                               result_va_00631, remark_cd_00631, rpt_lev_va_00631,
                               result_va_00671, remark_cd_00671, rpt_lev_va_00671,
                               result_va_62360, remark_cd_62360, rpt_lev_va_62360,
                               result_va_62854, remark_cd_62854, rpt_lev_va_62854,
                               result_va_70953, remark_cd_70953, rpt_lev_va_70953))
names(Sample1)[2]<-"ammonia"
names(Sample1)[5]<-"nitrite"
names(Sample1)[8]<-"nitrateNitriteN"
names(Sample1)[11]<-"OP"
names(Sample1)[14]<-"pheophytin"
names(Sample1)[17]<-"TDN"
names(Sample1)[20]<-"chlA"
Sample1$ammoniaLC<-as.lcens(Sample1$ammonia,Sample1$rpt_lev_va_00608,Sample1$remark_cd_00608)
Sample1$nitriteLC<-as.lcens(Sample1$nitrite,Sample1$rpt_lev_va_00613,Sample1$remark_cd_00613)
Sample1$nitrateNitriteNLC<-as.lcens(Sample1$nitrateNitriteN,Sample1$rpt_lev_va_00631,Sample1$remark_cd_00631)
Sample1$OPLC<-as.lcens(Sample1$OP,Sample1$rpt_lev_va_00671,Sample1$remark_cd_00671)
Sample1$pheophytinLC<-as.lcens(Sample1$pheophytin,Sample1$rpt_lev_va_62360,Sample1$remark_cd_62360)
Sample1$TDNLC<-as.lcens(Sample1$TDN,Sample1$rpt_lev_va_62854,Sample1$remark_cd_62854)
Sample1$chlALC<-as.lcens(Sample1$chlA,Sample1$rpt_lev_va_70953,Sample1$remark_cd_70953)
OMRLfdom<-read_csv("JPTfdomLab.csv") #edited from access DB export 4-9-21 from ahansen fdom in raman units
Sample2<-mergeNearest(Sample1,dates.left="startDateTime",all.left = TRUE,suffix.left = "lf",
                      OMRLfdom,dates.right = "dateTimeUTC",suffix.right = "rt",Date.noon = FALSE, max.diff = "0 hours")
Sample2<-subset(Sample2,select=-dateTimePST)
Sample2<-subset(Sample2,select=-dateTimeUTC)
Sample2<-subset(Sample2,select=-NWISSiteID)
Sample2<-subset(Sample2,select=-rpt_lev_va_00608)
Sample2<-subset(Sample2,select=-remark_cd_00608)
Sample2<-subset(Sample2,select=-rpt_lev_va_00613)
Sample2<-subset(Sample2,select=-remark_cd_00613)
Sample2<-subset(Sample2,select=-rpt_lev_va_00631)
Sample2<-subset(Sample2,select=-remark_cd_00631)
Sample2<-subset(Sample2,select=-rpt_lev_va_00671)
Sample2<-subset(Sample2,select=-remark_cd_00671)
Sample2<-subset(Sample2,select=-rpt_lev_va_62360)
Sample2<-subset(Sample2,select=-remark_cd_62360)
Sample2<-subset(Sample2,select=-rpt_lev_va_62854)
Sample2<-subset(Sample2,select=-remark_cd_62854)
Sample2<-subset(Sample2,select=-rpt_lev_va_70953)
Sample2<-subset(Sample2,select=-remark_cd_70953)
names(Sample2)[1]<-"dateTime"
calSet1<-merge(Sample2,uv,all.x = TRUE)
exportCSV(calSet1,"calSet1.csv")
### removed NAs manually for completeness and did some quick excel graphing for expvar suspects
fDOMcal<-read.csv("fDOMcal.csv")
fDOMcal$dateTime<-as.POSIXct(fDOMcal$dateTime,tz="UTC")
A254<-read_csv("A254.csv")
fDOMcal<-merge(fDOMcal,A254, all.x = TRUE)

fDOMwkreg<-with (fDOMcal,allReg(cbind(turb,fDOMwork,WT,Salppt,nitrateNitriteN),fdom370em460,lin.dep=5))
fDOMwkreg

fDOMwtreg<-with (fDOMcal,allReg(cbind(turb,fDOMwt,Salppt,nitrateNitriteN),fdom370em460,lin.dep=5))
fDOMwtreg

fDOMwwtreg<-with (fDOMcal,allReg(cbind(turb,fdom370em460,Salppt,nitrateNitriteN),fDOMwt,lin.dep=5))
fDOMwwtreg

fDOMwtTurbreg<-with (fDOMcal,allReg(cbind(fDOMwtTurb,Salppt,nitrateNitriteN),fdom370em460,lin.dep=5))
fDOMwtTurbreg

fDOMcorrreg<-with (fDOMcal,allReg(cbind(fDOMcorr,Salppt,nitrateNitriteN),fdom370em460,lin.dep=5))
fDOMcorrreg

fDOMwtTurb.lm<-lm(fdom370em460~fDOMwtTurb+Salppt,data=fDOMcal)
fDOMwtTurb.reg<-multReg(fDOMwtTurb.lm)

fDOMcorr.lm<-lm(fdom370em460~fDOMcorr,data=fDOMcal)
fDOMcor.reg<-multReg(fDOMcorr.lm)
fDOMcor.reg

#### Diagnostics
print(fDOMwtTurb.reg)
plot(fDOMwtTurb.reg,which=1)
plot(fDOMwtTurb.reg,which=2)
plot(fDOMwtTurb.reg,which=3)
plot(fDOMwtTurb.reg,which=4)
plot(fDOMwtTurb.reg,which=5)
plot(fDOMwtTurb.reg,which=6)
plot(fDOMwtTurb.reg,which="PERIM")
#####

fDOMwt.lm<-lm(fdom370em460~fDOMwt+turb+Salppt,data=fDOMcal)
fDOMwt.reg<-multReg(fDOMwt.lm)

#### Diagnostics
print(fDOMwt.reg)
plot(fDOMwt.reg,which=1)
plot(fDOMwt.reg,which=2)
plot(fDOMwt.reg,which=3)
plot(fDOMwt.reg,which=4)
plot(fDOMwt.reg,which=5)
plot(fDOMwt.reg,which=6)
plot(fDOMwt.reg,which="PERIM")
#####

fDOMwtTurb.lm<-lm(fdom370em460~fDOMwtTurb+Salppt,data=fDOMcal)
fDOMwtTurb.reg<-multReg(fDOMwt.lm)

#### Diagnostics
print(fDOMwt.reg)
plot(fDOMwt.reg,which=1)
plot(fDOMwt.reg,which=2)
plot(fDOMwt.reg,which=3)
plot(fDOMwt.reg,which=4)
plot(fDOMwt.reg,which=5)
plot(fDOMwt.reg,which=6)
plot(fDOMwt.reg,which="PERIM")

Nitratereg<-with (calSet1,allReg(cbind(fDOMcorr,Salppt,turb, A254,fourier(dateTime)),nitrateNitriteN,lin.dep=5))
Nitratereg
