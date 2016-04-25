#' @export
READ_CAMx_3DEMIS <- function(input,timezone="GMT", addutm = FALSE, temp="TEMPXX25V367X1.dat") {

system(paste("rm -f",temp,sep=" "),intern = FALSE)

HEAD <- .Fortran("READ_HEAD_PT",ifile=input,NSTK=as.integer(1),MAXS=as.integer(1),
         plon=as.single(1),plat=as.single(1),tlat1=as.single(1),tlat2=as.single(1),XORG=as.single(1),YORG=as.single(1),DX=as.single(1),DY=as.single(1),IDATA=as.integer(1),BEGTIME=as.single(1),
         ENDTIME=as.single(1),ifile2=temp)

META_SPEC = read.csv(temp, header = FALSE,stringsAsFactors=FALSE)
META_SPEC = as.data.frame(META_SPEC)
names(META_SPEC) = c("SPECIESN","MSPEC")
system(paste("rm -f",temp,sep=" "),intern = FALSE)


NBIG1 = HEAD$NSTK*HEAD$MAXS*24
NBIG2 = HEAD$NSTK*24
NBIG3 = HEAD$NSTK

OUT <- .Fortran("READ_CAMx_PT",ifile=input,NBIG1=as.integer(NBIG1),NBIG2=as.integer(NBIG2),NBIG3=as.integer(NBIG3),
         XSTK=single(NBIG3),YSTK=single(NBIG3),HSTK=single(NBIG3),DSTK=single(NBIG3),TSTK=single(NBIG3),VSTK=single(NBIG3),
         KCELL=integer(NBIG2),FLOW=single(NBIG2),PLMHT=single(NBIG2),HRS2=integer(NBIG2),STK2=integer(NBIG2),
         CONC=single(NBIG1),STK1=integer(NBIG1),HRS1=integer(NBIG1),SPECIES1=integer(NBIG1))
#fix here
OUT$SPECIES=as.integer(OUT$SPECIES)
OUT$ifile <- NULL
OUT$NBIG1 <- NULL
OUT$NBIG2 <- NULL
OUT$NBIG3 <- NULL

DAT1 = data.frame(OUT$STK1,OUT$HRS1,OUT$SPECIES1,OUT$CONC, stringsAsFactors=FALSE)
names(DAT1)=c("ISTK","HRS","SPECIESN","CONC")

DAT2 = data.frame(OUT$STK2,OUT$HRS2,OUT$KCELL,OUT$FLOW,OUT$PLMHT, stringsAsFactors=FALSE)
names(DAT2)=c("ISTK","HRS","KCELL","FLOW","PLMHT")

ISTK = as.integer(1:HEAD$NSTK)
DAT3 = data.frame(ISTK,OUT$XSTK,OUT$YSTK,OUT$HSTK,OUT$DSTK,OUT$TSTK,OUT$VSTK)
names(DAT3)=c("ISTK","XSTK","YSTK","HSTK","DSTK","TSTK","VSTK")

META_DAT = left_join(DAT1,DAT2, by = c("ISTK","HRS"))
META_DAT = left_join(META_DAT,DAT3,by = "ISTK")
META_DAT$ISTK <- NULL

HRS = META_DAT$HRS + HEAD$BEGTIME
HRS = as.character(HRS)
HRS[OUT$HR<10]= paste("0",HRS[OUT$HR<10], sep = "")

IDAY = HEAD$IDATA%%1000.
IYR = (HEAD$IDATA - IDAY)/1000.
CDAY = as.character(IDAY)
CYR = as.character(IYR)
if (IYR < 10) {
CYR =  paste("0",CYR, sep = "")
}
if (IDAY < 10) {
CDAY =  paste("0",CDAY, sep = "")
}
if (IDAY < 100) {
CDAY =  paste("0",CDAY, sep = "")
}

DATETIME = paste(CYR,CDAY,HRS, sep = " ")
DATETIME = as.POSIXct(DATETIME, format = "%y %j %H", tz = timezone)
META_DAT$HRS <- NULL

META_DAT$XSTK = META_DAT$XSTK/1000.
META_DAT$YSTK = META_DAT$YSTK/1000.
#attributes(XUTM)$units<-"km"
#attributes(YUTM)$units<-"km"
#LL = UTM2LL(UTMx=META_DAT$XSTK,UTMy=META_DAT$YSTK,clat = HEAD$plat,clon = HEAD$plon,tlat1 = HEAD$tlat1 ,tlat2 = HEAD$tlat2)
LL = UTM2LL(UTMx=META_DAT$XSTK,UTMy=META_DAT$YSTK)

if (addutm == FALSE) {
META_DAT$XSTK <- NULL
META_DAT$YSTK <- NULL
}

META_DAT = data.frame(LL$Lat,LL$Lon,DATETIME,META_DAT, stringsAsFactors=FALSE)
names(META_DAT)[1] <- "Lat"
names(META_DAT)[2] <- "Lon"



META_DAT = left_join(META_DAT, META_SPEC, by = "SPECIESN")
META_DAT$SPECIESN <- NULL
META_DAT = spread(META_DAT, key = "MSPEC", value = "CONC")
return(META_DAT)

}
