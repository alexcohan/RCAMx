#' @export
READ_CAMx_2DMET <- function(input,timezone="GMT",strp = TRUE, addutm = FALSE,temp="TEMPXX25V367X1.dat") {

system(paste("rm -f",temp,sep=" "),intern = FALSE)

HEAD <- .Fortran("READ_HEAD_BIG2",ifile=input,NX=as.integer(1),NY=as.integer(1),NZ=as.integer(1),MAXS=as.integer(1),
         plon=as.single(1),plat=as.single(1),tlat1=as.single(1),tlat2=as.single(1),XORG=as.single(1),YORG=as.single(1),DX=as.single(1),DY=as.single(1),IDATA=as.integer(1),BEGTIME=as.single(1),
         ENDTIME=as.single(1),ifile2=temp)

META_SPEC = read.csv(temp, header = FALSE,stringsAsFactors=FALSE)
META_SPEC = as.data.frame(META_SPEC)
names(META_SPEC) = c("SPECIESN","MSPEC")
system(paste("rm -f",temp,sep=" "),intern = FALSE)



NBIG = HEAD$NX*HEAD$NY*24*HEAD$MAXS

OUT <- .Fortran("READ_CAMx_IJ",ifile=input,NBIG=as.integer(NBIG),IS=integer(NBIG),JS=integer(NBIG),HRS=integer(NBIG),
         SPECIES=integer(NBIG),CONC=single(NBIG))

if (strp == TRUE) {
III = OUT$IS>1 & OUT$IS<HEAD$NX & OUT$JS>1 & OUT$JS<HEAD$NY
OUT2 = data.frame(IS = OUT$IS[III],
                  JS = OUT$JS[III],
                  HRS = OUT$HRS[III],
                  SPECIES = OUT$SPECIES[III],
                  CONC = OUT$CONC[III])
rm(III)
rm(OUT)
OUT = OUT2
rm(OUT2)
}

OUT$SPECIES=as.integer(OUT$SPECIES)
OUT$ifile <- NULL
OUT$NBIG <- NULL
OUT$HR = OUT$HR + HEAD$BEGTIME
HRS = as.character(OUT$HR)
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

XUTM = (HEAD$XORG+HEAD$DX*(OUT$IS-1.))/1000.
YUTM = (HEAD$YORG+HEAD$DY*(OUT$JS-1.))/1000.
attributes(XUTM)$units<-"km"
attributes(YUTM)$units<-"km"
LL = UTM2LL(UTMx=XUTM,UTMy=YUTM,clat = HEAD$plat,clon = HEAD$plon,tlat1 = HEAD$tlat1 ,tlat2 = HEAD$tlat2)

if (addutm == TRUE) {
META_DAT = data.frame(XUTM,YUTM,LL$Lat,LL$Lon,DATETIME,OUT$SPECIES,OUT$CONC, stringsAsFactors=FALSE)
colnames(META_DAT)=c("XUTM","YUTM","Lat","Lon","DATETIME","SPECIESN","CONC")
} else {
META_DAT = data.frame(LL$Lat,LL$Lon,DATETIME,OUT$SPECIES,OUT$CONC, stringsAsFactors=FALSE)
colnames(META_DAT)=c("Lat","Lon","DATETIME","SPECIESN","CONC")
}



META_DAT = left_join(META_DAT, META_SPEC, by = "SPECIESN")
META_DAT$SPECIESN <- NULL
META_DAT = spread(META_DAT, key = "MSPEC", value = "CONC")
META_DAT$K <- NULL
return(META_DAT)

}
