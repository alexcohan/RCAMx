#' @export
READ_CAMx_3DAVG <- function(inputAVG,inputMET,timezone="GMT",strp = TRUE, addutm = FALSE, temp="TEMPXX25V367X1.dat") {
system(paste("rm -f",temp,sep=" "),intern = FALSE)

#read avg file
HEAD1 <- .Fortran("READ_HEAD_BIG2",ifile=inputAVG,NX=as.integer(1),NY=as.integer(1),NZ=as.integer(1),MAXS=as.integer(1),
         plon=as.single(1),plat=as.single(1),tlat1=as.single(1),tlat2=as.single(1),XORG=as.single(1),YORG=as.single(1),DX=as.single(1),DY=as.single(1),IDATA=as.integer(1),BEGTIME=as.single(1),
         ENDTIME=as.single(1),ifile2=temp)

META_SPEC1 = read.csv(temp, header = FALSE,stringsAsFactors=FALSE)
META_SPEC1 = as.data.frame(META_SPEC1)
names(META_SPEC1) = c("SPECIESN","MSPEC")
system(paste("rm -f",temp,sep=" "),intern = FALSE)


NBIG1 = HEAD1$NX*HEAD1$NY*HEAD1$NZ*24*HEAD1$MAXS

OUT1 <- .Fortran("READ_CAMx_IJK",ifile=inputAVG,NBIG=as.integer(NBIG1),IS=integer(NBIG1),JS=integer(NBIG1),KS=integer(NBIG1),HRS=integer(NBIG1),
         SPECIES=integer(NBIG1),CONC=single(NBIG1))

#read 3dmet file
HEAD2 <- .Fortran("READ_HEAD_BIG2",ifile=inputMET,NX=as.integer(1),NY=as.integer(1),NZ=as.integer(1),MAXS=as.integer(1),
         plon=as.single(1),plat=as.single(1),tlat1=as.single(1),tlat2=as.single(1),XORG=as.single(1),YORG=as.single(1),DX=as.single(1),DY=as.single(1),IDATA=as.integer(1),BEGTIME=as.single(1),
         ENDTIME=as.single(1),ifile2=temp)

META_SPEC2 = read.csv(temp, header = FALSE,stringsAsFactors=FALSE)
META_SPEC2 = as.data.frame(META_SPEC2)
names(META_SPEC2) = c("SPECIESN","MSPEC")
system(paste("rm -f",temp,sep=" "),intern = FALSE)

NBIG2 = HEAD2$NX*HEAD2$NY*HEAD2$NZ*24*HEAD2$MAXS

OUT2 <- .Fortran("READ_CAMx_IJK",ifile=inputMET,NBIG=as.integer(NBIG2),IS=integer(NBIG2),JS=integer(NBIG2),KS=integer(NBIG2),HRS=integer(NBIG2),
         SPECIES=integer(NBIG2),CONC=single(NBIG2))

#find ZGRID_M
NH = grep("ZGRID_M",META_SPEC2$MSPEC,ignore.case=T)

#strip boundary cells?

if (strp == TRUE) {
III = (OUT1$IS>1 & OUT1$IS<HEAD1$NX & OUT1$JS>1 & OUT1$JS<HEAD1$NY)
OUT3 = data.frame(IS = OUT1$IS[III],
                 JS = OUT1$JS[III],
                 KS = OUT1$KS[III],
                 HRS = OUT1$HRS[III],
                 SPECIES = OUT1$SPECIES[III],
                 CONC = OUT1$CONC[III])

rm(OUT1)
rm(III)
OUT1 = OUT3
rm(OUT3)

III = (OUT2$IS>1 & OUT2$IS<HEAD1$NX & OUT2$JS>1 & OUT2$JS<HEAD1$NY & OUT2$SPECIES == NH)
OUT4 = data.frame(IS = OUT2$IS[III],
                  JS = OUT2$JS[III],
                  KS = OUT2$KS[III],
                  HRS = OUT2$HRS[III],
                  SPECIES = OUT2$SPECIES[III],
                  CONC = OUT2$CONC[III])
rm(III)
rm(OUT2)
OUT2 = OUT4
rm(OUT4)
} else {

III = OUT2$SPECIES == NH
OUT4 = data.frame(IS = OUT2$IS[III],
                  JS = OUT2$JS[III],
                  KS = OUT2$KS[III],
                  HRS = OUT2$HRS[III],
                  SPECIES = OUT2$SPECIES[III],
                  CONC = OUT2$CONC[III])
rm(III)

rm(OUT2)
OUT2 = OUT4
rm(OUT4)
}


OUT1$SPECIES=as.integer(OUT1$SPECIES)
OUT1$ifile <- NULL
OUT1$NBIG <- NULL
HIJK = paste(as.character(OUT2$IS),as.character(OUT2$JS),as.character(OUT2$KS),as.character(OUT2$HRS), sep = "ZZ")
OUT2 = data.frame(HIJK, OUT2$CONC, stringsAsFactors=FALSE)
names(OUT2)[2] <- "ZGRID_M"

rm(HIJK)
HIJK = paste(as.character(OUT1$I),as.character(OUT1$J),as.character(OUT1$K),as.character(OUT1$HR), sep = "ZZ")
OUT1 = data.frame(OUT1,HIJK, stringsAsFactors=FALSE)
META_DAT = left_join(OUT1, OUT2, by = "HIJK")
META_DAT$HIJK <- NULL

names(META_DAT)=c("I","J","K","HR","SPECIESN","CONC","ZGRID_M")

META_DAT = left_join(META_DAT, META_SPEC1, by = "SPECIESN")
META_DAT$SPECIESN <- NULL
META_DAT = spread(META_DAT, key = "MSPEC", value = "CONC")

if (addutm == FALSE) {
META_DAT$K <- NULL
}


HRS = as.character(META_DAT$HR+HEAD1$BEGTIME)
HRS[META_DAT$HR<10]= paste("0",HRS[META_DAT$HR<10], sep = "")

IDAY = HEAD1$IDATA%%1000.
IYR = (HEAD1$IDATA - IDAY)/1000.
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

XUTM = (HEAD1$XORG+HEAD1$DX*(META_DAT$I-1.))/1000.
YUTM = (HEAD1$YORG+HEAD1$DY*(META_DAT$J-1.))/1000.
attributes(XUTM)$units<-"km"
attributes(YUTM)$units<-"km"
LL = UTM2LL(UTMx=XUTM,UTMy=YUTM,clat = HEAD1$plat,clon = HEAD1$plon,tlat1 = HEAD1$tlat1 ,tlat2 = HEAD1$tlat2)

META_DAT$I <- NULL
META_DAT$J <- NULL
META_DAT$HR <- NULL

if (addutm == TRUE) {
META_DAT = data.frame(XUTM,YUTM,LL$Lat,LL$Lon,DATETIME, META_DAT, stringsAsFactors=FALSE)
names(META_DAT)[3] <- "Lat"
names(META_DAT)[4] <- "Lon"
} else {
META_DAT$K <- NULL
META_DAT = data.frame(LL$Lat,LL$Lon,DATETIME, META_DAT, stringsAsFactors=FALSE)
names(META_DAT)[1] <- "Lat"
names(META_DAT)[2] <- "Lon"
}


return(META_DAT)

}
