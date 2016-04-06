UTM2LL <- function(UTMx,UTMy,clat = 40.,clon = -97.,tlat1 = 33. ,tlat2 = 45.) {
# convert from model UTM coordinates (km or m) to Lat/Lon

sign = 1.
pole = 90.
conv = 57.29578
a1 = 6370.
unit = attr(UTMx,'units')

if (clat < 0) {
sign = -1.
}
pole = 90.*sign

if (abs(tlat1)>90.) {
tlat1b = 60.*sign
tlat2b = 30.*sign
} else {
tlat1b = tlat1
tlat2b = tlat2
}

xn = log10(cos(tlat1b/conv)) - log10(cos(tlat2b/conv))
xn = xn/(log10(tan((45. - sign*tlat1/2.)/conv)) - log10(tan((45. - sign*tlat2/2.)/conv)))
psi1 = sign*(90. - sign*tlat1)/conv
psi0 = (pole - clat)/conv

xc = 0.
yc = -a1/xn*sin(psi1)*(tan(psi0/2.)/tan(psi1/2.))^xn

UTMx1 = UTMx + xc
UTMy1 = UTMy + yc
if ( ! is.null(unit) ) {
if ( (unit == 'm') | (unit == 'M') ) {
UTMx1 = UTMx/1000. + xc
UTMy1 = UTMy/1000. + yc
} else {
if ( (unit != 'km') & (unit != 'KM') ) {
stop("UNKNOWN UNITS "+unit+"")
}
}
}

#make flp = 0 array
flp = array(0,length(UTMx))

flp[(UTMy1 == 0.) & (UTMx1 >= 0)] = 90./conv
flp[(UTMy1 == 0.) & (UTMx1 < 0)] = -90./conv
flp[(UTMy1 != 0.) & (clat < 0 )] = atan2(UTMx1,UTMy1)
flp[(UTMy1 != 0.) & (clat >= 0 )] = atan2(UTMx1,-UTMy1)

flpp = (flp/xn)*conv + clon
flpp[flpp < -180.] = flpp + 360.
flpp[flpp > 180.] = flpp - 360.

Lon = flpp

r = sign*sqrt((UTMx1^2) + (UTMy1^2))
cell = r*xn/(a1*sin(psi1))
cell2 = atan( tan(psi1/2.)*(cell^(1./xn))  )
Lat = pole - (2.*cell2*conv)

TT = data.frame(Lat,Lon, stringsAsFactors=FALSE)
names(TT) = c("Lat","Lon")
#TT <- as.data.frame(TT)
#attr(TT,'units') <- 'degrees'
attributes(TT)$units<-"degrees"

return(TT)
}

