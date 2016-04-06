#' @export
LL2UTM <- function(Lat,Lon,clat = 40.,clon = -97.,tlat1 = 33. ,tlat2 = 45.) {

sign = 1.
pole = 90.
conv = 57.29578
a1 = 6370.
unit = attr(Lat,'units')


if ( ! is.null(unit) ) {
if ( (unit != 'degrees') & (unit != 'deg') ) {
stop("UNKNOWN UNITS "+unit+"")
}
}

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

ylon = Lon - clon
ylon[ylon > 180.] = ylon - 360.
ylon[ylon < -180.] = ylon + 360.

UTMx = array(0,length(Lon))
UTMy = array(0,length(Lon))

flp = xn*ylon/conv
psx = (pole - Lat)/conv
r = -a1/xn*sin(psi1)*(tan(psx/2.)/tan(psi1/2.))^xn

UTMy = r*cos(flp)
if ( clat < 0 ) {
UTMx = r*sin(flp)
} else {
UTMx = -r*sin(flp)
}

UTMx = UTMx - xc
UTMy = UTMy - yc

TT = data.frame(UTMx,UTMx, stringsAsFactors=FALSE)
names(TT) = c("UTMx","UTMy")
#TT <- as.data.frame(TT)
#attr(TT,'units') <- 'km'
attributes(TT)$units<-"km"

return(TT)
}
