\name{UTM2LL}
\alias{UTM2LL}
\title{Convert UTM coordinates to Lat/Lon}
\description{
  Reads arrays of UTM coordinates and returns arrays of Latitude and longitude.
}
\usage{
UTM2LL(UTMx,UTMy,
clat = 40.,clon = -97.,tlat1 = 33. ,tlat2 = 45.)
}
\arguments{
  \item{UTMx}{Real array of UTM x-coordinates.}

  \item{UTMy}{Real array of UTM y-coordinates.}

  \item{clat}{Central latitude of projection.}

  \item{clon}{Central longitude of projection.}

  \item{tlat1}{First true latitute of projection.}

  \item{tlat2}{Second true latitute of projection.}
}
