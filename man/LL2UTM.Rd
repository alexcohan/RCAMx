\name{LL2UTM}
\alias{LL2UTM}
\title{Convert Lat/Lon to UTM coordinates}
\description{
  Reads arrays of Lat/Lon and returns arrays of model UTM coordinates.
}
\usage{
LL2UTM(Lat,Lon,
clat = 40.,clon = -97.,tlat1 = 33. ,tlat2 = 45.)
}
\arguments{
  \item{Lat}{Real array of latitudes.}

  \item{Lon}{Real array of longitudes.}

  \item{clat}{Central latitude of projection.}

  \item{clon}{Central longitude of projection.}

  \item{tlat1}{First true latitute of projection.}

  \item{tlat2}{Second true latitute of projection.}
}
