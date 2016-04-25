\name{READ_CAMx_2DMET}
\alias{READ_CAMx_2DMET}
\title{Reads CAMx 2D Met File}
\description{
  Reads CAMx 2D met file and returns a R data frame.
}
\usage{
READ_CAMx_2DMET(input,timezone="GMT", 
strp = TRUE, addutm = FALSE, temp="TEMPXX25V367X1.dat")
}
\arguments{
  \item{input}{Input data file of CAMx 2D met in UAM format.}

  \item{timezone}{Timezone of input data file.}

  \item{strp}{Optional flag to include boundary cell data.}

  \item{addutm}{Optional flag to include model utm coordinates in
output data frame.}

  \item{temp}{Temporary file used to store species names.  RCAMx needs access to read and write this file.}
}
