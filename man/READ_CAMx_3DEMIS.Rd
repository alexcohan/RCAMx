\name{READ_CAMx_3DEMIS}
\alias{READ_CAMx_3DEMIS}
\title{Reads CAMx Point Source Emmissions File}
\description{
  Reads CAMx point source emissions file and returns a R data frame.
}
\usage{
READ_CAMx_3DEMIS(input,timezone="GMT", addutm = FALSE, temp="TEMPXX25V367X1.dat")
}
\arguments{
  \item{input}{Input data file of CAMx point sources in UAM format.}

  \item{timezone}{Timezone of input data file.}

  \item{addutm}{Optional flag to include model utm coordinates in
output data frame.}

  \item{temp}{Temporary file used to store species names.  RCAMx needs access to read and write this file.} 

}
