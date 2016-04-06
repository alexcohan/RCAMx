\name{READ_CAMx_3DEMIS}
\alias{READ_CAMx_3DEMIS}
\title{Reads CAMx Point Source Emmissions File}
\description{
  Reads CAMx point source emissions file and returns a R data frame.
}
\usage{
READ_CAMx_3DEMIS(input,temp="TEMPXX25V367X1.dat",timezone="GMT", 
addutm = FALSE)
}
\arguments{
  \item{input}{Input data file of CAMx point sources in UAM format.}

  \item{temp}{Temporary file used to store species names.} 

  \item{timezone}{Timezone of input data file.}

  \item{addutm}{Optional flag to include model utm coordinates in
output data frame.}
}
