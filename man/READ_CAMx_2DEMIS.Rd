\name{READ_CAMx_2DEMIS}
\alias{READ_CAMx_2DEMIS}
\title{Reads CAMx Area Source Emmissions File}
\description{
  Reads CAMx area source emissions file and returns a R data frame.
}
\usage{
READ_CAMx_2DEMIS(input,temp="TEMPXX25V367X1.dat",timezone="GMT", 
strp = TRUE, addutm = FALSE)
}
\arguments{
  \item{input}{Input data file of CAMx area sources in UAM format.}

  \item{temp}{Temporary file used to store species names.} 

  \item{timezone}{Timezone of input data file.}

  \item{strp}{Optional flag to include boundary cell data.}

  \item{addutm}{Optional flag to include model utm coordinates in
output data frame.}
}
