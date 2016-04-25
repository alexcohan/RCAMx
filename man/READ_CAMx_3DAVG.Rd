\name{READ_CAMx_3DAVG}
\alias{READ_CAMx_3DAVG}
\title{Reads CAMx 3D Average Concentration File}
\description{
  Reads CAMx 3D average concentration file and returns a R data frame.
}
\usage{
READ_CAMx_3DAVG(inputAVG,inputMET,timezone="GMT",
strp = TRUE, addutm = FALSE, temp="TEMPXX25V367X1.dat")
}
\arguments{
  \item{inputAVG}{Input data file of CAMx 3D average concentration file in UAM format.}

  \item{inputMET}{Input data file of CAMx 3D metereology file in UAM format.}

  \item{timezone}{Timezone of input data file.}

  \item{strp}{Optional flag to include boundary cell data.}

  \item{addutm}{Optional flag to include model utm coordinates in
output data frame.}

  \item{temp}{Temporary file used to store species names.  RCAMx needs access to read and write this file.}

}
