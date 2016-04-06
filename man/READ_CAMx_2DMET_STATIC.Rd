\name{READ_CAMx_2DMET_STATIC}
\alias{READ_CAMx_2DMET_STATIC}
\title{Reads CAMx Static 2D Met File}
\description{
  Reads CAMx static 2D met file and returns a R data frame.
}
\usage{
READ_CAMx_2DMET_STATIC(input,timezone="GMT", 
strp = TRUE, addutm = FALSE)
}
\arguments{
  \item{input}{Input data file of CAMx static 2D met in UAM format.}

  \item{timezone}{Timezone of input data file.}

  \item{strp}{Optional flag to include boundary cell data.}

  \item{addutm}{Optional flag to include model utm coordinates in
output data frame.}
}
