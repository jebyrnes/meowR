% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeMEOWmapData.R
\name{makeMEOWmapData}
\alias{makeMEOWmapData}
\title{makeMEOWmapData}
\usage{
makeMEOWmapData(newdata, fillColName, fillAlphaColName = NULL,
  regionColName = type, type = "ECOREGION", excludeNoDataAreas = T)
}
\arguments{
\item{newdata}{dataset to be used}

\item{fillColName}{name of the column with information to be used for a fill value}

\item{fillAlphaColName}{Colname of column used to scale alpha level of fill}

\item{regionColName}{data name of column with ecoregion, province, or realm names}

\item{type}{are these values from an ECOREGION, PROVINCE, or REALM map. Defaults to "ECOREGION". Note all caps.}

\item{excludeNoDataAreas}{Exclude areas/regions from the plot with no data?}
}
\value{
Returns \code{\link{data.frame}} object
}
\description{
\code{makeMEOWmapData} takes a dataset with values and a MEOW column merges them for use by \code{\link{makeMEOWmap}}
}
\details{
\code{makeMEOWmapData} takes a dataset with values and a MEOW column and makes a joined \code{\link{data.frame}}
}
\examples{

data(regions.df)

ndf <- data.frame(Ecoregions = levels(regions.df$ECOREGION), 
Values = runif(length(levels(regions.df$ECOREGION)), 0,100))

head(makeMEOWmapData(ndf, fillColName="Values", regionColName="Ecoregions"))

}
\author{
Jarrett Byrnes.
}

