#' @title makeMEOWmapData
#'
#' @description
#' \code{makeMEOWmapData} takes a dataset with values and a MEOW column merges them for use by \code{\link{makeMEOWmap}}
#' 
#' @details  \code{makeMEOWmapData} takes a dataset with values and a MEOW column and makes a joined \code{\link{data.frame}}
#'
#' @author Jarrett Byrnes.
#' @param newdata dataset to be used
#' @param fillColName name of the column with information to be used for a fill value
#' @param regionColName data name of column with ecoregion, province, or realm names
#' @param type are these values from an ECOREGION, PROVINCE, or REALM map. Defaults to "ECOREGION". Note all caps.
#' @param fillAlphaColName Colname of column used to scale alpha level of fill
#' @param excludeNoDataAreas Exclude areas/regions from the plot with no data?
#' 
#' 
#' @export
#' @return Returns \code{\link{data.frame}} object
#' 
#'
#' @examples
#' 
#' data(regions.df)
#' 
#' ndf <- data.frame(Ecoregions = levels(regions.df$ECOREGION), 
#' Values = runif(length(levels(regions.df$ECOREGION)), 0,100))
#' 
#' head(makeMEOWmapData(ndf, fillColName="Values", regionColName="Ecoregions"))
#' 

makeMEOWmapData <- function (newdata, fillColName, fillAlphaColName=NULL,
                             regionColName = type, type = "ECOREGION",
                             excludeNoDataAreas = T) {
  regionData <- regions.df
  if (type == "PROVINCE") 
    regionData <- provinces.df
  if (type == "REALM") 
    regionData <- realms.df
  if (regionColName != type) 
    regionData[[regionColName]] <- regionData[[type]]
  
  #join the regional data with our new data
  regionData <- join(regionData, newdata)
  regionData$score <- regionData[[fillColName]]
  regionData$fillAlpha <- NA
  if(!is.null(fillAlphaColName)) regionData$fillAlpha <- regionData[[fillAlphaColName]]
  if (excludeNoDataAreas && sum(is.na(regionData$score)) > 
      0) 
    regionData <- regionData[-which(is.na(regionData$score)),] 

  return(regionData)
}