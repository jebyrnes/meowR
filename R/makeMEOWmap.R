#' @title makeMEOWmap
#'
#' @description
#' \code{makeMEOWmap} takes a dataset with values and a MEOW column and makes a map in \code{\link{ggplot2}}
#' 
#' @details  \code{makeMEOWmap} takes a dataset with values and a MEOW column and makes a map in \code{\link{ggplot2}}
#'
#' @author Jarrett Byrnes.
#' @param newdata dataset to be used
#' @param fillColName name of the column with information to be used for a fill value
#' @param regionColName data name of column with ecoregion, province, or realm names
#' @param type are these values from an ECOREGION, PROVINCE, or REALM map. Defaults to "ECOREGION". Note all caps.
#' @param fillPal The palatte used by \code{\link{scale_fill_gradientn}}. Defaults to \code{\link{brewer.pal}(11, "Spectral")}
#' @param pal The palatte used by \code{\link{scale_fill_manual}} if data is categorical. Defaults to "spectral" from \code{\link{RColorBrewer}}
#' @param pathCol The path color for regional outlines. Defaults to "black"
#' @param pathAlpha The alpha of the regional outlines. Defaults to 1.
#' @param guide What kind of guide should be used, and what should it's title be? Defaults to \code{\link{guide_colourbar}(title=fillColName)}
#' @param dataOut Return a merged dataframe for plotting instead of a plot? Defaults to \code{FALSE}
#' @param na.value How are NAs areas handled for fill? Defaults to making them not plot - NA.
#' @param add.worldmap Should a map of the world be plotted under the regions? Defaults to FALSE
#' @param excludeNoDataAreas Exclude areas/regions from the plot with no data?
#' @param ... Other arguments to be supplied to fitting function.
#' 
#' 
#' @export
#' @return Returns \code{\link{ggplot}} object
#' 
#'
#' @examples
#' 
#' data(regions.df)
#' 
#' ndf <- data.frame(Ecoregions = levels(regions.df$ECOREGION), 
#' Values = runif(length(levels(regions.df$ECOREGION)), 0,100))
#' 
#' makeMEOWmap(ndf, fillColName="Values", regionColName="Ecoregions")
#' 

###Function to make a map for provinces or regions
# Takes a dataframe as an argument. You specify the column that has the region
# in it - be it ECOREGION, ecoregion_name, or whatever. 
# type is whether this is an ECOREGION, PROVINCE, or REALM map where
# the regionColName's values match the appropriate type of map
# and fillColName is the name of the column that will determine the fill color
makeMEOWmap <- function(newdata, fillColName, regionColName=type, type="ECOREGION", 
                        fillPal=brewer.pal(11, "Spectral"), pal="Spectral",
                        pathCol="black", pathAlpha=1,
                        guide = guide_colourbar(title=fillColName),
                        dataOut=FALSE,  na.value=NA, add.worldmap=FALSE, 
                        excludeNoDataAreas=T, ...){
  regionData <- regions.df
  if(type=="PROVINCE") regionData <- provinces.df
  if(type=="REALM") regionData <- realms.df
  
  #sometimes the colname is different
  if(regionColName != type) regionData[[regionColName]] <- regionData[[type]]
  
  regionData <- join(regionData, newdata)
  regionData$score <- regionData[[fillColName]]
  
  if(excludeNoDataAreas && sum(is.na(regionData$score))>0) 
    regionData <- regionData[-which(is.na(regionData$score)),]
  
  if(dataOut) return(regionData)
  
  ret <- ggplot(regionData) + theme_bw(base_size=17) +
    aes(long,lat) + 
    geom_polygon(mapping=aes(fill=score, group=group)) +
    geom_path(color=pathCol, alpha=pathAlpha, mapping=aes(group=group)) +
    coord_equal() #
  
  if(is.numeric(regionData$score)){
    ret <- ret + scale_fill_gradientn(colours=fillPal, guide=guide, na.value=na.value, ...)
  }else{
    ret <- ret + scale_fill_manual(values=fillPal, guide=guide, na.value=na.value, ...)
    
  }
  
  if(add.worldmap){ 
    ret <- ret + geom_path(data=worldmap.df, aes(x=longitude, y=latitude, group=NA))
  }
  
  ret
}
