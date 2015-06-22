#' meowR
#'
#' @name meowR
#' @docType package
#' 
#' @description Methods and data for adding Spalding et al.’s Marine Ecoregions of the World to plots. Data from http://www.marineregions.org/downloads.php downloaded on June 1st 2015.
#' @author Jarrett Byrnes
#' @import ggplot2 plyr sp maptools maps RColorBrewer
NULL

#' Marine Ecoregions of the World Data
#' @name regions
#' @aliases provinces realms regions.df provinces.df realms.df
#' @usage
#' data(regions)
#' data(provinces)
#' data(realms)
#' 
#' data(regions.df)
#' data(provinces.df)
#' data(realms.df)
#' @description Data from \link{http://www.marineregions.org/downloads.php}. All datasets provides either as \code{\link{SpatialPolygonsDataFrame}} objects or, with .df, as \code{\link{data.frame}} objects
#'
#' @docType data
#' @keywords data
#' @rdname regions
#' @references Spalding, M.D., Fox, H.E., Allen, G.R., Davidson, N., Ferdaña, Z.A., Finlayson, M., Halpern, B.S., Jorge, M.A., Lombana, A.L., Lourie, S.A., 2007. Marine ecoregions of the world: a bioregionalization of coastal and shelf areas. Bioscience 57, 573–583.
#' @examples
#' 
#' data(regions)
#' data(provinces)
#' data(realms)
#' 
#' par(mfrow=c(2,2), mar=c(0,1,2,0))
#' plot(regions, main="Ecoregion")
#' plot(provinces, lwd=2, border="red", main="Province")
#' plot(realms, lwd=2, border="blue", main="Realm")
#' par(mfrow=c(1,1), mar=c(5, 4, 4, 2) + 0.1)
#' 
#' #or with ggplot2
#' base_province_ggplot <- ggplot(provinces.df) + theme_bw() +
#' aes(long,lat,group=group) + 
#'   geom_polygon(fill=NA) +
#'   geom_path(color="black") +
#'   coord_equal() 
#'   
#'   base_province_ggplot
NULL


#' Map of the world
#' @name worldmap
#' @aliases worldmap.df
#' @usage
#' data(worldmap)
#' data(worldmap.df)
#' @description Data from \link{maps} package. All datasets provides either as \code{\link{SpatialPolygonsDataFrame}} objects or, with .df, as \code{\link{data.frame}} objects
#'
#' @docType data
#' @keywords data
#' @examples
#' 
#' data(worldmap)
#' plot(worldmap, type="l")
#' 
#' data(worldmap.df)
#' ggplot(worldmap.df, aes(x=longitude, y=latitude))+geom_path()
#' 
NULL