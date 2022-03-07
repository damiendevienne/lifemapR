#' lifemap
#'
#' Create a new lifemap using the basemap set by \code{lmpar()}.
#'
#' @param data a data frame with at least two columns (lat and long). This 
#' data frame can be (but does not have to) the result of a call to \code{taxid2map()}.
#' @param ... other options, as passed to \code{leaflet()}
#'
#' @return A map widget.
#'
#' @import leaflet
#'
lifemap <- function(data=NULL, ...) {
  lifemap<-leaflet(data=data,...)
  urlprefix<-geturlprefix()
  tilesurl<-paste(urlprefix, "osm_tiles/{z}/{x}/{y}.png", sep="")
  #default for the map
  lifemap<-addTiles(lifemap, tilesurl, options = tileOptions(maxZoom = 42))
  lifemap<-setView(lifemap, 0,-5,5)
  return(lifemap)
}

