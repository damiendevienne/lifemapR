#' geturlprefix
#'
#' Gets the prefix of the url used for accessing Lifemap
#' based on the basemap option set with lmpar().
#' This is mainly used as an internal function.
#'
#' @param basemap To which basemap are the data retrieved associated.
#' By default, the basemap retrieved by function \code{lmpar()} is used.
#'
#' @return Get or set the name of the basemap.
#'


geturlprefix<-function(basemap=NULL) {
  if(!is.null(basemap)) {
    if (basemap %in% c("default","ncbi","virus","fr")) {
        map<-basemap
      } 
  }
  else map<-lmpar()
	switch(map,
         "ncbi"={
           urlprefix="http://lifemap-ncbi.univ-lyon1.fr/"
         },
         "virus"={
           urlprefix="http://virusmap.univ-lyon1.fr/"
         },
         "default"={
           urlprefix="http://lifemap.univ-lyon1.fr/"
         },
         "fr"={
           urlprefix="http://lifemap-fr.univ-lyon1.fr/"
         },
    )
	return(urlprefix)
}
