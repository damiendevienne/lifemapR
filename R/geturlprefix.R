#' geturlprefix
#'
#' Gets the prefix of the url used for accessing Lifemap
#' based on the basemap option set with lmpar().
#' This is mainly used as an internal function.
#'
#'
#' @return Get or set the name of the basemap.
#'


geturlprefix<-function() {
	map<-lmpar()
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