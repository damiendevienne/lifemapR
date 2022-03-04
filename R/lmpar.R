#' lmpar
#'
#' Get or set the basemap for lifemap as an option. 
#' This function is called by others to know what map to  
#' display and what database to query to get the taxa
#' coordinates.
#'
#' @param map a character string for the version of lifemap among 'standard', 'ncbi', 'french' and virus' version; 'standard' by default
#'
#' @return Get or set the name of the basemap.
#'



lmpar<-function(map="default") {
	currmap <- getOption("lm-basemap")
	if (is.null(currmap)) { #first time calling this function
			cat("Setting basemap to 'default'\n")
			cat("  Call lmpar() with option map='ncbi', 'fr' or 'virus' to change the basemap.\n")
			options("lm-basemap"="default")
	}
	call<-as.list(match.call())
	if (is.null(call$map)) { #the function is called without arguments
		map<-getOption("lm-basemap")
		return(map)
	}
	else {
		if (map %in% c("default", "ncbi", "fr","virus")) options("lm-basemap"=map)
		else {
			cat("Invalid basemap name. Should be one of 'default', 'ncbi', 'fr' or 'virus' \n")
			cat("  Basemap was unchanged\n")
		}
	}
}
