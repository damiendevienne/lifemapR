#' taxid2map
#'
#' This function queries the Lifemap database to retrieve
#' the coordinates of taxa on Lifemap. Queries are based on ncbi taxid(s). 
#'
#' @param taxid either a unique taxid or a vector of taxids for which to
#' retrieve coordinates
#' @param verbose Boolean. Should warnings be written if case of duplicated or invalid 
#' taxids. Default to TRUE.
#' retrieve coordinates
#'
#' @return A list with two components: 
#' coo: a dataframe with information for the taxid(s) requested: 
#' lat, long, scientific name, zoom at which the taxa is visible, and 
#' number of descendants of this taxa.
#' missing: a 
#'
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
#' @examples taxid2map(c(2,9443,2087))

taxid2map<-function(taxid, verbose=TRUE) {
  if (sum(duplicated(taxid))>0 & verbose) cat("Some taxids were duplicated. They ware removed.\n")
  taxids<-as.character(unique(taxid)) # change to characters and remove duplicates
  coordinates<-NULL # coordinates of detected groups in the lifemap
  wrongs <- vector()  # vector that will contain unfound groups from info
  i<-1
  while(i<=length(taxid)) {
    # url cannot be too long, so that we need to cut the taxids
    # (100 max in one chunk)
    # and make as many requests as necessary
    cat(".")
    taxids_sub<-taxid[i:(i+99)]
    taxids_sub<-taxids_sub[!is.na(taxids_sub)]
    taxids_sub<-paste(taxids_sub, collapse="%20") # accepted space separator in url
    url.prefix<-geturlprefix()
    url<-paste(url.prefix,"/solr/taxo/select?q=taxid:(",taxids_sub,")&wt=json&rows=1000",sep="", collapse="")
    # do the Solr Query
    data_sub<-fromJSON(url)
    if (data_sub[["response"]][["numFound"]] != 0)
      coordinates<-rbind(coordinates,data_sub$response$docs[,c("taxid","lon","lat", "sci_name","zoom","nbdesc")])
      i<-i+100
  }
  cat("\n")
  if (!is.null(coordinates)) {
    for (j in 1:ncol(coordinates)) coordinates[,j]<-unlist(coordinates[,j])
    class(coordinates$taxid)<-"character"
  }
  #TODO: deal with missing taxids, and find a way to get to them.
  missing<-NULL
  missingtaxids<-taxids[which(is.na(match(taxids, coordinates$taxid)))]
  howmanymissing<-length(missingtaxids)
  if (howmanymissing>0) {
    if (verbose) cat(paste("Warning! We couldn't retrieve ",howmanymissing," taxid(s)\n", sep=""))
    missing<-missingtaxids
  }
  #format output
  res<-list()
  res$coo<-coordinates
  res$missing<-missing
  if (is.null(res$coo)) res$coo<-NA
  if (is.null(res$missing)) res$missing<-NA
  return(res)
}
