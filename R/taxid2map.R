#' taxid2map
#'
#' This function queries the Lifemap database to retrieve
#' the coordinates of taxa on Lifemap. Queries are based on ncbi taxid(s). 
#'
#' @param taxid either a unique taxid or a vector of taxids for which to
#' retrieve coordinates
#' @param remove.missing Logical. Should taxids that were not found
#' be removed from the output. Default to TRUE.
#' @param remove.duplicates Logical. Should duplicated taxids (if any)
#' be removed from the output. Default to TRUE.
#' @param verbose Logical. Should warnings be written if case of duplicated or invalid 
#' taxids. Default to TRUE.
#' retrieve coordinates
#'
#' @return A data frame with lon, lat and zoom, with the first column 
#' in the same order as the input taxid.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @export
#'
#' @examples taxid2map(c(2,9443,2087))

taxid2map<-function(taxid, remove.missing=TRUE, remove.duplicates=TRUE, verbose=TRUE) {
  if (sum(duplicated(taxid))>0) {
    if (verbose) cat("Warning! Some taxids are duplicated.\n")
    if (remove.duplicates) taxid<-unique(taxid)
  }
  taxids<-as.character(taxid) # change to characters and remove duplicates
  coordinates<-NULL # coordinates of detected groups in the lifemap
  wrongs <- vector()  # vector that will contain unfound groups from info
  i<-1
  url.prefix<-geturlprefix()

  # create progress bar
  pb <- txtProgressBar(min = 0, max = length(taxid), style = 3)

  while(i<=length(taxid)) {
    setTxtProgressBar(pb, i)
    # url cannot be too long, so that we need to cut the taxids
    # (100 max in one chunk)
    # and make as many requests as necessary
    cat(".")
    taxids_sub<-taxid[i:(i+99)]
    taxids_sub<-taxids_sub[!is.na(taxids_sub)]
    taxids_sub<-paste(taxids_sub, collapse="%20") # accepted space separator in url
    url<-paste(url.prefix,"/solr/taxo/select?q=taxid:(",taxids_sub,")&wt=json&rows=1000",sep="", collapse="")
    # do the Solr Query
    data_sub<-fromJSON(url)
    if (data_sub[["response"]][["numFound"]] != 0)
      coordinates<-rbind(coordinates,data_sub$response$docs[,c("taxid","lon","lat", "zoom")])
      i<-i+100
  }
  close(pb)
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
  
  #reorder everything as in the original order
  coordinates2<-coordinates[match(taxid, coordinates$taxid),]
  coordinates2$taxid<-taxids
  #remove taxid if not recovered. 
  if (remove.missing) {
    coordinates2<-coordinates2[!is.na(coordinates2$lon),]
    if (verbose) cat(paste(howmanymissing," taxa removed\n", sep=""))
  }
  rownames(coordinates2)<-NULL
  colnames(coordinates2)[which(colnames(coordinates2)=="lon")]<-"lng"
  #format output
  return(coordinates2)
}
