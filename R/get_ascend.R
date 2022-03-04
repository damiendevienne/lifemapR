#' Get ascend
#'
#' Get a vector of taxid (numeric) from maximum 1000 of the ascendants from each taxid in input.
#'
#' @param taxids a vector of taxids
#'
#' @return A vector of taxid (numeric) summarizing the ancestors of the groups.
#'
#' @import jsonlite
#'
#' @export
#'
#' @examples get_ascend(2087)
#'
get_ascend<-function(taxids) {
  taxids<-as.character(taxids)
  ascend<-NULL
  i<-1
  while(i<=length(taxids)) { #for each taxid in vector by group of 100
    # url cannot be too long, so that we need to cut the taxids
    # (100 max in one chunk)
    # and make as many requests as necessary
    cat(".")
    taxids_sub<-taxids[i:(i+99)]
    taxids_sub<-taxids_sub[!is.na(taxids_sub)] # remove NA values
    taxids_sub<-paste(taxids_sub, collapse="%20") # accepted space separator in url
    url<-paste("http://lifemap-ncbi.univ-lyon1.fr/solr/addi/select?q=taxid:(",taxids_sub,")&wt=json&rows=1000",sep="", collapse="") #get all taxids of ascendant groups in database
    # do the request from Solr
    data_sub<-fromJSON(url)
    if (data_sub$response$numFound!=0) { # if query succeeded
      for (j in data_sub$response$docs[,"ascend"]) { # ascend is the taxid of ascendant groups
        ascend<-append(ascend,j) # add each taxid to the list of results
      }
    }
    i<-i+100
  }
  return(ascend)
}
