#' Get descend
#'
#' Get a vector of taxids (numeric) from maximum 1000 of the descendants from each taxid in input.
#'
#' @param taxids a vector of taxids
#'
#' @return descend a vector of taxids
#'
#' @import jsonlite
#'
#' @export
#'
#' @examples get_descend(2)
#'
get_descend<-function(taxids) {
  taxids<-as.character(taxids)
  descend<-NULL
  i<-1
  while(i<=length(taxids)) { #for each taxid in vector by group of 100
    # url cannot be too long, so that we need to cut the taxids
    # (100 max in one chunk)
    # and make as many requests as necessary
    cat(".")
    taxids_sub<-taxids[i:(i+99)]
    taxids_sub<-taxids_sub[!is.na(taxids_sub)] # remove NA values
    taxids_sub<-paste(taxids_sub, collapse="%20") # accepted space separator in url
    url<-paste("http://lifemap-ncbi.univ-lyon1.fr/solr/addi/select?q=ascend:(",taxids_sub,")&wt=json&rows=1000",sep="", collapse="") #get all taxids of descendant groups in database
    # do the request from Solr
    data_sub<-fromJSON(url)
    if (data_sub$response$numFound!=0) { # if query succeeded
      for (j in data_sub$response$docs[,"taxid"]){ # descend is the taxid of descendant groups
        descend<-append(descend,j) # add each taxid to the list of results
      }
    }
    i<-i+100
  }
  return(descend)
}

