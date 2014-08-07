##copyright 2002 R. Gentleman, all rights reserved
##helper functions for dealing with data environments (soon to be hash
##tables)

## JZ added lookUp and modified the other functions so that they all
## use lookUp. Nov. 6, 2003.
lookUp <- function(x, data, what, load=FALSE) {
    if(length(x) < 1){
        stop("No keys provided")
    }
    mget(x, envir=getAnnMap(what, chip=data, load=load),
         ifnotfound=NA)
}

getGO <- function(x, data) {
    lookUp(x, data, "GO")
 }

 getGOdesc <- function(x, which = c("BP", "CC", "MF", "ANY")) {
     which <- match.arg(which)
     options(show.error.messages = FALSE)
     ans <- try(lookUp(x, "GO", "TERM", load=TRUE))
     options(show.error.messages = TRUE)
     onts <- sapply(ans, Ontology)
     if(inherits(ans, "try-error")){
         warning(paste("Invalid GO term", x))
         return(NULL)
     }else{
         if(which == "ANY"){
             return(ans)
         }else{
             ans <- ans[onts %in% which]
             if(length(ans) == 0){
                 return(NULL)
             }else{
                 return(ans)
             }
         }
     }
 }

  getSYMBOL <- function(x, data) {
      unlist(lookUp(x, data, "SYMBOL"))
 }

  getPMID <- function(x, data) {
      lookUp(x, data, "PMID")
  }

  getLL <- function(x, data) {
      .Defunct("getLL", package="annotate", msg="please use getEG")
  }

  getEG <- function(x, data) {
      unlist(lookUp(x, data, "ENTREZID"))
  }

# This function needs to be updated when new annotation items are
# added to the data packages
getUniqAnnItem <- function(){
    return(c("ACCNUM", "ENTREZID", "GENENAME", "SYMBOL", "MAP",
             "GRIF", "SUMFUNC", "NM", "NP"))
}
