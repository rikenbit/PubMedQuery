##Copyright R. Gentleman, 2004
##simple functions to get Evidence codes

.isMissingGOEntry <- function(x) (length(x) == 1L && is.na(x))

##get then GO term names for a particular (sub)ontology
getOntology = function(inlist, ontology=c("MF", "BP", "CC")) {
   which = match.arg(ontology)
   onts = sapply(inlist, function(z) {
       if (!.isMissingGOEntry(z))
         z$Ontology
       else
         z
       })
   onts = onts[!is.na(onts)]
   unique(names(inlist[onts %in% which]))
}


##get GO evidence codes
getEvidence = function(inlist) {
    ans <- sapply(inlist, function(z) {
         if (!.isMissingGOEntry(z))
           z$Evidence
         else
           z
     })
    ans[!is.na(ans)]
}


##drop a specified set of evidence codes
dropECode = function(inlist, code = "IEA") {
    hasCode = sapply(inlist, function(z) {
        if (!.isMissingGOEntry(z))
          z$Evidence
        else
          z
        })
    hasCode <- hasCode[!is.na(hasCode)]
    badVals = hasCode %in% code
    inlist[!badVals]
}


## helper function, determines if there is a GO annotation for the
## desired mode
hasGOannote <- function(x, which="MF") {
    if (is(x, "GOTerms")) {
        cat <- Ontology(x)
        if (!is.na(cat) && cat == which)
          return(TRUE) else return(FALSE)
    }
    if (is.list(x)) {
        gT <- sapply(x, function(y) is(y, "GOTerms"))
        if (any(gT)) {
            if (all(gT)) {
                cats <- sapply(x, Ontology)
                return(cats == which)
            }
            else
              stop("mixed arguments not allowed")
        }
    }
    if (!is.character(x))
      stop("wrong argument")
    tm <- getGOOntology(x)
    return(tm == which)
}


##three functions to get all the GO information for a set of GO terms
##FIXME: these need to be renovated - probably removed even..
 getGOOntology <- function(x) {
     if( !is.character(x) )
         stop("need a character argument")
     if(length(x) == 0 )
         return( character(0))
     wh <- mget(x, envir=GOTERM, ifnotfound=NA)
     return( sapply(wh, Ontology) )
 }

 getGOParents <- function(x) {
     if( !is.character(x) )
         stop("need a character argument")
     if(length(x) == 0 )
         return(list())
     hasMF <- mget(x, envir=GOMFPARENTS, ifnotfound=NA)
     hasBP <- mget(x, envir=GOBPPARENTS, ifnotfound=NA)
     hasCC <- mget(x, envir=GOCCPARENTS, ifnotfound=NA)
     lenx <- length(x)
     rval <- vector("list", length=lenx)
     names(rval) <- x
     rval <- vector("list", length=lenx)
     names(rval) <- x
     for(i in 1:lenx) {
         if( (length(hasMF[[i]]) > 1 ) || !is.na(hasMF[[i]]) )
             rval[[i]] <- list(Ontology="MF", Parents=hasMF[[i]])
         else if( (length(hasMF[[i]]) > 1 ) || !is.na(hasBP[[i]]) )
             rval[[i]] <- list(Ontology="BP", Parents=hasBP[[i]])
         else if( (length(hasMF[[i]]) > 1 ) || !is.na(hasCC[[i]]) )
             rval[[i]] <- list(Ontology="CC", Parents=hasCC[[i]])
         else
             stop(paste(x[i], "is not a member of any ontology"))
     }
     return(rval)
 }

 getGOChildren <- function(x) {
     if( !is.character(x) )
         stop("need a character argument")
     if(length(x) == 0 )
         return(list())
     hasMF <- mget(x, envir=GOMFCHILDREN, ifnotfound=NA)
     hasBP <- mget(x, envir=GOBPCHILDREN, ifnotfound=NA)
     hasCC <- mget(x, envir=GOCCCHILDREN, ifnotfound=NA)
     lenx <- length(x)
     rval <- vector("list", length=lenx)
     names(rval) <- x
     for(i in 1:lenx) {
         if( (length(hasMF[[i]]) > 1 ) || !is.na(hasMF[[i]]) )
             rval[[i]] <- list(Ontology="MF", Children=hasMF[[i]])
         else if( (length(hasMF[[i]]) > 1 ) || !is.na(hasBP[[i]]) )
             rval[[i]] <- list(Ontology="BP", Children=hasBP[[i]])
         else if( (length(hasMF[[i]]) > 1 ) || !is.na(hasCC[[i]]) )
             rval[[i]] <- list(Ontology="CC", Children=hasCC[[i]])
         else
             rval[[i]] <- list()
     }
     return(rval)
 }

 getGOTerm <- function(x) {
     if( !is.character(x) )
         stop("need a character argument")
     if(length(x) == 0 )
         return(list())
     terms <- mget(x, envir=GOTERM, ifnotfound=NA)
     isNA = sapply(terms,function(x) !(isS4(x) && is(x, "GOTerms")))
     if( any(isNA) )
         terms = terms[!isNA]

     ontology <- sapply(terms, Ontology)
     terms = sapply(terms, Term)
     return(split(terms, ontology))
 }


filterGOByOntology <- function(goids, ontology=c("BP", "CC", "MF")) {
    ontology <- match.arg(ontology)
    eName <- switch(ontology,
                    BP="GOBPPARENTS",
                    CC="GOCCPARENTS",
                    MF="GOMFPARENTS",
                    stop("invalid ontology ", ontology))
    e <- get(eName)
    goids %in% ls(e)
}

aqListGOIDs <- function(ont) {
    ## Return all GO IDs in the specified ontologies
    ont <- unique(ont)
    knownOnts <- c("BP", "CC", "MF")
    badOnt <- ont[!(ont %in% knownOnts)]
    if (length(badOnt))
      stop("Unknown ontology codes: ", paste(badOnt, collapse=", "),
           "\nvalid codes are: ", paste(knownOnts, collapse=", "))
    ## determine size
    lens <- integer(length(ont))
    for (i in seq(along=ont))
      lens[i] <- length(getAnnMap(paste(ont[i], "PARENTS", sep=""),
                                  chip="GO"))
    ## retrieve IDs
    ans <- character(sum(lens))
    lens <- c(0L, lens)
    for (i in seq(along=ont)) {
        ans[lens[i]+1:lens[i+1]] <- ls(getAnnMap(paste(ont[i], "PARENTS", sep=""),
                                               chip="GO"))
    }
    ans
}
