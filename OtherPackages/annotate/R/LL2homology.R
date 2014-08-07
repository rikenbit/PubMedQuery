LL2homology <- function(homoPkg, llids){
    warning("This function has been DEPRECATED.")
    if(!require(homoPkg, character.only = TRUE))
                  stop("Package homology not available!")

    hgids <- mget(as.character(llids), envir = get(paste(homoPkg,
                                    "LL2HGID", sep = "")), ifnotfound = NA)

    #if(length(hgids) == 1){
    #    return(HGID2homology(hgids[[1]]))
    #}

    return(sapply(hgids, HGID2homology, homoPkg = homoPkg))
}

ACC2homology <- function(accs, homoPkg){
    warning("This function has been DEPRECATED.")
    if(!require(homoPkg, character.only = TRUE))
        stop(paste("Package", homoPkg, "not available!"))

    hgids <- mget(as.character(accs), envir = get(paste(homoPkg, "ACC2HGID",
                  sep = ""), pos = match(paste("package:", homoPkg, sep = ""),
                         search())), ifnotfound = NA)
    return(sapply(hgids, HGID2homology, homoPkg))
}

HGID2homology <- function(hgid, homoPkg){
    warning("This function has been DEPRECATED.")
    homoGenes <- list()

    #  hgid may be of length greater than 1 as a LL id may be mapped to
    # more than 2 HGIDs
    for(i in hgid){
        options(show.error.messages = FALSE)
        tryMe <- try(get(as.character(i),
                         envir = get(paste(homoPkg, "DATA", sep = ""),
                         pos = match(paste("package:", homoPkg, sep = ""),
                         search()))))
        options(show.error.messages = TRUE)
        if(!inherits(tryMe, "try-error")){
            homoGenes[[length(homoGenes) + 1]] <- tryMe
        }
    }
    return(homoGenes)
}
