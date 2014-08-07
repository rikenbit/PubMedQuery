# This function takes a character string for the name of a platform
# specific annotation data package and a character string for an
# environment in the data directory of the package and then returns a
# list of vectors with ids of a given environment as names and vectors
# of character strings for names of probes associated with the
# environment ids as values.
#
# baseName - a character string for the name of a platform specific
# annotation data package (e. g. for an Affymetrix chip).
# what - a character string for the name of an environment for which a
# reverse mapping is sought (e. g. "ENTREZID").
#
# Copyright 2004, Jianhua Zhang. All rights reserved.
#

probesByLL <- function(baseName, what = "ENTREZID"){
    tempList <- list()

    mergeRowByKey <- function (mergeMe, keyCol = 1, sep = ";"){
        mergeTwoCol <- function(x){
            return(paste(unique(x[, -keyCol]), sep = "", collapse = ";"))
        }
        mergeMultiCol <- function(x){
            return(apply(x, 2, function(y)
                         paste(unique(y), sep = "", collapse = ";")))
        }
        # Returns the original value if mergeMe is a vector
        if(is.null(ncol(mergeMe))){
            return(mergeMe)
        }
        merged <- split.data.frame(mergeMe, factor(mergeMe[, keyCol]))
        if(ncol(mergeMe) == 2){
            merged <- sapply(merged, mergeTwoCol)
            merged <- cbind(names(merged), merged)
            colnames(merged) <- c(colnames(mergeMe)[keyCol],
                                  colnames(mergeMe)[-keyCol])
            return(merged)

        }else{
            merged <- sapply(merged, mergeMultiCol)
            merged <- t(merged)
            colnames(merged) <- colnames(mergeMe)
            return(merged)
        }
    }

    splitEntry <- function(dataRow, sep = ";", asNumeric = FALSE){
        if(is.na(dataRow) || is.null(dataRow) || dataRow == ""){
            return(NA)
        }else{
            if(asNumeric){
                return(as.numeric(unlist(strsplit(dataRow, sep))))
            }else{
                return(unlist(strsplit(dataRow, sep)))
            }
        }
    }

    options(show.error.messages = FALSE)
    temp <- try(as.list(getAnnMap(what, baseName)))
    options(show.error.messages = TRUE)
    if(inherits(temp, "try-error")){
        stop(paste(what, "may not be a valid environment of", baseName))
    }else{
        temp <- temp[!is.na(temp)]
        temp <- sapply(temp, paste, sep = "", collapse = ";")
        temp <- cbind(names(temp), as.vector(temp))
        if(!is.vector(temp[[1]])){
            stop(paste("Values of", what, "are not vectors"))
        }else{
            if(nrow(temp) == 0){
                return(c(temp[2], temp[1]))
            }
            seped <- sapply(temp[,2], splitEntry)
            reps <- sapply(seped, length)
            data <- cbind(rep(temp[,1], reps),
                          unlist(seped, use.names = FALSE))
            data <- mergeRowByKey(data, keyCol = 2)
            tempList[data[,1]] <- data[,2]
            return(sapply(tempList, splitEntry))
        }
    }
}
