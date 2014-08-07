# Checks the DESCRIPTION file to see if the packages whose names are
# passed have the same version number

compatibleVersions <- function(...){
    pkgs <- list(...)
    versions <- NULL
    for(i in pkgs){
        options(show.error.messages = FALSE, warn = -1)
        versions <- try(c(versions, packageDescription(i)[["Version"]]))
        options(show.error.messages = TRUE, warn = 0)

        if(inherits(versions, "try-error")){
            stop(paste("Package", i, "is not in the library"))
        }
    }

    if(length(unique(versions)) == 1){
        return(TRUE)
    }else{
        return(FALSE)
    }
}
