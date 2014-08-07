annObjPrefix <- function(name) {
    if (length(grep("\\.db$", name)))
      substr(name, 1, nchar(name) - 3L)
    else
      name
}

annPkgName <- function(name, type=c("db", "env")) {
    type <- match.arg(type)
    if (length(grep("\\.db$", name)))
      if (type == "db")
        name
      else
        substr(name, 1, nchar(name) - 3L)
    else if (type == "db")
      paste(name, ".db", sep="")
    else
      name
}

## For cases where there is not a Bimap, but where there is an AnnoationDb
## object with a cols() value that matches the map argument, we want getAnnMap
## to spawn up a FlatBimap object and return that.

getAnnMap <- function(map, chip, load=TRUE, type=c("db", "env")) {
    typeMissed <- FALSE
    searchName <- NULL
    if (missing(type)) {
        typeMissed <- TRUE
        searchNames <- paste("package:", chip, c("", ".db"), sep="")
        searchPth <- search()
        whLoaded <- match(searchNames, searchPth)
        whLoaded <- whLoaded[!is.na(whLoaded)]
        if (length(whLoaded))
          searchName <- searchPth[sort(whLoaded)][1]
    } else {
        badTypes <- type[!(type %in% c("db", "env"))]
        if (length(badTypes))
          stop("unknown types in 'type' argument: ",
               paste(badTypes, collapse=", "))
    }
    pkg <- annPkgName(name=chip, type=type[1])
    if (is.null(searchName))
      searchName <- paste("package", pkg, sep=":")
    pkgEnv <- tryCatch(as.environment(searchName), error=function(e) {
        if (load) {
            ok <-
              suppressWarnings(require(pkg, character.only=TRUE,
                                       quietly=TRUE))
            if (!ok && length(type) > 1) {
                origPkg <- pkg
                for (t in type[2:length(type)]) {
                    pkg <- annPkgName(name=chip, type=t)
                    searchName <- paste("package", pkg, sep=":")
                    if (suppressWarnings(require(pkg, character.only=TRUE,
                                                 quietly=TRUE))) {
                        if (!typeMissed)
                          warning("getAnnMap: ", "package ", origPkg,
                                  " not available, ", "using ", pkg, " instead",
                                  call.=FALSE)
                        ok <- TRUE
                        break
                    }
                }
            }
            if (!ok)
              stop("getAnnMap: ", "package ", pkg, " not available",
                   call.=FALSE)
            as.environment(searchName)
        } else {
            stop("getAnnMap: ", pkg, " package not attached and load is FALSE",
                 call.=FALSE)
        }
    })
    mapName <- paste(annObjPrefix(chip), map, sep="")
    if(exists(mapName, envir=pkgEnv, inherits=FALSE)){
      return( get(mapName, envir=pkgEnv, inherits=FALSE) ) 
    }else{
      ## chip will be a character, but we need to make it into a real thing.
      ## spawn up a new FlatBimap
      db <- eval(parse(text=chip))
      if(map %in% columns(db)){ ## if cols says its present
           return(AnnotationDbi:::makeFlatBimapUsingSelect(db,
                                                           col=map))
      }
    }
}



