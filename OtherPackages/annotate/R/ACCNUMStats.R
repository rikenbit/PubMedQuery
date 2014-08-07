ACCNUMStats <- function(pkgName){
    accMap <- getAnnMap("ACCNUM", pkgName)
    accs <- as.list(accMap)
    return(table(unlist(sapply(accs, whatACC))))
}

whatACC <- function(accs){
    if(is.na(accs[1])){
        return("NA")
    }
    accs <- strsplit(accs, ";")
    if(regexpr("^[a-zA-Z]{2}\\.[0-9]+$", accs[1]) > 0){
        return("UniGene")
    }
    if(regexpr("^(NP_)|(NG_)|(NM_)|(NC_)|(XR_)|(XM_)|(XP_)[0-9]+[._]?[0-9]?$",
               accs[1]) > 0){
        return("RefSeq")
    }
    if(regexpr("^[A-Z]+[0-9]+[._]?[0-9]$", accs[1]) > 0){
        return("GBAcc")
    }
    if(regexpr("^[0-9]+$", accs[1]) > 0){
        return("Image")
    }

    return("Unknown")
}

