### homoData objects are used by homoPkgBuilder to represent homology data

setClass("homoData", representation(homoOrg = "character",
                                    homoLL = "numeric",
                                    homoType = "character",
                                    homoPS = "numeric",
                                    homoURL = "character",
                                    homoACC = "character",
                                    homoHGID = "numeric"))

# Set the get methods
setGeneric("homoOrg",
               function(object) standardGeneric("homoOrg"))

setMethod("homoOrg", "homoData",
          function(object) object@homoOrg)

setGeneric("homoLL",
               function(object) standardGeneric("homoLL"))

setMethod("homoLL", "homoData",
          function(object) object@homoLL)

setGeneric("homoType",
               function(object) standardGeneric("homoType"))

setMethod("homoType", "homoData",
          function(object) object@homoType)

setGeneric("homoPS",
               function(object) standardGeneric("homoPS"))

setMethod("homoPS", "homoData",
          function(object) object@homoPS)

setGeneric("homoURL",
               function(object) standardGeneric("homoURL"))

setMethod("homoURL", "homoData",
          function(object) object@homoURL)

setGeneric("homoACC",
               function(object) standardGeneric("homoACC"))

setMethod("homoACC", "homoData",
          function(object) object@homoACC)

setGeneric("homoHGID",
               function(object) standardGeneric("homoHGID"))

setMethod("homoHGID", "homoData",
          function(object) object@homoHGID)

setMethod("show", "homoData",
          function(object) {
              if(length(homoOrg(object)) > 0 && !is.na(homoOrg(object))){
                  cat(paste("homoOrg:", homoOrg(object)), fill = TRUE)
              }
              if(length(homoLL(object)) > 0 && !is.na(homoLL(object))){
                  cat(paste("\nhomoLL:", homoLL(object)), fill = TRUE)
              }
              if(length(homoHGID(object)) > 0 && !is.na(homoHGID(object))){
                  cat(paste("\nhomoHGID:", homoHGID(object)), fill = TRUE)
              }
              if(length(homoACC(object)) > 0 && !is.na(homoACC(object))){
                  cat(paste("\nhomoACC:", homoACC(object)), fill = TRUE)
              }
              if(length(homoType(object)) > 0 && !is.na(homoType(object))){
                  cat(paste("\nhomoType:", homoType(object)), fill = TRUE)
              }
              if(length(homoPS(object)) > 0 && !is.na(homoPS(object))){
                  cat(paste("\nhomoPS:", homoPS(object)), fill = TRUE)
              }
              if(length(homoURL(object)) > 0 && !is.na(homoURL(object))){
                  cat(paste("\nhomoURL:", homoURL(object)), fill = TRUE)
              }
              cat("\n")
})

mapOrgs <- function(toMap, what = c("code", "name")){
    fun <- function(x){
        if(what == "code"){
            return(orgs[[x]])
        }else{
            return(names(orgs[orgs == x]))
        }
    }
    what <- match.arg(what)
    orgs <- getOrgNameNCode()
    if(is.null(toMap) || is.na(toMap)){
         return(NA)
    }
    if(length(toMap) == 1){
        return(fun(toMap))
    }else{
        return(sapply(toMap, fun))
    }
}

getOrgNameNCode <- function(){
    return(list("3055" = "Chlamydomonas reinhardtii",
             "3702" = "Arabidopsis thaliana",
             "3847" = "Glycine max",
             "3880" = "Medicago truncatula",
             "4081" = "Lycopersicon esculentum",
             "4513" = "Hordeum vulgare",
             "4530" = "Oryza sativa",
             "4565" = "Triticum aestivum",
             "4577" = "Zea mays",
             "4896" = "Schizosaccharomyces pombe",
             "4932" = "Saccharomyces cerevisiae",
             "5141" = "Neurospora crassa",
             "5833" = "Plasmodium falciparum",
             "6239" = "Caenorhabditis elegans",
             "7165" = "Anopheles gambiae",
             "7227" = "Drosophila melanogaster",
             "7719" = "Ciona intestinalis",
             "7955" = "Danio rerio",
             "8022" = "Oncorhynchus mykiss",
             "8090" = "Oryzias latipes",
             "8355" = "Xenopus laevis",
             "8364" = "Xenopus tropicalis",
             "9031" = "Gallus gallus",
             "9606" = "Homo sapiens",
             "9615" = "Canis familiaris",
             "9598" = "Pan troglodytes",
             "9823" = "Sus scrofa",
             "9913" = "Bos taurus",
             "10090" = "Mus musculus",
             "10116" = "Rattus norvegicus",
             "28985" = "Kluyveromyces, lactis",
             "29760" = "Vitis vinifera",
             "33169" = "Eremothecium gossypii",
             "44689" = "Dictyostelium discoideum",
             "148305" = "Magnaporthe grisea"
            ))
}


homoData <- function(organism, LL, type, PS, ACC, HGID, URL){
    return(new("homoData", homoOrg = mapOrgs(organism),
                   homoLL = LL, homoType = type,
                   homoPS = PS, homoURL = URL,
                   homoACC = ACC, homoHGID = HGID))
}
