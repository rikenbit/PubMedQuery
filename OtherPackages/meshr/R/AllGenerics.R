##
## Generic functions 
##

## This is already defiend in Category/R/AllGenerics.R but seems like required.
## Probably because this is not listed in exportMethods() of Category/NAMESPACE?
setGeneric("universeGeneIds<-",
           function(r, value) standardGeneric("universeGeneIds<-"))

setGeneric("universeGeneIds", function(r) standardGeneric("universeGeneIds"))

setGeneric("pAdjust", function(r) standardGeneric("pAdjust"))

setGeneric("meshIds", function(r) standardGeneric("meshIds"))

setGeneric("meshTerms", function(r) standardGeneric("meshTerms"))

setGeneric("meshCategory", function(r) standardGeneric("meshCategory"))

setGeneric("meshAnnotation", function(r) standardGeneric("meshAnnotation"))

setGeneric("category", function(r) r@category)
setGeneric("category<-", 
    function(r, value) standardGeneric("category<-")
)

setGeneric("database", function(r) r@database)
setGeneric("database<-", 
    function(r, value) standardGeneric("database<-")
)

setGeneric("meshDatabase", function(r) standardGeneric("meshDatabase"))

setGeneric("meshHyperGTest", 
           function(p) standardGeneric("meshHyperGTest"),
           valueClass="MeSHHyperGResult")


