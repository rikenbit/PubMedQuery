##
## Accessor methods for MeSHHyperGResult class 
##

setMethod("meshCategory", "MeSHHyperGResult", function(r) r@meshCategory)

setMethod("meshAnnotation", "MeSHHyperGResult", function(r) r@meshAnnotation)

setMethod("meshDatabase", "MeSHHyperGResult", function(r) r@meshDatabase)

setMethod("meshIds", "MeSHHyperGResult", function(r) r@meshIds)

setMethod("meshTerms", "MeSHHyperGResult", function(r) r@meshTerms)

## generic is defined in Category/R/AllGenerics.R
setMethod("pvalues", "MeSHHyperGResult", function(r) r@pvalues)

## generic is defined in methods/R/AllGenerics.R
setMethod("show", "MeSHHyperGResult", function(object){
cat("MeSH enrichment analysis for category", object@meshCategory, '\n')
cat("Annotation package used: ", object@meshAnnotation, '\n')
cat("The correspondance is retrived from: ", object@meshDatabase, '\n')
cat("Number of MeSH terms identified: ", length(object@meshIds), '\n')
})

## generic is defined in base/R/AllGenerics.R
setMethod("summary", "MeSHHyperGResult", function(object){
  mesh.df <- data.frame(MESHID = object@meshIds, MESHTERM = object@meshTerms, PVALUE = object@pvalues)
  mesh.df
})

