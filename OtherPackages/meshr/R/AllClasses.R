##
## Two classes defiend in meshr package 
##

## Class to holds parameters that will be passed on to statistical test 
setClass("MeSHHyperGParams",
        representation=representation(
          geneIds="ANY",
          universeGeneIds="ANY",
          annotation="character",
          category="character",
          database="character",
          pvalueCutoff="numeric",
          pAdjust="character"),
        prototype=prototype(
          pvalueCutoff=0.05,
          pAdjust="none"
          )
        )

## Class to holds MeSH enrichment analysis results 
setClass("MeSHHyperGResult",
        representation=representation(
          meshCategory="character",
          meshAnnotation="character",
          meshDatabase="character",          
          meshIds="character",
          meshTerms="character",
          pvalues="numeric"
          )
        )

