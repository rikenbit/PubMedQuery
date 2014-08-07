formatGeneReportFull = function (result) 
{
	####  Changelog: 2010-05-09: 
	#### 	-Coping with a change in DAVIDQuery API returnvalue format that broke this function.
	####	-Formatting of the featureNamesComplex components is improved: parsed into a list.
	
	resultNames = unlist(result[1, ])
	result = result[-1, ]
	names(result) = resultNames
    ids <- strsplit(attr(result, "ids"), split = ",")[[1]]
    annot <- attr(result, "annot")
    type <- attr(result, "type")
    idRows <- match(ids, result[, 1])
    names(idRows) = ids
    formatted =  list()
    featureNames = resultNames[-1]
    featureNamesComplex = c("GENERIF_SUMMARY", "SP_COMMENT")
    featureNamesSimple = setdiff(featureNames, featureNamesComplex)
    for(id in ids) {

    	formatted[[id]] = strsplit(unlist(result[idRows[id], featureNamesSimple, drop=TRUE]), split=",")
    	names(formatted[[id]]) = featureNamesSimple
    	for(feature in featureNamesComplex) {
    		thisFeature = result[idRows[id], feature]
    		g.out = gregexpr("[^,]+:", thisFeature)[[1]]
    		g.names = (substring(thisFeature, g.out, 
    					g.out + attr(g.out, "match.length") - 2))
    		g.values = (substring(thisFeature, 
    					g.out + attr(g.out, "match.length"), 
    					c(g.out[-1] - 2, nchar(thisFeature) - 1)))
    		names(g.values) = g.names
    		formatted[[id]][[feature]] = as.list(g.values)
#    		if(!is.null(formatted[[id]][[thisFeature]]))
#    			names(formatted[[id]][[thisFeature]]) = g.names
    	}
    }
    names(formatted) = ids
    return(formatted)
}
