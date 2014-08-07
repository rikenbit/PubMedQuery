##copyright 2004 R. Gentleman, all rights reserved

##given the name of chip compute the pathway adjacency matrix for LLids
PWAmat = function(data) {
    if(!is.character(data) || length(data) != 1 )
        stop("wrong argument")

    if( length(grep("^org\\..+\\.sgd$", data))>=1 ){ 
         dataE = getAnnMap("PATH2ORF", data, load=TRUE, type="db")
    }
    else if (  length(grep("^org\\..+\\.eg$", data))>=1 ){
         dataE = getAnnMap("PATH2EG", data, load=TRUE, type="db")
    }
    else {
         dataE = getAnnMap("PATH2PROBE", data, load=TRUE, type=c("db"))
    }

    if( data == "YEAST" ||  length(grep("^org\\..+", data))>=1 )
        pathLL = as.list(dataE)
    else {
        pathLL = eapply(dataE, function(x) {
            x = x[!is.na(x)]
            if(length(x)>0){
                LLs = getEG(x, data)
                LLs = LLs[!is.na(LLs)]
                unique(LLs) }
        })
    }
    uniqLL = unique(unlist(pathLL,use.names=FALSE))
    Amat = sapply(pathLL, function(x) {
        mtch = match(x, uniqLL)
        zeros = rep(0, length(uniqLL))
        zeros[mtch] = 1
        zeros})
    dimnames(Amat) = list(uniqLL, names(pathLL))
    return(Amat)
}


##given the name of chip compute the PubMed adjacency matrix for probe set ids
PMIDAmat = function(pkg, gene=NULL) {
    if(!is.character(pkg) || length(pkg) != 1 )
        stop("wrong argument")

    probe2pmid <- get(paste(pkg, "PMID", sep=""))
    if(is.null(gene)){
        gene2pmid <- as.list(probe2pmid)
    }else{
        if(any(duplicated(gene))) warning("Gene is not unique.")
        gene2pmid <- mget(unique(gene), probe2pmid, ifnotfound=NA)
    }
    pmid <- unique(unlist(gene2pmid))

    Amat <- sapply(gene2pmid,
                   function(x){
                       mtch <- match(x, pmid)
                       zeros <- rep(0, length(pmid))
                       zeros[mtch] <- 1
                       return(zeros)
                   }
                   )
    dimnames(Amat) = list(pmid, names(gene2pmid))
    return(Amat)
}


##given a GO term, and an exprset, produce a heatmap of all probes
##mapped to that GOterm;
GO2heatmap = function(x, eset, data, ...) {
    if( missing(data) )
        data = annotation(eset)
    mapE = get(paste(data, "GO2ALLPROBES", sep=""))

    whG = mapE[[x]]
    ##need this because there could be multiple criteria
    whG = unique(whG)
    whGs = whG[whG %in% featureNames(eset)]

    dataM = exprs(eset)[whGs,]
    heatmap(dataM, ...)
}

GOmnplot = function (x, eset, data = "hgu133plus2", group, ...)
{
    mapE = get(paste(data, "GO2ALLPROBES", sep = ""))
    whG = mapE[[x]]
    whG = unique(whG)
    whGs = whG[whG %in% featureNames(eset)]
    dataM = exprs(eset)[whGs, ]
    tts = apply(dataM, 1, function(x) sapply(split(x, group), mean))
    rn = row.names(tts)
    if( length(levels(factor(group))) != 2 )
        stop("only works for factors with two levels")
    plot(tts[1,], tts[2,], xlab=rn[1], ylab=rn[2], ...)
    abline(a=0, b=1)
    return(tts)
}

p2LL = function(data) {
    .Defunct("p2LL", package="annotate")
}

setGeneric("KEGG2heatmap", function(x, eset, data, ...) 
                             standardGeneric("KEGG2heatmap"))

setMethod("KEGG2heatmap", c("character", "eSet", "character"),
   function(x, eset, data, ...) {   
    if( missing(data) )
       data = annotation(eset)
    mapE = get(paste(data, "PATH2PROBE", sep = ""))
    whG = mapE[[x]]
    whG = unique(whG)
    whGs = whG[whG %in% featureNames(eset)]
    dataM = exprs(eset)[whGs, ] 
    heatmap(dataM, ...)
})

setMethod("KEGG2heatmap", c("character", "matrix", "character"),
   function(x, eset, data, ...) {
    mapE = get(paste(data, "PATH2PROBE", sep = ""))
    whG = mapE[[x]] 
    whG = unique(whG)
    whGs = whG[whG %in% row.names(eset)]
    dataM = eset[whGs, ]
    heatmap(dataM, ...)
})


setGeneric("KEGGmnplot", function(x, eset, data= "hgu133plus2", group,
                               ...) standardGeneric("KEGGmnplot"))

setMethod("KEGGmnplot",  c("character", "eSet", "character"),
    function (x, eset, data = "hgu133plus2", group, ...)  {
      mapE = get(paste(data, "PATH2PROBE", sep = ""))
      whG = mapE[[x]]
      whG = unique(whG)
      whGs = whG[whG %in% featureNames(eset)]
      dataM = exprs(eset)[whGs, ]
      tts = apply(dataM, 1, function(x) sapply(split(x, group), mean))
      rn = row.names(tts)
      if( length(levels(factor(group))) != 2 )
          stop("only works for factors with two levels")
      plot(tts[1,], tts[2,], xlab=rn[1], ylab=rn[2], ...)
      abline(a=0, b=1)
      return(tts)
  })

setMethod("KEGGmnplot",  c("character", "matrix", "character"),
    function (x, eset, data = "hgu133plus2", group, ...)  {
      mapE = get(paste(data, "PATH2PROBE", sep = ""))
      whG = mapE[[x]]
      whG = unique(whG)
      whGs = whG[whG %in% row.names(eset)]
      dataM = eset[whGs, ]
      tts = apply(dataM, 1, function(x) sapply(split(x, group), mean))
      rn = row.names(tts)
      if( length(levels(factor(group))) != 2 )
          stop("only works for factors with two levels")
      plot(tts[1,], tts[2,], xlab=rn[1], ylab=rn[2], ...)
      abline(a=0, b=1)
      return(tts)
  })


