    # Defines the chromLocation class


    # Define the class structure of the chromLocation object
## FIXME: we make the slots probesToChrom and geneSymbols ANY because
## they should be able to hold either an environment or an object from
## AnnotationDbi.  We could use a class union here, but they could have
## other consequences for dispatch.
    setClass("chromLocation", representation(organism="character",
                                             dataSource="character",
                                             chromLocs="list",
                                             probesToChrom="ANY",
                                             chromInfo="numeric",
                                             geneSymbols="ANY"
                                             ))

    # Define the accessors
##     if (is.null(getGeneric("organism")))
##         setGeneric("organism", function(object)
##                    standardGeneric("organism"))

    setMethod("organism", "chromLocation", function(object)
              object@organism)

    setMethod("organism", "character", function(object)
              get(paste(object,"ORGANISM",sep="")))


    if (is.null(getGeneric("dataSource")))
        setGeneric("dataSource", function(object)
                   standardGeneric("dataSource"))

    setMethod("dataSource", "chromLocation", function(object)
              object@dataSource)

    if (is.null(getGeneric("nChrom")))
        setGeneric("nChrom", function(object)
                   standardGeneric("nChrom"))

    setMethod("nChrom", "chromLocation", function(object)
              length(object@chromInfo))

    if (is.null(getGeneric("chromNames")))
        setGeneric("chromNames", function(object)
                   standardGeneric("chromNames"))

    setMethod("chromNames", "chromLocation", function(object)
              names(object@chromInfo))

    if (is.null(getGeneric("chromLocs")))
        setGeneric("chromLocs", function(object)
                   standardGeneric("chromLocs"))

    setMethod("chromLocs", "chromLocation", function(object)
              object@chromLocs)

    if (is.null(getGeneric("chromLengths")))
        setGeneric("chromLengths", function(object)
                   standardGeneric("chromLengths"))

    setMethod("chromLengths", "chromLocation", function(object) {
        z <- as.numeric(object@chromInfo)
        ## Unknown chromosome lengths come out as NA from the
        ## data package, put this as 0 as we want a numeric vector
        z[is.na(z)] <- 0
        z
    })

    if (is.null(getGeneric("probesToChrom")))
        setGeneric("probesToChrom", function(object)
                   standardGeneric("probesToChrom"))

    setMethod("probesToChrom", "chromLocation", function(object)
              object@probesToChrom)

    if (is.null(getGeneric("chromInfo")))
        setGeneric("chromInfo", function(object)
                   standardGeneric("chromInfo"))
    setMethod("chromInfo", "chromLocation", function(object)
              object@chromInfo)

    if (is.null(getGeneric("geneSymbols")))
        setGeneric("geneSymbols", function(object)
                   standardGeneric("geneSymbols"))
    setMethod("geneSymbols", "chromLocation", function(object)
              object@geneSymbols)

    setMethod("show", "chromLocation", function(object) {
        cat("Instance of a chromLocation class with the following fields:\n")
        cat("\tOrganism: ", organism(object), "\n\t")
        cat("Data source: ", dataSource(object), "\n\t")
        cat("Number of chromosomes for this organism: ", nChrom(object), "\n\t")

        ## Build up a matrix of chromosome names & their locations
        cat("Chromosomes of this organism and their lengths in base pairs:")
        cNames <- chromNames(object)
        cLens <- chromLengths(object)
        for (i in 1:nChrom(object)) {
            cat("\n\t\t",cNames[i],":",cLens[i])
        }
        cat("\n")
    })


buildChromLocation <- function(dataPkg) {
    ##takes an environment/hash table with the chrom locations and
    ##named list, one element for each distinct chromosome name and
    ##each element of that list is a named vector, the names are the
    ##probeids and the values are the locations
    CHRLOC2chromLoc <- function(chrEnv) {
        chrLocs <- as.list(chrEnv)

        ## Need to extract out the ones w/ multiple mappings
        chrLens <- sapply(chrLocs, length)
        multis <- split(chrLens, factor(chrLens))

        ## First handle the single mapped genes
        singleNames <- names(multis$"1")
        singleLocs <- chrLocs[singleNames]
        chromNames <- unlist(sapply(singleLocs, function(z) {
            if (is.na(z))
                z
            else
                names(z)
        }))
        chromNames <- factor(chromNames)
        a <- split(singleLocs, chromNames)
        chrLocList <- lapply(a, function(x) {g <- unlist(lapply(x, function(z)
                                                            {names(z) <- NULL;
                                                             z})); g})

        ## Now handle the multi mapped genes
        ## !!! FIXME:
        ## !!! This is *very* inefficient.  Make this better
        ## !!!
        if (length(multis) > 1) {
            for (i in 2:length(multis)) {
                curNames <- names(multis[[i]])
                curLocs <- chrLocs[curNames]
                for (j in 1:length(curLocs)) {
                    curGene <- curLocs[[j]]
                    curGeneChroms <- names(curGene)
                    names(curGene) <- rep(curNames[j],length(curGene))
                    for (k in 1:length(curGene))
                        chrLocList[[curGeneChroms[k]]] <-
                            c(chrLocList[[curGeneChroms[k]]], curGene[k])
                }
            }
        }


        chrLocList
    }


    chrlocEnv <- getAnnMap("CHRLOC", dataPkg)
    chrLocList <- CHRLOC2chromLoc(chrlocEnv)

    ## !!! Need to get the version info for dataSource
    newCC <- new("chromLocation",
                 organism=getAnnMap("ORGANISM", dataPkg),
                 dataSource=dataPkg,
                 chromLocs=chrLocList,
                 chromInfo=getAnnMap("CHRLENGTHS", dataPkg),
                 probesToChrom=getAnnMap("CHR", dataPkg),
                 geneSymbols=getAnnMap("SYMBOL", dataPkg))

    return(newCC)
}

usedChromGenes <- function(eSet, chrom, specChrom) {
    ## Passed an instance of an eSet, a chromosome name, and
    ## an instance of a chromLocation object, this function will return the
    ## set of genes in eSet that exist on the named chromosome,
    ## ordered by location

    ## Extract the gene names of the chromosome of interest
    cLocs <- chromLocs(specChrom)
    genes <- cLocs[[chrom]]

    ## Extract out of the expr set the genes that belong on this chrom
    usedGenes <- genes[names(genes) %in% featureNames(eSet)]

    ## Order the genes by location
    ord <- order(abs(usedGenes))
    usedGenes <- as.list(usedGenes[ord])

    return(usedGenes)
}

##############
# manipulate the chromosome locations so that all of the more general terms
# are also included for a chromosome location
# for example: a gene located at 14q11 would be also located at 14, 14q,
# 14q1, and 14q11
##############
chrCats<-function(data)
{
  chrEnv<-paste(data, "MAP", sep="")
  xx<-as.list(eval(as.name(chrEnv)))

  # first need to have only one location per Affy id
  # so if the length is greater than 1, take the first location
  # only 9 Affy ids are located at more than one place for hgu95av2MAP

  # find out which genes have more than one location
  xxLen<-unlist(lapply(xx, function(x) 
                        {
                          if (any(is.na(x)))
                            return(0)
                          else
                            return(length(x))
                        }))
 
  affyIdsWithTwoOrMoreLocs<-which(xxLen > 1)
  if (length(affyIdsWithTwoOrMoreLocs) > 0)
    for (i in 1:length(affyIdsWithTwoOrMoreLocs))
    {
      # only use the first location
      xx[affyIdsWithTwoOrMoreLocs[i]]<-xx[[affyIdsWithTwoOrMoreLocs[i]]][1]
    }

  # now each element has 0 or 1 locations
  
  # next need to remove any leading spaces
  # also remove any text after the first space (if it's not a leading space)
  spaces<-grep(" ", unlist(xx))
  if (length(spaces) > 0)
  {
    for (i in 1:length(spaces))
    {
      pieces<-unlist(strsplit(xx[[spaces[i]]], " "))
      # then have a leading space
      if (pieces[1] == "")
      {
        firstNonSpace<-0
        # may have more than one leading space
        for (j in 1:length(pieces))
        {
          if (pieces[j]!="" && firstNonSpace==0)
            firstNonSpace<-j
        }
        pieces<-pieces[firstNonSpace:length(pieces)]
      }

      # now check if there are any spaces elsewhere in the text
      if (length(pieces) > 1)
      {
        # if the length is greater than 1, then there are spaces elsewhere
        # just take the text up to the first space
        xx[spaces[i]]<-pieces[1]
      }
      else
      {
        # only had leading spaces
        xx[spaces[i]]<-pieces[1]
      }
    }
  }

  # now have got rid of spaces - next look for other characters

  # look at one at a time
  yy<-list()
  for (i in 1:length(xx))
  {
    if (is.na(xx[i]))
    {
      yy[[i]]<-NA
    }
    else
    {
      strToManip<-xx[[i]]

      if (length(grep("|", strToManip, fixed=T)) > 0)
      {
        strToManip<-unlist(strsplit(strToManip, "|", fixed=T))
      }

      if (length(grep("-", strToManip)) > 0)
      {
        # for the second element need to include the chromosome number
        if (length(grep("-", strToManip)) == 1)
        {
          tempToManip<-unlist(strsplit(strToManip[grep("-", strToManip)], "-"))
          # need to add the chromosome number to the second element
          if (length(grep("q", tempToManip[1])) > 0)
          {
            splitonQ<-unlist(strsplit(tempToManip[1], "q"))
            chrNo<-splitonQ[1]
            if (length(grep("^[qpc]", tempToManip[2])) > 0)
              tempToManip[2]<-paste(chrNo, tempToManip[2], sep="")
            else
              tempToManip[2]<-paste(chrNo, "q", tempToManip[2], sep="")

            # now need to check for all the bands in between these two bands
            if (nchar(tempToManip[1])==nchar(tempToManip[2]))
            {
              curnch<-nchar(tempToManip[1])
              # check if all the characters match up until the last
              #  character
              if (substr(tempToManip[1], 1, curnch-1)==
                   substr(tempToManip[2], 1, curnch-1))
              {
                # then need to include all bands in between
                twoVals<-c(substr(tempToManip[1], curnch, curnch),
                            substr(tempToManip[2], curnch, curnch))
                # check that the 2 characters are actually numeric
                if (!any(is.na(as.numeric(twoVals))))
                {
                  minVal<-min(as.numeric(twoVals))
                  maxVal<-max(as.numeric(twoVals))
                  if (minVal+1 != maxVal)
                  {
                    for (k in (minVal+1):(maxVal-1))
                    {
                      tempToManip[(k-minVal)+2]<-
                        paste(substr(tempToManip[1], 1, curnch-1), k, sep="")
                    }
                  }
                }
              }
            }
          }
          else
          {
            if (length(grep("p", tempToManip[1])) > 0)
            {
              splitonP<-unlist(strsplit(tempToManip[1], "p"))
              chrNo<-splitonP[1]
              if (length(grep("^[qpc]", tempToManip[2])) > 0)
                tempToManip[2]<-paste(chrNo, tempToManip[2], sep="")
              else
                tempToManip[2]<-paste(chrNo, "p", tempToManip[2], sep="")

              # now need to check for all the bands in between these two bands
              if (nchar(tempToManip[1])==nchar(tempToManip[2]))
              {
                curnch<-nchar(tempToManip[1])
                # check if all the characters match up until the last
                #  character
                if (substr(tempToManip[1], 1, curnch-1)==
                     substr(tempToManip[2], 1, curnch-1))
                {
                  # then need to include all bands in between
                  twoVals<-c(substr(tempToManip[1], curnch, curnch),
                              substr(tempToManip[2], curnch, curnch))
                  # check that the 2 characters are actually numeric
                  if (!any(is.na(as.numeric(twoVals))))
                  {
                    minVal<-min(as.numeric(twoVals))
                    maxVal<-max(as.numeric(twoVals))
                    if (minVal+1 != maxVal)
                    {
                      for (k in (minVal+1):(maxVal-1))
                      {
                        tempToManip[(k-minVal)+2]<-
                          paste(substr(tempToManip[1], 1, curnch-1), k, sep="")
                      }
                    }
                  }
                }
              }
            }
            else
            {
              if (length(grep("cen", tempToManip[1])) > 0)
              {
                splitonCen<-unlist(strsplit(tempToManip[1], "cen"))
                chrNo<-splitonCen[1]
                tempToManip[2]<-paste(chrNo, tempToManip[2], sep="")

                # can't check for between values because I don't know how
                # many bands there are until it hits the centromere
              }
              else
              {
                print(paste("There is no p, q, or cen to split on in iteration", i))
                print("This is not expected!")
              }
            }
          }
          # now add everything back together into strToManip
          strToManip<-c(strToManip, tempToManip)
          strToManip<-strToManip[-grep("-", strToManip)]
        }

        else
        {
          for (j in 1:length(grep("-", strToManip)))
          {
            tempToManip<-unlist(strsplit(strToManip[grep("-", 
                                                       strToManip)[j]], "-"))
            if (length(grep("q", tempToManip)) > 0)
            {
              splitonQ<-unlist(strsplit(tempToManip[1], "q"))
              chrNo<-splitonQ[1]
              tempToManip[2]<-paste(chrNo, tempToManip[2], sep="")

              # now need to check for all the bands in between these two bands
              if (nchar(tempToManip[1])==nchar(tempToManip[2]))
              {
                curnch<-nchar(tempToManip[1])
                # check if all the characters match up until the last
                #  character
               if (substr(tempToManip[1], 1, curnch-1)==
                    substr(tempToManip[2], 1, curnch-1))
                {
                  # then need to include all bands in between
                  twoVals<-c(substr(tempToManip[1], curnch, curnch),
                              substr(tempToManip[2], curnch, curnch))
                  # check that the 2 characters are actually numeric
                  if (!any(is.na(as.numeric(twoVals))))
                  {
                    minVal<-min(as.numeric(twoVals))
                    maxVal<-max(as.numeric(twoVals))
                    if (minVal+1 != maxVal)
                    {
                      for (k in (minVal+1):(maxVal-1))
                      {
                        tempToManip[(k-minVal)+2]<-
                          paste(substr(tempToManip[1], 1, curnch-1), k, sep="")
                      }
                    }
                  }
                }
              }
            }
            else
            {
              if (length(grep("p", tempToManip)) > 0)
              {
                splitonP<-unlist(strsplit(tempToManip[1], "p"))
                chrNo<-splitonP[1]
                tempToManip[2]<-paste(chrNo, tempToManip[2], sep="")

                # now need to check for all the bands in between these two 
                # bands
                if (nchar(tempToManip[1])==nchar(tempToManip[2]))
                {
                  curnch<-nchar(tempToManip[1])
                  # check if all the characters match up until the last
                  #  character
                  if (substr(tempToManip[1], 1, curnch-1)==
                      substr(tempToManip[2], 1, curnch-1))
                  {
                    # then need to include all bands in between
                    twoVals<-c(substr(tempToManip[1], curnch, curnch),
                                substr(tempToManip[2], curnch, curnch))
                    # check that the 2 characters are actually numeric
                    if (!any(is.na(as.numeric(twoVals))))
                    {
                      minVal<-min(as.numeric(twoVals))
                      maxVal<-max(as.numeric(twoVals))
                      if (minVal+1 != maxVal)
                      {
                        for (k in (minVal+1):(maxVal-1))
                        {
                          tempToManip[(k-minVal)+2]<-
                            paste(substr(tempToManip[1], 1, curnch-1), k, 
                                          sep="")
                        }
                      }
                    }
                  }
                }
              }
              else
              {
                if (length(grep("cen", tempToManip[1])) > 0)
                {
                  splitonCen<-unlist(strsplit(tempToManip[1], "cen"))
                  chrNo<-splitonCen[1]
                  tempToManip[2]<-paste(chrNo, tempToManip[2], sep="")

                  # can't check for between values because I don't know how
                  # many bands there are until it hits the centromere
                }
                else
                {
                  print(paste("There is no p, q, or cen to split on in iteration", i))
                  print("This is not expected!")
                }
              }
            }
            # add this to strToManip
            strToManip<-c(strToManip, tempToManip)
          }
          strToManip<-strToManip[-grep("-", strToManip)]          
        }
      }

      # now strToManip may be a character string with more than one element
      for (j in 1:length(strToManip))
      {
        if (length(grep("q", strToManip[j])) > 0)
        {
          splitonQ<-unlist(strsplit(strToManip[j], "q"))
          chrNo<-splitonQ[1]
          chrNoQ<-paste(chrNo, "q", sep="")
          if (length(splitonQ) == 2)
          {
            addBands<-rep("", nchar(splitonQ[2]))
            for (k in 1:nchar(splitonQ[2]))
            {
              addBands[k]<-paste(chrNoQ, substr(splitonQ[2], 1, k), sep="")
            }
            if (length(yy) < i)
              yy[[i]]<-c(chrNo, chrNoQ, addBands)
            else
              yy[[i]]<-c(yy[[i]], chrNo, chrNoQ, addBands)
          }
          else
          {
            if (length(yy) < i)
              yy[[i]]<-c(chrNo, chrNoQ)
            else
              yy[[i]]<-c(yy[[i]], chrNo, chrNoQ)
          }            
        }
        else
        {
          if (length(grep("p", strToManip[j])) > 0)
          {  
            splitonP<-unlist(strsplit(strToManip[j], "p"))
            chrNo<-splitonP[1]
            chrNoP<-paste(chrNo, "p", sep="")
            if (length(splitonP) == 2)
            {
              addBands<-rep("", nchar(splitonP[2]))
              for (k in 1:nchar(splitonP[2]))
              {
                addBands[k]<-paste(chrNoP, substr(splitonP[2], 1, k), sep="")
              }
              if (length(yy) < i)
               yy[[i]]<-c(chrNo, chrNoP, addBands)
              else
                yy[[i]]<-c(yy[[i]], chrNo, chrNoP, addBands)
            }
            else
            {
              if (length(yy) < i)
               yy[[i]]<-c(chrNo, chrNoP)
              else
                yy[[i]]<-c(yy[[i]], chrNo, chrNoP)
            }
          }
          else
          {
            if (length(grep("cen", strToManip[j])) > 0)
            {
              splitonCen<-unlist(strsplit(strToManip[j], "cen"))
              chrNo<-splitonCen[1]
              chrNoCen<-paste(chrNo, "cen", sep="")
              if (length(yy) < i)
                yy[[i]]<-c(chrNo, chrNoCen)
              else
                yy[[i]]<-c(yy[[i]], chrNo, chrNoCen)
            }
            else
            {
              if (length(yy) < i)
                yy[[i]]<-strToManip[j]
              else 
                yy[[i]]<-c(yy[[i]], strToManip[j])
            }
          }
        }
      }
      yy[[i]]<-unique(yy[[i]])
      # need to remove any elements that end in ., t, e, or c
      # this removes any elements that end in '.'
      if (length(grep("[.]$", yy[[i]])) > 0)
        yy[[i]]<-yy[[i]][-(grep("[.]$", yy[[i]]))]
      # this removes any elements that end in t
      if (length(grep("t$", yy[[i]])) > 0)
        yy[[i]]<-yy[[i]][-(grep("t$", yy[[i]]))]
      # this removes any elements that end in e
      if (length(grep("e$", yy[[i]])) > 0)
        yy[[i]]<-yy[[i]][-(grep("e$", yy[[i]]))]
      # this removes any elements that end in c
      if (length(grep("c$", yy[[i]])) > 0)
        yy[[i]]<-yy[[i]][-(grep("c$", yy[[i]]))]  
    }
  }
  names(yy)<-names(xx)
  # should I remove the affy ids that have no known chromosome location??
  # yes!
  yy<-yy[-which(is.na(yy))]
  return(yy)
}


##########
# need to convert the data from affy ids to LLids
##########
createLLChrCats<-function(data)
{
  affyMapValues<-chrCats(data)
  # need to convert affy ids to LLids
  LLids<-getEG(names(affyMapValues), data)
  LLMapValues<-list()
  testsum<-0
  uLLids <- unique(LLids)
  for (i in 1:length(uLLids))
  {
    curLL <- uLLids[i]
    matchingAffys<-names(LLids)[LLids==curLL]
    affyMapIndex<-match(matchingAffys, names(affyMapValues))
    LLMapValues[[i]]<-unique(unlist(affyMapValues[affyMapIndex]))
  }
  names(LLMapValues) <- uLLids
  return(LLMapValues)  
}

########
# create the incidence matrix for the following affy ids where the categories
# are based on chromosome location
########
createMAPIncMat<-function(data)
{
  allLLMapValues<-createLLChrCats(data)
  # now create the incidence matrix
  allUniqueCats<-unique(unlist(allLLMapValues))
  # now have the categories and the affy ids so can create the incidence
  #  matrix - rows are the categories and columns are the affy ids
  numRows<-length(allUniqueCats)
  numCols<-length(allLLMapValues)
  incMat<-matrix(rep(0, numRows*numCols), nrow=numRows, ncol=numCols)
  for (i in 1:length(allLLMapValues))
  {
    curCats<-allLLMapValues[[i]]
    rowIndex<-match(curCats, allUniqueCats)
    incMat[rowIndex, i]<-rep(1, length(rowIndex))
  }  
  # need to add row and column names
  rownames(incMat)<-allUniqueCats
  colnames(incMat)<-names(allLLMapValues)

  return(incMat)
}

