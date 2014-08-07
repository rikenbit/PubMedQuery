setMethod("meshHyperGTest", signature(p="MeSHHyperGParams"),
          function(p) .meshHyperGTestInternal(p) )

.meshHyperGTestInternal <- function(p) {
  ##
  ## MeSH enrichment analysis 
  ##

  ## Map gene ID to MeSH ID through annotation data
  my.keytype <- c("GENEID")
  my.cols <- c("GENEID", "MESHID")
  my.geneids <- as.data.frame(p@geneIds)
  names(my.geneids) <- "GENEID"
  universe.geneids <- as.data.frame(p@universeGeneIds)
  names(universe.geneids) <- "GENEID"

  ## Retive data of specific database
  selectedDatabase <- select(eval(parse(text=p@annotation)), keys = p@database, columns = c("GENEID", "MESHID","MESHCATEGORY"), keytype = "SOURCEDB")
  selectedDatabase <- selectedDatabase[which(selectedDatabase[,3] == p@category), 1:2]

  ## Error against impossible category-database combination was choosed
  if(nrow(selectedDatabase) == 0){
    stop("Impossible MeSH category - database combination was choosed.")
  }

  selected.mesh <- merge(my.geneids, selectedDatabase, "GENEID")
  universe.mesh <- merge(universe.geneids, selectedDatabase, "GENEID")

  selected.table <- table(selected.mesh[,2])
  universe.table <- table(universe.mesh[,2])

  ## Hypergeometric test 
  pvals <- array()
  for (i in 1:length(selected.table)) {
    numWdrawn <- selected.table[i]
    mesh.index <- which(names(selected.table[i])==names(universe.table))
    numW <- universe.table[mesh.index]
    numB <- length(p@universeGeneIds) - numW
    numDrawn <- length(p@geneIds)
    pvals[i] <- phyper(numWdrawn - 1L, numW, numB, numDrawn, lower.tail=FALSE)
  }
  
  ## Multiple testing correction  
  stats <- switch(p@pAdjust, BH = {
    p.adjust(pvals, "BH")
  }, QV = {
    suppressWarnings(fdrtool(pvals, statistic="pvalue", plot=FALSE, verbose=FALSE)$qval)
  }, lFDR = {
    suppressWarnings(fdrtool(pvals, statistic="pvalue", plot=FALSE, verbose=FALSE)$lfdr)
  }, none = pvals)

  ## Choose siginificantly enriched MeSH terms 
  mesh.list <- names(selected.table[which(stats < p@pvalueCutoff)])
  sort.index <- unlist(sort(stats[which(stats < p@pvalueCutoff)], index.return=TRUE)[2])
  pval.vec <- unlist(sort(stats[which(stats < p@pvalueCutoff)], index.return=TRUE)[1])
  mesh.list <- mesh.list[sort.index]

  ## Retrieve full name of MeSH category 
  mesh.cat <- p@category

  switch(mesh.cat,
    "A" = {
      mesh.full.cat <- "Anatomy"
    }, "B" = {
      mesh.full.cat <- "Organisms"
    }, "C" = {
      mesh.full.cat <- "Diseases"
    }, "D" = {
      mesh.full.cat <- "Chemicals and Drugs"
    }, "E" = {
      mesh.full.cat <- "Analytical, Diagnostic and Therapeutic Techniques and Equipment"
    }, "F" = {
      mesh.full.cat <- "Psychiatry and Psychology"
    }, "G" = {
      mesh.full.cat <- "Phenomena and Processes"
    }, "H" = {
      mesh.full.cat <- "Disciplines and Occupations"
    }, "I" = {
      mesh.full.cat <- "Anthropology, Education, Sociology and Social Phenomena"
    }, "J" = {
      mesh.full.cat <- "Technology and Food and Beverages"
    }, "K" = {
      mesh.full.cat <- "Humanities"
    }, "L" = {
      mesh.full.cat <- "Information Science"
    }, "M" = {
      mesh.full.cat <- "Persons"
    }, "N" = {
      mesh.full.cat <- "Health Care"
    }, "V" = {
      mesh.full.cat <- "Publication Type"
    }, "Z" = {
      mesh.full.cat <- "Geographical Locations"
    }
  )

  ## Mapping  MeSH ID to MeSH term
  mesh.df <- select(MeSH.db, keys=mesh.list, columns=c("MESHID", "MESHTERM", "CATEGORY"), keytype="MESHID")
  # remove categories not specified 
  mesh.df <- mesh.df[mesh.cat==mesh.df[,3],][,1:2]
  # remove MeSH terms appearing multiple times within same category 
  mesh.df <- mesh.df[!duplicated(mesh.df[,1]),]
  mesh.df <- mesh.df[sort.index,]
  mesh.df$PVALUE <- pval.vec
  
  new("MeSHHyperGResult",
      meshCategory=mesh.full.cat,
      meshAnnotation=p@annotation, 
      meshDatabase=p@database, 
      meshIds=mesh.df[,1],
      meshTerms=mesh.df[,2],
      pvalues=mesh.df[,3])
}

