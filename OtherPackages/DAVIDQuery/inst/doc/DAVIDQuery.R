### R code from vignette source 'DAVIDQuery.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: chunk1
###################################################
library("DAVIDQuery")
result = DAVIDQuery(type="UNIPROT_ACCESSION", annot=NULL, tool="geneReportFull")
names(result)



###################################################
### code chunk number 2: chunk2
###################################################
Sys.sleep(10)  ### Assure that queries are not too close in time.
result = DAVIDQuery(type="UNIPROT_ACCESSION", annot=NULL, tool="geneReport")
result$firstURL
result$secondURL
result$downloadURL
result$DAVIDQueryResult



###################################################
### code chunk number 3: chunk3
###################################################
Sys.sleep(10)  ### Assure that queries are not too close in time.
result = testGene2Gene(details=FALSE)
length(result)
names(result[[1]])


###################################################
### code chunk number 4: chunk4
###################################################
Sys.sleep(10)  ### Assure that queries are not too close in time.
affyToUniprot(details=FALSE)
Sys.sleep(10)  ### Assure that queries are not too close in time.
uniprotToAffy(details=FALSE)


###################################################
### code chunk number 5: sessionInfo
###################################################
toLatex(sessionInfo())


