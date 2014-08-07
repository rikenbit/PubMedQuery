### R code from vignette source 'GEOquery.Rnw'

###################################################
### code chunk number 1: GEOquery.Rnw:53-54
###################################################
library(GEOquery)


###################################################
### code chunk number 2: GEOquery.Rnw:59-63
###################################################
# If you have network access, the more typical way to do this
# would be to use this:
# gds <- getGEO("GDS507")
gds <- getGEO(filename=system.file("extdata/GDS507.soft.gz",package="GEOquery"))


###################################################
### code chunk number 3: GEOquery.Rnw:73-77
###################################################
# If you have network access, the more typical way to do this
# would be to use this:
# gds <- getGEO("GSM11805")
gsm <- getGEO(filename=system.file("extdata/GSM11805.txt.gz",package="GEOquery"))


###################################################
### code chunk number 4: GEOquery.Rnw:86-93
###################################################
# Look at gsm metadata:
Meta(gsm)
# Look at data associated with the GSM:
# but restrict to only first 5 rows, for brevity
Table(gsm)[1:5,]
# Look at Column descriptions:
Columns(gsm)


###################################################
### code chunk number 5: GEOquery.Rnw:98-99
###################################################
Columns(gds)


###################################################
### code chunk number 6: GEOquery.Rnw:105-115
###################################################
# Again, with good network access, one would do:
# gse <- getGEO("GSE781",GSEMatrix=FALSE)
gse <- getGEO(filename=system.file("extdata/GSE781_family.soft.gz",package="GEOquery"))
Meta(gse)
# names of all the GSM objects contained in the GSE
names(GSMList(gse))
# and get the first GSM object on the list
GSMList(gse)[[1]]
# and the names of the GPLs represented
names(GPLList(gse))


###################################################
### code chunk number 7: GEOquery.Rnw:130-134
###################################################
# Note that GSEMatrix=TRUE is the default
gse2553 <- getGEO('GSE2553',GSEMatrix=TRUE)
show(gse2553)
show(pData(phenoData(gse2553[[1]]))[1:5,c(1,6,8)])


###################################################
### code chunk number 8: GEOquery.Rnw:141-142
###################################################
eset <- GDS2eSet(gds,do.log2=TRUE)


###################################################
### code chunk number 9: GEOquery.Rnw:147-149
###################################################
eset
pData(eset)


###################################################
### code chunk number 10: GEOquery.Rnw:156-160
###################################################
#get the platform from the GDS metadata
Meta(gds)$platform
#So use this information in a call to getGEO
gpl <- getGEO(filename=system.file("extdata/GPL97.annot.gz",package="GEOquery"))


###################################################
### code chunk number 11: GEOquery.Rnw:165-167
###################################################
MA <- GDS2MA(gds,GPL=gpl)
MA


###################################################
### code chunk number 12: GEOquery.Rnw:179-181
###################################################
gsmplatforms <- lapply(GSMList(gse),function(x) {Meta(x)$platform})
gsmplatforms


###################################################
### code chunk number 13: GEOquery.Rnw:186-189
###################################################
Table(GSMList(gse)[[1]])[1:5,]
# and get the column descriptions
Columns(GSMList(gse)[[1]])[1:5,]


###################################################
### code chunk number 14: GEOquery.Rnw:194-207
###################################################
# get the probeset ordering
probesets <- Table(GPLList(gse)[[1]])$ID
# make the data matrix from the VALUE columns from each GSM
# being careful to match the order of the probesets in the platform
# with those in the GSMs
data.matrix <- do.call('cbind',lapply(GSMList(gse),function(x) 
                                      {tab <- Table(x)
                                       mymatch <- match(probesets,tab$ID_REF)
                                       return(tab$VALUE[mymatch])
                                     }))
data.matrix <- apply(data.matrix,2,function(x) {as.numeric(as.character(x))})
data.matrix <- log2(data.matrix)
data.matrix[1:5,]


###################################################
### code chunk number 15: GEOquery.Rnw:212-221
###################################################
require(Biobase)
# go through the necessary steps to make a compliant ExpressionSet
rownames(data.matrix) <- probesets
colnames(data.matrix) <- names(GSMList(gse))
pdata <- data.frame(samples=names(GSMList(gse)))
rownames(pdata) <- names(GSMList(gse))
pheno <- as(pdata,"AnnotatedDataFrame")
eset2 <- new('ExpressionSet',exprs=data.matrix,phenoData=pheno)
eset2


###################################################
### code chunk number 16: GEOquery.Rnw:236-242
###################################################
gpl97 <- getGEO('GPL97')
Meta(gpl97)$title
head(Meta(gpl97)$series_id)
length(Meta(gpl97)$series_id)
head(Meta(gpl97)$sample_id)
length(Meta(gpl97)$sample_id)


###################################################
### code chunk number 17: GEOquery.Rnw:247-250
###################################################
gsmids <- Meta(gpl97)$sample_id
gsmlist <- sapply(gsmids[1:5],getGEO)
names(gsmlist)


###################################################
### code chunk number 18: GEOquery.Rnw:258-259
###################################################
toLatex(sessionInfo())


