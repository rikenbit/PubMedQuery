### From http://david.abcc.ncifcrf.gov/content.jsp?file=DAVID_API.html

DAVIDToolChoices <- strsplit(
"gene2gene
term2term
summary
chartReport
annotationReport
list
geneReport
geneReportFull
geneIdConversion", split="\n")[[1]]

names(DAVIDToolChoices) = 
	strsplit(split="\n",
"Gene Functional Classification
Functional Annotation Clustering
Functional Annotation Summary
Functional Annotation Chart
Functional Annotation Table
Show Gene List Names in Batch
Gene Report
Gene Full Report
Gene ID Conversion")[[1]]

DAVIDToolChoices <- DAVIDToolChoices[
	strsplit(split="\n",
"Gene Functional Classification
Functional Annotation Table
Show Gene List Names in Batch
Gene Report
Gene Full Report
Gene ID Conversion")[[1]]
]
### We cannot yet handle some Annotation features,
### so they are dropped from the pick list.
