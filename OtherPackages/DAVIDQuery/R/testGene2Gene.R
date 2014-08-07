testGene2Gene <- function(ids="33246_AT,32469_AT,1786_AT,32680_AT,1355_G_AT,37968_AT,33530_AT,31987_AT,35956_S_AT,35956_S_AT,1112_G_AT,33077_AT,1331_S_AT,40350_AT,37968_AT,38926_AT,37953_S_AT,34436_AT,37097_AT,32439_AT,35121_AT,40317_AT,39469_S_AT,32439_AT,33685_AT,40294_AT,1575_AT,39187_AT,34720_AT,41489_AT,35439_AT,39698_AT,40790_AT,33922_AT,39908_AT,41113_AT,34606_S_AT,37711_AT,38945_AT,32073_AT", type="AFFYMETRIX_3PRIME_IVT_ID", ...){
##	This is DAVID's example of using gene2gene.
##	The default example list was obtained as follows: 
##    	- Navigating to http://david.abcc.ncifcrf.gov/gene2gene.jsp
##		- On the Upload tab clicking "Demolist 1", 
##		- Clicking "Download File"
##		- Noting the URL of the file (which  is not permanent),
##		- Executing 
##			gene2gene.example.output <- read.delim(("http://david.abcc.ncifcrf.gov/data/download/GG_4E93BEC325C1.txt"))
##			gene2gene.example.input <- gene2gene.example.output[,1]
##			gene2gene.example.input <- gene2gene.example.input [
##		- grep("Functional Group", gene2gene.example.input)]
	DAVIDQuery(ids=ids, type=type, 
		tool="gene2gene", annot=NULL, ...)
}
