\name{testGene2Gene}
\alias{testGene2Gene}
\title{ Test DAVID gene2gene tool }
\description{
  This function tests and demonstrates the use of DAVIDQuery to access the gene2gene tool. 
}
\usage{
testGene2Gene(ids = "33246_AT,32469_AT,1786_AT,32680_AT,1355_G_AT,37968_AT,33530_AT,31987_AT,35956_S_AT,35956_S_AT,1112_G_AT,33077_AT,1331_S_AT,40350_AT,37968_AT,38926_AT,37953_S_AT,34436_AT,37097_AT,32439_AT,35121_AT,40317_AT,39469_S_AT,32439_AT,33685_AT,40294_AT,1575_AT,39187_AT,34720_AT,41489_AT,35439_AT,39698_AT,40790_AT,33922_AT,39908_AT,41113_AT,34606_S_AT,37711_AT,38945_AT,32073_AT", type = "AFFYMETRIX_3PRIME_IVT_ID", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ids}{ Arg passed to  \code{\link{DAVIDQuery}}. }
  \item{type}{ Arg passed to  \code{\link{DAVIDQuery}}. }
  \item{\dots}{ Other args passed to  \code{\link{DAVIDQuery}}. }
}
\details{
  Input Affy IDS are taken from the example on the DAVID web site.
}
\value{
  The value returned by DAVIDQuery using tool=gene2gene.
}

\examples{
	testGene2Gene(details=FALSE)  
		### Run example from http://david.abcc.ncifcrf.gov/gene2gene.jsp
	testGene2Gene(ids=idExampleList, type="ENTREZ_GENE_ID", details=FALSE)
		### Run example from http://david.abcc.ncifcrf.gov/content.jsp?file=DAVID_API.html
}
\seealso{ \code{\link{DAVIDQuery}} }
\author{ Roger Day }
\keyword{ database }
