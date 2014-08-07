\name{hgu95Achroloc}
\alias{hgu95Achroloc}
\non_function{}

\title{Annotation data for the Affymetrix HGU95A GeneChip}
\usage{data(hgu95Achroloc)}

\format{
  These data sets provide environments with mappings from the Affymetrix
  identifiers to chromosomal location, in bases.
  The environments function like hashtables and can be accessed using
  \code{mget}.
  If the returned value is \code{NA} then the current 
  mapping was unable to identify this. Mappings and data sources are
  constantly evolving so updating often is recommended.

}
\description{Data, in the form of environments for the Affymetrix U95A
 chip.}

\source{The \code{AnnBuilder} package.}

\examples{
 data(hgu95Achroloc)
 data(sample.ExpressionSet)
 mget(featureNames(sample.ExpressionSet)[330:340], env=hgu95Achroloc, 
       ifnotfound=NA)

}
\keyword{datasets}
