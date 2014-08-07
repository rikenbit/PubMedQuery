\name{hgu95Asym}
\non_function{}
\docType{data}
\alias{hgu95Asym}

\title{Annotation data for the Affymetrix HGU95A GeneChip}

\usage{data(hgu95Asym)}

\format{
  This data set provides an environment with mappings from the Affymetrix
  identifiers to gene symbol.
  The environment functions like a hashtables and can be accessed using
  \code{mget}.
  If the returned value is \code{NA} then the current 
  mapping was unable to identify this. Mappings and data sources are
  constantly evolving so updating often is recommended.
}
\description{Data, in the form of environments for the Affymetrix U95A
 chip.}

\source{The \code{AnnBuilder} package.}

\examples{
 data(hgu95Asym)
 data(sample.ExpressionSet)
 mget(featureNames(sample.ExpressionSet)[330:340], env=hgu95Asym, ifnotfound=NA)
}
\keyword{datasets}
