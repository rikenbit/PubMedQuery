\name{meshr-package}
\alias{meshr-package}
\alias{meshr}
\docType{package}
\title{
Enrichment analysis for MeSH terms. 
}
\description{
  meshr package conducts a MeSH enrichment analysis employing gene-MeSH 
  annotation data. A hypergeometric test accounting for a multiple
  tesing correction is used to find significantly enriched MeSH terms. 
}
\details{
\tabular{ll}{
Package: \tab meshr\cr
Version: \tab 0.99.0\cr
Date: \tab 4-5-2014\cr
biocViews: \tab AnnotationData, FunctionalAnnotation, Bioinformatics, Statistics, Annotation, MultipleComparisons\cr
Depends: \tab R (>= 3.0.1), cummeRbund, org.Hs.eg.db, fdrtool, Category, BiocGenerics, methods, MeSH.db, MeSH.AOR.db, MeSH.PCR.db, MeSHDbi, org.MeSH.Hsa.db, org.MeSH.Aca.db, org.MeSH.Atu.K84.db, org.MeSH.Bsu.168.db, org.MeSH.Syn.db\cr
Imports: \tab \cr
Suggests: \tab \cr
License: \tab Artistic-2.0\cr
}

Index:
\preformatted{
meshHyperGTest performs a hypergeometric statistical test. 
}

Further information is available in the vignettes. 

}
\author{
Gota Morota, Koki Tsuyuzaki, Takeru Nakazato, Itoshi Nikaido

Maintainer: Koki Tsuyuzaki <k.t.the-answer@hotmail.co.jp>
}

\examples{
ls("package:meshr")
}

\seealso{
  \code{\link{MeSHHyperGParams-class}},  \code{\link{MeSHHyperGResult-class}},
  \code{\link{meshHyperGTest}}
}

\keyword{ package }
