\name{hgByChroms}
\alias{hgByChroms}
\non_function{}
\title{ A dataset to show the human genome base pair locations per
  chromosome. }
\usage{data(hgByChroms)}
\format{
  A list, with the names consisting of the names of the chromosomes in
  the human genome (thus 24 elements).  Each element consists of a named
  vector of +/- values - where each value represents the location of a
  base pair (the numeric value is the location, while the +/- denotes
  the strand value), with the name providing the name of the base pair.
}
\description{
  The data is described above.
}
\source{
  Cheng Li of the Dana-Farber Cancer Institute.
}
\examples{
 data(hgByChroms)
}
\keyword{datasets}
