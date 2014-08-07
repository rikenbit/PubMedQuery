\name{GO2heatmap}
\alias{GO2heatmap}
\alias{KEGG2heatmap}
\alias{KEGG2heatmap,character,eSet,character-method}
\alias{KEGG2heatmap,character,matrix,character-method}

\title{Compute a heatmap for the specified data, for either a GO
  category or a KEGG pathway.}
\description{
  For a given GO category or KEGG pathway, all probes in the supplied
  data are mapped to the pathway and a heatmap is produced.
}
\usage{
GO2heatmap(x, eset, data, ...)
KEGG2heatmap(x, eset, data, ...)
}
\arguments{
  \item{x}{The name of the category or pathway.}
  \item{eset}{An \code{ExpressionSet} providing the data.}
  \item{data}{The name of the chip.}
  \item{\dots}{Additional parameters to pass to \code{heatmap}.}
}
\details{
 For the given pathway or GO category all matching probes are
  determined, these are used to subset the data and \code{heatmap} is
  invoked on that set of data. Extra parameters can be passed through to
  \code{heatmap} using the \code{\dots} parameter.
 The \code{annotation} slot of the \code{eset} argument is used to
 determine the appropriate annotation data to use.
}
\value{
  The value returned by \code{heatmap} is passed back to the user.
}
\author{R. Gentleman }
\seealso{\code{\link{heatmap}}}
\examples{
  library("hgu95av2.db")
  data(sample.ExpressionSet)
  KEGG2heatmap("04810", sample.ExpressionSet, "hgu95av2")
}
\keyword{manip}
