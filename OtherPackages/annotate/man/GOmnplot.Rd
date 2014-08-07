\name{GOmnplot}
\alias{GOmnplot}
\alias{KEGGmnplot}
\alias{KEGGmnplot,character,eSet,character-method}
\alias{KEGGmnplot,character,matrix,character-method}

\title{A function to plot by group means against each other.}
\description{
  For a two sample comparison, as determined by \code{group}, and a
  specified KEGG pathway or GO category, per group means are computed
  and plotted against each other.
}
\usage{
GOmnplot(x, eset, data = "hgu133plus2", group, ...)
KEGGmnplot(x, eset, data = "hgu133plus2", group, ...)
}
\arguments{
  \item{x}{The name of the KEGG pathway or GO category.}
  \item{eset}{An \code{ExpressionSet} containing the data.}
  \item{data}{The name of the chip that was used to provide the data.}
  \item{group}{The variable indicating group membership, should have two
  different values.}
  \item{\dots}{Extra parameters to pass to the call to \code{plot}.}
}
\details{
  All probes in \code{eset} that map to the given category are
  determined. Then per group, per probe means are computed and plotted
  against each other. Extra parameters can be passed to the plot
  function via the \code{dots} argument.
}
\value{
The matrix of per group means, for each probe.
}
\author{R. Gentleman}
\seealso{ \code{\link{KEGG2heatmap}}}
\examples{
  library("hgu95av2.db")
  data(sample.ExpressionSet)
  KEGGmnplot("04810", sample.ExpressionSet, sample.ExpressionSet$sex, 
             data = "hgu95av2")
}
\keyword{manip}
