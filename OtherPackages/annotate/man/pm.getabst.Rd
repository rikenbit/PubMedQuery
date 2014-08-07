\name{pm.getabst}
\alias{pm.getabst}
\title{Obtain the abstracts for a set PubMed list.}
\description{
The data provided by PubMed is reduced to a small set. This set is 
then suitable for further rendering.
}
\usage{
pm.getabst(geneids, basename)
}
\arguments{
  \item{geneids}{The identifiers used to find Abstracts }
  \item{basename}{The base name of the annotation package to use. }
}
\details{
We rely on the annotation in the package associated with the 
\code{basename} to provide PubMed identifiers for the genes described by
the gene identifiers.
With these in hand we then use the \code{pmfetch} utility to download the
PubMed abstracts in XML form. These are then translated (transformed) to a 
shorter version containing a small subset of the data provided by PubMed.

This function has the side effect of creating an environment in 
\code{.GlobalEnv} that contains the mapping for the requested data.
This is done for efficiency -- so we don't continually read in the data
when there are many different queries to be performed.
}
\value{
 A list of lists containing objects of class \code{pubMedAbst}.
 There will be one element of the list for each identifier.
 Each of these elements is a list containing one abstract (of
 class \code{pubMedAbst} for each PubMed identifier associated with
 the gene identifier.
}
\author{Robert Gentleman}
\seealso{\code{\link{pm.abstGrep}}, \code{\link{pm.titles}}}
\examples{
  library("hgu95av2.db")
  hoxa9 <- "37806_at"
  absts <- pm.getabst(hoxa9, "hgu95av2")
}
\keyword{manip}
