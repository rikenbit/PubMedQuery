\name{makeAnchor}
\alias{makeAnchor}
\title{A Function To Generate HTML Anchors}
\description{
  This function will take a set of links and titles and will generate
  HTML anchor tags out of these values
}
\usage{
makeAnchor(link, title, toMain = FALSE)
}
\arguments{
  \item{link}{A vector of URLs}
  \item{title}{A vector of website names}
  \item{toMain}{Used for frame pages}
}
\value{
  A vector of HTML anchor tags
}
\author{Jeff Gentry}
\examples{
makeAnchor("http://www.bioconductor.org","Bioconductor")
}
\keyword{utilities}
