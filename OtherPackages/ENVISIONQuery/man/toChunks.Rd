\name{toChunks}
\alias{toChunks}
\title{Convert the ID set into the list of ID subsets.}
\usage{toChunks(ids, chunk=1000)}
\description{Convert the ID set into the list of ID subsets.}
\details{Converts the ID set into the list of ID subsets according to the chunk size.
Used to subdivied a large query into the set of smaller queries to overcome a potential limitation
on a single query ID set size.}
\value{A list of ID subsets according to the chunk size.}
\keyword{internal}
\author{Alex Lisovich, Roger Day}
\arguments{\item{ids}{Character vector representing the input ID set.}
\item{chunk}{The maximium number of IDs retrieved during a single query session. Default is 1000.}}
