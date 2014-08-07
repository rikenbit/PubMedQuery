\name{getInputIdEntries}
\alias{getInputIdEntries}
\title{Retrieve a set of input ID entries...}
\usage{getInputIdEntries(entry, setName="set")}
\description{Retrieve a set of input ID entries}
\details{The retrieval method based on assumption than the input ID set info incapsulated within the very first
node with name 'set'.}
\value{Character vector representing the set of input IDs.}
\keyword{internal}
\author{Alex Lisovich, Roger Day}
\arguments{\item{entry}{The 'Entry' node of the Enfin xml document.}
\item{setName}{The name ov candidate nodes potentially containing the input ID set info. Default is 'set'.}}
