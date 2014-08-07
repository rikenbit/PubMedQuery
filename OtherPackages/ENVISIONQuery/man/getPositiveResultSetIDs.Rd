\name{getPositiveResultSetIDs}
\alias{getPositiveResultSetIDs}
\title{Retrieve a set of positive query result references from the 'Entry' node of the Enfin xml document.}
\usage{getPositiveResultSetIDs(entry,setName="set",keyword="setType",term="positive result set")}
\description{Retrieve a set of positive query result references from the 'Entry' node of the Enfin xml document.}
\value{Character vector representing the references to the positive matching results node set.}
\keyword{internal}
\author{Alex Lisovich, Roger Day}
\arguments{\item{entry}{The 'Entry' node of the Enfin xml document.}
\item{setName}{The name of the the nodes potentially containing the positive result set info. Default is 'set'.}
\item{keyword}{The name of a set node subnode potentially containing the positive result set info. Default is 'setType'.}
\item{term}{The attribute name to test the candidate node against. Default is 'positive result set'.}}
