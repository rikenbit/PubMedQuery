\name{getIdEntries}
\alias{getIdEntries}
\title{Retrieve a set of all ID entries...}
\usage{getIdEntries(entry, nodeName="molecule", primaryAttr="id",
    dataAttrs=c("id", "db"))}
\description{Retrieve a set of all ID entries}
\value{Data frame with a single column containing all (input and output) IDs. 
with row names corresponding to the Enfin xml reference names.}
\keyword{internal}
\author{Alex Lisovich, Roger Day}
\arguments{\item{entry}{The 'Entry' node of the Enfin xml document.}
\item{nodeName}{The name identifying the node containing the ID info. Default is 'molecule'.}
\item{primaryAttr}{The name of attribute which is used as row names of the output data frame.
Default is 'id' standing for gene ID}
\item{dataAttrs}{The names of the attributes which values are to be retrieved. 
Default is c('id','db'), i.e. gene ID and data base source.}}
