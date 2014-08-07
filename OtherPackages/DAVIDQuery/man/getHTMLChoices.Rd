\name{getHTMLChoices}
\alias{getHTMLChoices}
\title{Get (name,value) pairs from the HTML page selection box...}
\usage{getHTMLChoices(page, header)}
\description{Get (name,value) pairs from the HTML page selection box}
\value{data frame containing (name,value) pair columns.
The first column is a decorated ID name, i.e. the name which appears in the corresponding drop-down list on the DAVID web site.
The second column is actual value used in a conversion request corresponding to a particular decorated name.}
\keyword{internal}
\author{Roger Day, Alex Lisovich}
\arguments{\item{page}{character string representing HTML page}
\item{header}{character string uniquely identifying selection box}}
