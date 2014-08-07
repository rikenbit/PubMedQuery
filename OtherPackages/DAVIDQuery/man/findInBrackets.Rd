\name{findInBrackets}
\alias{findInBrackets}
\title{Extract the substring(s) between the left and right brackets...}
\usage{findInBrackets(string, left, right, includeLeft=FALSE,
    includeRight=FALSE)}
\description{Extract the substring(s) between the left and right brackets
where left is the last occurence of the left bracket
before the first occurence of the right bracket}
\value{substring between the left and right brackets. If not found, returns empty string.}
\keyword{internal}
\author{Roger Day, Alex Lisovich}
\arguments{\item{string}{character string to extract substring from}
\item{left}{left bracket (character string)}
\item{right}{right bracket (character string)}
\item{includeLeft}{if TRUE include left bracket into the result string. Default is FALSE}
\item{includeRight}{if TRUE include right bracket into the result string. Default is FALSE}}
\examples{\dontrun{
# obtain an HTML page represented by a character string conversionPage
# then extract the link to the data file with extension 'txt':
fileName<-findInBrackets(conversionPage,"href=\"",".txt");
}}
