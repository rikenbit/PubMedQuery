\name{bracketedStrings}
\alias{bracketedStrings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Extract bracketed substrings. }
\description{
Extract substrings that are bracketed by specified strings before and after.
}
\usage{
bracketedStrings(s, before, after, verbose=FALSE, addNames=FALSE, drop.na=TRUE, warn.if.gt.1=TRUE)
}
\arguments{
  \item{s}{ Vector of strings to search. }
  \item{before}{ String to the left of the desired substring within s. }
  \item{after}{  String to the right of the desired substring within s. }
  \item{verbose}{ If TRUE, print the starting and ending index (or indices) of the desired substring(s). }
  \item{addNames}{ If TRUE, and if \code{s} is a vector, set the \code{names} attribute of the return value to \code{s}. }
  \item{drop.na}{ If TRUE, remove empty strings from the return value. }
  \item{warn.if.gt.1}{ If TRUE, warn if a string has more than one pair of bracketed target strings. }
}
\value{
For a single input string \code{s}, the return value is the desired substring sandwiched between \code{before} and \code{after}. For a vector of inputs, list of outputs.  }
\author{ Roger Day }

\seealso{ \code{\link{DAVIDQuery}} }
\examples{ 
bracketedStrings("quickbrownfox", "quick", "fox")
bracketedStrings(c("quickbrownfox", "quickredfox"), "quick", "fox", addNames=TRUE)
bracketedStrings("quickbrownfoxANDquickredfox", "quick", "fox")
bracketedStrings("quickbrownfoxANDquickredfox", "quick", "fox", warn.if.gt.1=FALSE)
}
\keyword{ character }
\keyword{ manip }
