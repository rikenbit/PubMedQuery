\name{setRepository}
\alias{setRepository}
\alias{getRepositories}
\alias{clearRepository}
\title{Functions to add arbitrary repositories}

\description{These functions allow end users to add arbitrary
  repositories for use with the \code{htmlpage} function.
}
\usage{
setRepository(repository, FUN, ..., verbose=TRUE)
getRepositories()
clearRepository(repository, verbose=TRUE)
}
\arguments{
  \item{repository}{A character name for the repository.}
  \item{FUN}{A function to build hyperlinks for the repository. See
    details for more information.}
  \item{...}{Allows one to pass arbitrary code to underlying functions.}
  \item{verbose}{Output warning messages?}
}
\details{These functions allow end users to add, view, and remove repositories
  for use with the \code{htmlpage} function. \code{getRepositories} will
  output a vector of names for available
  repositories. \code{clearRepository} can be used to remove a
  repository if so desired. \code{setRepository} can be used to add a
  repository. See the examples section for the format of the FUN
  argument.

  Once a new repository has been set, the \code{htmlpage} function can
  be called using the name of the new repository as a value in the
  repository argument (e.g., htmlpage(<other args>, repository =
  list("newrepositoryname"))
}
\author{Martin Morgan <mtmorgan@fhcrc.org>}
\examples{

## A simple fake URI
repofun <- function(ids, ...)
paste("http://www.afakeuri.com/", ids, sep = "")

setRepository("simple", repofun)

## More complicated, we want to make sure that
## NAs get converted to empty cells

repofun <- function(ids, ...){
bIDs <- which(is.na(ids))
out <- paste("http://www.afakeuri.com/", ids, sep = "")
out[bIDs] <- "&nbsp;"
out
}

setRepository("complex", repofun)

## More complicated URI where we need to pass more information
## An example is Ensembl, which requires a species as part of the URI
## Since htmlpage() has an '...' argument, we can pass arbitrary
## arguments to this function that will be passed down to our
## repfun. Here we assume the argument species="Homo_sapiens" has been
## included in the call to htmlpage().


repofun <- function(ids, ...){
if(!is.null(list(...)$species))
      species <- list(...)$species
  else
      stop("To make links for Ensembl, you need to pass a 'species' argument.",
           call. = FALSE)
out <- paste("http://www.ensembl.org/", species, "/Search/Summary?species=",
              species, ";idx=;q=", ids, sep = "")
out
}

setRepository("species_arg", repofun)

}
\keyword{manip}
