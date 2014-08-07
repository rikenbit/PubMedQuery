\name{HTMLPage-class}
\docType{class}
\alias{HTMLPage-class}
\alias{HTMLPage}
\alias{FramedHTMLPage}
\alias{FramedHTMLPage-class}
\alias{fileName}
\alias{mainPage}
\alias{sidePage}
\alias{pageText}
\alias{toFile}
\alias{topPage}
\alias{pageTitle}
\alias{HTMLPage,HTMLPage-method}
\alias{FramedHTMLPage,HTMLPage-method}
\alias{fileName,HTMLPage-method}
\alias{mainPage,HTMLPage-method}
\alias{sidePage,HTMLPage-method}
\alias{pageText,HTMLPage-method}
\alias{toFile,HTMLPage-method}
\alias{topPage,HTMLPage-method}
\alias{pageTitle,HTMLPage-method}
\alias{show,HTMLPage-method}
\alias{initialize,FramedHTMLPage-method}
\alias{mainPage,FramedHTMLPage-method}
\alias{show,FramedHTMLPage-method}
\alias{sidePage,FramedHTMLPage-method}
\alias{toFile,FramedHTMLPage-method}
\alias{topPage,FramedHTMLPage-method}
\title{Classes to represent HTML pages}
\description{Class \code{HTMLPage} and \code{FramedHTMLPage} are a pair
  of experimental classes used to explore concepts of representing HTML
  pages using S4 objects.}
\section{Slots}{
  \describe{
    \item{\code{fileName}:}{Object of class \code{"character"} The
      filename of the HTML page}
    \item{\code{pageText}:}{Object of class \code{"character"} The text
      of the HTML page}
    \item{\code{pageTitle}:}{Object of class \code{"character"} The
      title of the HTML page}
    \item{\code{topPage}:}{Object of class \code{"HTMLPage"} The header
      page for a FramedHTMLPage}
    \item{\code{sidePage}:}{Object of class \code{"HTMLPage"} The side
      index page for a FramedHTMLPage}
    \item{\code{mainPage}:}{Object of class \code{"HTMLPage"} The
      primary page for a FramedHTMLPage}
  }
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "HTMLPage")}: Describes
      information about the page}
    \item{fileName}{\code{signature(object = "HTMLPage")}: Gets the
      fileName slot}
    \item{pageText}{\code{signature(object = "HTMLPage")}: Gets the
      pageText slot}
    \item{pageTitle}{\code{signature(object = "HTMLPage")}: Gets the
      pageTitle slot}
    \item{toFile}{\code{signature(object = "HTMLPage")}: Writes the page
    out to the file designated by the fileName slot}
  }
}
\author{Jeff Gentry}
\note{
  These classes are currently experimental.

  FramedHTMLPage is modeled after the framing layout of the Bioconductor
  website (www.bioconductor.org).
}

\examples{
##---- Should be DIRECTLY executable !! ----
}
\keyword{classes}
