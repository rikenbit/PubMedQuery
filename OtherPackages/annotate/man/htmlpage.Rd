\name{htmlpage}
\alias{htmlpage}
\title{Functions to build HTML pages}

\description{ This function is designed to create an HTML table
  containing both static information as well as links to various online
  annotation sources.
}
\usage{
htmlpage(genelist, filename, title, othernames, table.head,
         table.center = TRUE, repository = list("en"), ...)
}
\arguments{
  \item{genelist}{A list or \code{data.frame} of character vectors
    containing ids to be made into hypertext links. See details for more
    information.}
  \item{filename}{A filename for the resultant HTML table.}
  \item{title}{A title for the table.}
  \item{othernames}{A list or \code{data.frame} of other things to add
    to the table. These will not be hyperlinks. The list of othernames
    can contain vectors, matrices, \code{data.frames} or lists.}
  \item{table.head}{A character vector of column headers for the table.}
  \item{table.center}{Center the table? Defaults to \code{TRUE}.}
  \item{repository}{A list of repositories to use
    for creating the hypertext links. Currently available repositories
    include 'gb' (GenBank), 'en' (EntrezGene), 'omim' (Online Mendelian
    Inheritance in Man), 'sp' (SwissProt), 'affy' (Affymetrix), 'ug'
    (UniGene), 'fb' (FlyBase), 'go' (Gene Ontology), 'ens' (Ensembl).
    Additional repositories can easily be added. See
  \code{setRepository} for more information.}
  \item{...}{Further arguments to be passed. See details for more
  information.}
}
\details{ This function will accept a list or \code{data.frame} of
  character vectors, each containing different ids that are to be turned
  into hyperlinks (e.g., a list containing affy ids, genbank accession
  numbers, and Entrez Gene ids). For instances where there are more than
  one id per gene, use a sub-list of character vectors. See the vignette
  'HowTo: Get HTML Output' for more information. Othernames should be a
  list or \code{data.frame}. Again, if there are multiple entries for a
  given gene, use a sub-list. This is more easily explained using an
  example - please see the examples section below and the above
  mentioned vignette.

  In even the simplest case the genelist, othernames and repository have
  to be lists. A simple character vector will not suffice. 

  Note that this function now uses \code{xtable} to create the HTML
  table, and there is the ability to pass some arguments on to either
  \code{xtable} or \code{print.xtable}. One such argument would be
  'append=TRUE', which would allow one to put lots of tables in one
  page, as long as the filename argument remained the same.

  Additionally, the Ensembl repository needs a species argument in order
  to form a usable URI. This argument can be passed in the form of e.g.,
  species = "Homo\_sapiens". Note the capitalization of the genus, and
  the separation by an underscore (\_).
}
\value{
  This function is used only for the side effect of creating an HTML table.
}
\author{Robert Gentleman <rgentlem@fhcrc.org>, further
  modifications by James W. MacDonald <jmacdon@med.umich.edu>}
\examples{
  library("annotate")
  ## A very simple example. Two columns, one with links, the other without.

  gos <- paste("GO:000000", 1:9, sep="")
  notlinks <- LETTERS[1:9]

  htmlpage(list(gos), "simple.html", "Two column data", list(notlinks),
           c("GO IDs", "Letters"), repository = list("go"))

  if(!interactive())
    file.remove("simple.html")

  ## A more complex example with multiple links per cell
  ## first we create data to annotate
  unigene <- list("Hs.600536",c("Hs.596913","HS.655491"),"Hs.76704")
  refseq <- list(c("NM_001030050", "NM_001030047", "NM_001648",
  "NM_001030049"), "NM_000860", c("NM_001011645", "NM_000044"))
  entrez <- c("354", "3248", "367")
  genelist <- list(unigene, refseq, entrez)

  ## now some other data

  symb <- c("KLK3","HPGD","AR")
  desc <- c("Prostate-specific antigen precursor",
            "15-hydroxyprostaglandin dehydrogenase",
            "Androgen receptor")
  t.stat <- c(40.21, -22.14, 21.56)
  p.value <- rep(0,3)
  fold.change <- c(3.54, -2.35, 3.18)
  expression <- matrix(c(11.78, 11.69, 11.62, 8.17, 5.78, 5.58, 5.68,
                         8.26, 9.08, 9.28, 9.19, 6.05), ncol=4, byrow=TRUE)

  otherdata <- list(symb, desc, t.stat, p.value, fold.change, expression)
  table.head <- c("UniGene", "RefSeq", "EntrezGene", "Symbol",
                  "Description", "t-stat", "p-value", "fold change",
                  paste("Sample", 1:4))

  htmlpage(genelist, "test.html", "Some gene expression data", otherdata,
           table.head, repository = list("ug","gb","en"))

  if(!interactive())
    file.remove("test.html")
}
\keyword{manip}
