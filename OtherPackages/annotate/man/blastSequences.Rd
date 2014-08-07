\name{blastSequences}
\alias{blastSequences}

\title{
  Run a blast query to NCBI for either a string or an entrez gene ID and
  then return a series of MultipleAlignment objects.
}

\description{
  This function sends a query to NCBI as a string of sequence or an
  entrez gene ID and then returns a series of MultipleAlignment objects.
}

\usage{
  blastSequences(x, database, hitListSize, filter, expect, program)
}

\arguments{
  \item{x}{
    A sequence as a character vector or an integer corresponding to an
  entrez gene ID.
  }
  \item{database}{
    Which NCBI database to use.
  }
  \item{hitListSize}{
    
  }
  \item{filter}{
    
  }
  \item{expect}{
    How many hits do you hope to get back, this will put a limit on the amount.
  }
  \item{program}{
    Which program do you want to use for blast.  Default value is blastn.
  }
}

\details{
  Right now the function only works for "blastn".
}

\value{ a series of MultipleAlignment objects of the appropriate type. }

\author{
  M. Carlson
}

\examples{

## x can be an entrez gene ID
blastSequences(17702)

if(interactive()){

## or x can be a sequence
blastSequences(x = "GGCCTTCATTTACCCAAAATG")

## hitListSize does not promise that you will get the number of matches you
## want..  It will just try to get that many.
blastSequences(x = "GGCCTTCATTTACCCAAAATG", hitListSize="20")

}
}
