\name{getSEQ}
\alias{getGI}
\alias{getSEQ}

\title{Queries the NCBI database to obtain the sequence for a given
  GenBank Accession number}
\description{
  Given a GenBank Accession number, getSEQ queries the NCBI database for
  the nucleotide sequence.  
}
\usage{
getGI(accNum)
getSEQ(gi)
}

\arguments{
  \item{accNum}{\code{accNum} a character string for a GenBank Accession
    number (i.e. M22490)}
  \item{gi}{\code{gi} a character string or numeric numbers for a
    GenBank accession number or gi number. A gi number is a series of
    digits that are assigned consecutively to each sequence record
    processed by NCBI}
}
\details{
  The NCBI database is queried for the given GenBank Accession number to
  obtain the nucleotide sequence in FASTA format. The leading
  identification line of the sequence data is then dropped to return only
  the nucleotide sequence.  

  getGI returns the gi number corresponding to a given GenBank accession
  number.
}
\value{
  getSEQ returns a character string of nucleotide sequence
}
\references{\url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi}}
\author{Jianhua Zhang}

\examples{
getSEQ("M22490")
}
\keyword{misc}

