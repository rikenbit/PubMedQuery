\name{LL2homology}
\alias{LL2homology}
\alias{HGID2homology}
\alias{ACC2homology}
\title{DEPRECATED Functions that find the homology data for a given set of
  LocusLink ids or HomoloGeneIDs}
\description{
  These functions are DEPRECATED. All this functionality has been
  replaced by inPARANOID packages.
  Given a set of LocusLink ids or NCBI HomoloGeneIDs, the functions obtain the
  homology data and represent them as a list of sub-lists using the
  homology data package for the organism of interest. A sub-list can be of
  length 1 or greater depending on whether a LocusLink id can be mapped
  to one or more HomoloGeneIDs.
}
\usage{
LL2homology(homoPkg, llids)
HGID2homology(hgid, homoPkg)
ACC2homology(accs, homoPkg)
}
\arguments{
  \item{llids}{\code{llids} a vector of character strings or numberic
    numbers for a set of LocusLink ids whose homologous genes in other
    organisms are to be found}
  \item{hgid}{\code{hgid} a named vector of character strings or numberic
    numbers for a set of HomoloGeneIDs whose homologous genes in other
    organisms are to be found. Names of the vector give the code used by
    NCBI for organisms}
  \item{accs}{\code{accs} a vector of character strings for a set of
    GenBank Accession numbers}
  \item{homoPkg}{\code{homoPkg} a character string for the name of the
    homology data package for a given organism, which is a short version
    of the scientific name of the organism plus homology (e. g. hsahomology)}
}
\details{
  The homology data package has to be installed before executing any of
  the two functions.

  Each sub-list  has the following elements:

  homoOrg - a named vector of a single character string whose value
  is the scientific name of the organism and name the numeric code
  used by NCBI for the organism.
  
  homoLL - an integer for LocusLink id.
  
  homoHGID - an integer for internal HomoloGeneID.
  
  homoACC - a character string for GenBank accession number of the
  best matching sequence of the organism.
  
  homoType - a single letter for the type of similarity measurement
  between the homologous genes. homoType can be either B (reciprocal
  best best between three or more organisms), b (reciprocal best
  match between two organisms), or c (curated homology relationship
  between two organisms).
  
  homoPS - a percentage value measured as the percent of identity of
  base pair alignment between the homologous sequences. 
  
  homoURL - a url to the source if the homology relationship is a
  curated orthology.
  
  Sub-lists with homoType = B or b will not have any value
  for homoURL and objects with homoType = c will not have any value
  for homoPS.
}
\value{
  Both functions returns a list of sub-lists containing data for
  homologous genes in other organisms.
}
\references{\url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?=homologene}}
\author{Jianhua Zhang}

\examples{
\dontrun{
    ## hsahomology is a deprecated package! 
    if(require("hsahomology")){
        llids <- ls(env = hsahomologyLL2HGID)[2:5]
        LL2homology("hsahomology", llids)
    }
    
}
}
\keyword{misc}

