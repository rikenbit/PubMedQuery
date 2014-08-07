\name{serializeEnv}
\alias{serializeEnv}
\alias{serializeDataPkgEnvs}
\title{A Function To Serialize Environment}
\description{
  This function will serialize an environment in R to an XML format
  stored in a compressed file.
}
\usage{
serializeEnv(env, fname)
serializeDataPkgEnvs(pkgDir)
}
\arguments{
  \item{env}{The name of the environment to serialize.}
  \item{fname}{The name of the output file.}
  \item{pkgDir}{The directory where a data package is}
}
\details{
   The environment is converted into an XML format and then outputted to
   a gzipped file (using \code{\link{gzfile}}).  The values in the
   environment are serialized (using \code{\link{serialize}}) in ASCII
   format although the keys are stored in plain text.

   The format of the XML is very simple, with the primary block being
   \code{values}, which contain blocks of \code{entries}, and each entry
   having a \code{key} and a \code{value}.  For instance, if we had an
   environment with one value in it, the character \code{c} with a key
   of \code{a} (e.g. \code{assign("a", "c", env=foo)}), this is what the
   output would look like.
   
   \preformatted{
     <?xml version="1.0"?>
     <values xmlns:bt="http://www.bioconductor.org/RGDBM">
        <entry>
           <key>
             a
           </key>
           <value>
              A\n2\n131072\n66560\n1040\n1\n1033\n1\nc\n
           </value>
	</entry>    
     </values>
   }
}
\author{Jeff Gentry}
\seealso{\code{\link{gzfile}},
  \code{\link{serialize}}} 
\examples{
   z <- new.env()
   assign("a", 1, env=z)
   assign("b", 2, env=z)
   assign("c", 3, env=z)
   serializeEnv(z, tempfile())
}
\keyword{utilities}
