\name{ecc}
\alias{ecc}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 17 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 17.4, Example 17.4.3.}

\arguments{See Section 17.4, Example 17.4.3.}

\details{See Section 17.4, Example 17.4.3.}

\value{See Section 17.4, Example 17.4.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{multi.ecc}}
}

\examples{
  # error-correcting code for 3 codewords
t(sapply(1:3, ecc, 3))
  # error-correcting code for 4 codewords
t(sapply(1:4, ecc, 4))
  # error-correcting code for 5 codewords
t(sapply(1:5, ecc, 5))
}

\keyword{arith}
