\name{trans.mult2}
\alias{trans.mult2}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 16 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 16.4, Example 16.4.1.}

\arguments{See Section 16.4, Example 16.4.1.}

\details{See Section 16.4, Example 16.4.1.}

\value{See Section 16.4, Example 16.4.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{kernel.polynomial}}
}

\examples{
  # original dataset
kmdat.orig <- kmdat.train[1:10,1:4]
  # dot product matrix for the original dataset
kmdat.dp <- as.matrix(kmdat.orig) \%*\% t(kmdat.orig)

  # transformed dataset
kmdat.trans <- trans.mult2(kmdat.orig)
  # dot product matrix for the transformed dataset
kmdat.dpt <- kmdat.trans \%*\% t(kmdat.trans)

  # verify that the dot product matrix for the transformed dataset
  # is the same as the squared original dot product matrix
max(abs((kmdat.dpt-kmdat.dp^2)))
}

\keyword{arith}
