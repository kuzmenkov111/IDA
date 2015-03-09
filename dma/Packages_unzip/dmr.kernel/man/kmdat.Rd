\name{kmdat}
\alias{kmdat}
\alias{kmdat.train}
\alias{kmdat.test}
\alias{kmdat.ls}
\alias{kmdat.train.ls}
\alias{kmdat.test.ls}
\alias{kmdat.plot}
\alias{kmg}
\alias{kmf}
\alias{kmf.plot}

\docType{data}

\title{An example dataset from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example dataset from Chapter 16 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 16.1, Example 16.1.1.}

\details{See Section 16.1, Example 16.1.1.}

\source{
}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\examples{
data(kmdat)
data(kmdat.train)
data(kmdat.test)
data(kmdat.ls)
data(kmdat.train.ls)
data(kmdat.test.ls)
data(kmdat.plot)

library(lattice)
wireframe(f~a1+a2, kmdat.plot, col="green", zoom=1)
}

\keyword{datasets}
