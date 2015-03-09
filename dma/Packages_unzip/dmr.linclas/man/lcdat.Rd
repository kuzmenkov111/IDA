\name{lcdat}
\alias{lcdat}
\alias{lcdat.train}
\alias{lcdat.test}
\alias{lcdat.ls}
\alias{lcdat.train.ls}
\alias{lcdat.test.ls}
\alias{lcdat.plot}
\alias{lcg}
\alias{lcg.plot}
\alias{pcdat}
\alias{pcdat.train}
\alias{pcdat.test}
\alias{pcdat.plot}
\alias{pcg}
\alias{pcg.plot}

\docType{data}

\title{Example datasets from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example datasets from Chapter 5 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 5.1, Example 5.1.1.}

\source{
}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\examples{
data(lcdat)
data(lcdat.train)
data(lcdat.test)
data(lcdat.ls)
data(lcdat.train.ls)
data(lcdat.test.ls)
data(lcdat.plot)
summary(pcdat)
summary(pcdat.train)
summary(pcdat.test)
summary(pcdat.plot)

library(lattice)
wireframe(g~a1+a2, lcdat.plot, col="green", zoom=0.8)
wireframe(g~a1+a2, pcdat.plot, col="green", zoom=0.8)
}

\keyword{datasets}
