\name{multi.enc.nbc}
\alias{multi.enc.nbc}
\alias{multi.dec.nbc}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 17 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 17.4, Example 17.4.1.}

\arguments{See Section 17.4, Example 17.4.1.}

\details{See Section 17.4, Example 17.4.1.}

\value{See Section 17.4, Example 17.4.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{multi.class}}
}

\examples{
library(dmr.claseval)
data(Vehicle, package="mlbench")

v.nbc <- multi.enc.nbc(Vehicle$Class, "Class")
err(multi.dec.nbc(v.nbc, levels(Vehicle$Class)), Vehicle$Class)
}

\keyword{arith}
