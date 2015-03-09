\name{prob.ci.par}
\alias{prob.ci.par}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.29.}

\arguments{See Section 2.4, Example 2.4.29.}

\details{See Section 2.4, Example 2.4.29.}

\value{See Section 2.4, Example 2.4.29.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{prob.ci.boot}}
}

\examples{
data(weather, package="dmr.data")

prob.ci.par(weather$play, "yes")
prob.ci.par(weather$play, "yes", delta=0.01)
prob.ci.par(weather$play, "yes", delta=0.1)
}

\keyword{univar}
