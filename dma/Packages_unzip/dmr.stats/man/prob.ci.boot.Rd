\name{prob.ci.boot}
\alias{prob.ci.boot}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.30.}

\arguments{See Section 2.4, Example 2.4.30.}

\details{See Section 2.4, Example 2.4.30.}

\value{See Section 2.4, Example 2.4.30.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{prob.ci.par}}
}

\examples{
data(weather, package="dmr.data")

prob.ci.boot(weather$play, "yes")
prob.ci.boot(weather$play, "yes", delta=0.01)
prob.ci.boot(weather$play, "yes", delta=0.1)
}

\keyword{univar}
\keyword{nonparametric}
