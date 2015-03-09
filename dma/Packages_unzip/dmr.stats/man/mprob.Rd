\name{mprob}
\alias{mprob}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.31.}

\arguments{See Section 2.4, Example 2.4.31.}

\details{See Section 2.4, Example 2.4.31.}

\value{See Section 2.4, Example 2.4.31.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{mest}}
\code{\link{laprob}}
\code{\link{mvar}}
\code{\link{mmean}}
}

\examples{
data(weather, package="dmr.data")

mprob(weather$outlook, "rainy", m=0)
mprob(weather$outlook, "rainy")
mprob(weather$play[weather$outlook=="overcast"], "no", m=0)
mprob(weather$play[weather$outlook=="overcast"], "no")
mprob(weather$play[weather$outlook=="overcast"], "no", m=3, p0=0.5)
}

\keyword{univar}
