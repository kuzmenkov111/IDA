\name{entropy}
\alias{entropy}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.26.}

\arguments{See Section 2.4, Example 2.4.26.}

\details{See Section 2.4, Example 2.4.26.}

\value{See Section 2.4, Example 2.4.26.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{gini}}
\code{\link{entropy.p}}
\code{\link{entropy.cond}}
}

\examples{
data(weather, package="dmr.data")

entropy(weather$outlook)
entropy(weather$play)
entropy(weather$play[weather$outlook=="overcast"])
entropy(weather$play[weather$outlook!="overcast"])
}

\keyword{univar}
