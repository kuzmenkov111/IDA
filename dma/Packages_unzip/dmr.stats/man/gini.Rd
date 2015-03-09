\name{gini}
\alias{gini}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.27.}

\arguments{See Section 2.4, Example 2.4.27.}

\details{See Section 2.4, Example 2.4.27.}

\value{See Section 2.4, Example 2.4.27.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{entropy}}
\code{\link{gini.p}}
}

\examples{
data(weather, package="dmr.data")

gini(weather$outlook)
gini(weather$play)
gini(weather$play[weather$outlook=="overcast"])
gini(weather$play[weather$outlook!="overcast"])
}

\keyword{univar}
