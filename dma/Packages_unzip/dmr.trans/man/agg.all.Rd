\name{agg.all}
\alias{agg.all}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 17 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 17.3, Example 17.3.3.}

\arguments{See Section 17.3, Example 17.3.3.}

\details{See Section 17.3, Example 17.3.3.}

\value{See Section 17.3, Example 17.3.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{agg}}
\code{\link{predict.agg}}
}

\examples{
data(weatherc, package="dmr.data")

  # standardization for the weatherc data
w.aggm <- agg.all(play~., weatherc, 1)
}

\keyword{arith}
