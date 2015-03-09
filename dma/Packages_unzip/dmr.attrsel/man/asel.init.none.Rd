\name{asel.init.none}
\alias{asel.init.none}
\alias{asel.init.all}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 19 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 19.3, Example 19.3.1.}

\arguments{See Section 19.3, Example 19.3.1.}

\details{See Section 19.3, Example 19.3.1.}

\value{See Section 19.3, Example 19.3.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{asel.next.forward}}
\code{\link{asel.next.backward}}
\code{\link{asel.search.greedy}}
}

\examples{
data(weather, package="dmr.data")

  # attribute selection search initialization for the weather data
asel.init.none(names(weather)[-5])
asel.init.all(names(weather)[-5])
}

\keyword{models}
