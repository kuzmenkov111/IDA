\name{asel.next.forward}
\alias{asel.next.forward}
\alias{asel.next.backward}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 19 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 19.3, Example 19.3.2.}

\arguments{See Section 19.3, Example 19.3.2.}

\details{See Section 19.3, Example 19.3.2.}

\value{See Section 19.3, Example 19.3.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{asel.init.none}}
\code{\link{asel.init.all}}
\code{\link{asel.search.greedy}}
}

\examples{
data(weather, package="dmr.data")

  # attribute selection next state generation for the weather data
asel.next.forward(c("outlook", "humidity"), names(weather)[-5])
asel.next.backward(c("outlook", "humidity"), names(weather)[-5])
}

\keyword{models}
