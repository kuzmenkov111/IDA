\name{asel.search.greedy}
\alias{asel.search.greedy}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 19 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 19.3, Example 19.3.3.}

\arguments{See Section 19.3, Example 19.3.3.}

\details{See Section 19.3, Example 19.3.3.}

\value{See Section 19.3, Example 19.3.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{asel.init.none}}
\code{\link{asel.init.all}}
\code{\link{asel.next.forward}}
\code{\link{asel.next.backward}}
}

\examples{
data(weather, package="dmr.data")

  # greedy attribute selection search for the weather data
asel.search.greedy(names(weather)[-5], "play", evalf=function(subset, target) 1)
asel.search.greedy(names(weather)[-5], "play", evalf=function(subset, target) 1,
                   penalty=0)
asel.search.greedy(names(weather)[-5], "play", evalf=function(subset, target) 1,
                   initf=asel.init.none, nextf=asel.next.forward)
asel.search.greedy(names(weather)[-5], "play", evalf=function(subset, target) 1,
                   initf=asel.init.none, nextf=asel.next.forward, penalty=0)
}

\keyword{models}
