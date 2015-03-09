\name{disc.all}
\alias{disc.all}
\alias{predict.disc}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 18 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 18.2, Example 18.2.1.}

\arguments{See Section 18.2, Example 18.2.1.}

\details{See Section 18.2, Example 18.2.1.}

\value{See Section 18.2, Example 18.2.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{disc.eqfreq}}
\code{\link{disc.eqwidth}}
\code{\link{disc.pure}}
\code{\link{disc.bottomup}}
\code{\link{disc.topdown}}
\code{\link{disc.nm}}
}

\examples{
data(weatherc, package="dmr.data")

  # random all-attributes discretization
disc.rand <- disc.all(function(v, k=3) sort(runif(k-1, min=min(v), max=max(v))))

  # random discretization for the weatherc data
w.dr3m <- disc.rand(play~., weatherc, 3)
w.dr43m <- disc.rand(play~., weatherc, list(temperature=4, humidity=3))

  # apply discretization breaks to the weatherc data
w.dr3 <- predict(w.dr3m, weatherc)
w.dr43 <- predict(w.dr43m, weatherc)
}

\keyword{arith}
\keyword{models}
