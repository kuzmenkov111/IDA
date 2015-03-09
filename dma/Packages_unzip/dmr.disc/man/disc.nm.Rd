\name{disc.nm}
\alias{disc.nm}
\alias{discnm.eqfreq}
\alias{discnm.eqwidth}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 18 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 18.3, Example 18.3.3.}

\arguments{See Section 18.3, Example 18.3.3.}

\details{See Section 18.3, Example 18.3.3.}

\value{See Section 18.3, Example 18.3.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{disc.eqwidth}}
\code{\link{disc.eqfreq}}
\code{\link{disc.all}}
\code{\link{predict.disc}}
}

\examples{
data(weatherc, package="dmr.data")

  # non-modeling discretization for the weatherc data
discnm.eqwidth(play~., weatherc, 4)
discnm.eqfreq(play~., weatherc, 3)
discnm.eqfreq(play~., weatherc, list(temperature=4, humidity=3))
}

\keyword{arith}
\keyword{models}
