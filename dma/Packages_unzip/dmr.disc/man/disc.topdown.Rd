\name{disc.topdown}
\alias{disc.topdown}
\alias{disc.topdown1}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 18 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 18.4, Example 18.4.8.}

\arguments{See Section 18.4, Example 18.4.8.}

\details{See Section 18.4, Example 18.4.8.}

\value{See Section 18.4, Example 18.4.8.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{disc.bottomup}}
\code{\link{disc.all}}
\code{\link{predict.disc}}
}

\examples{
data(weatherc, package="dmr.data")

  # top-down discretization of the temperature attribute in the weatherc data
disc.topdown1(weatherc$temperature, weatherc$play, 3,
              evalf=function(b, bl, br, v, class) b)

  # top-down discretization for the weatherc data
disc.topdown(play~., weatherc, 3, evalf=function(b, bl, br, v, class) b)
}

\keyword{arith}
\keyword{models}
