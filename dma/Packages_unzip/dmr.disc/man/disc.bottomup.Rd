\name{disc.bottomup}
\alias{disc.bottomup}
\alias{disc.bottomup1}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 18 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 18.4, Example 18.4.2.}

\arguments{See Section 18.4, Example 18.4.2.}

\details{See Section 18.4, Example 18.4.2.}

\value{See Section 18.4, Example 18.4.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{disc.topdown}}
\code{\link{disc.all}}
\code{\link{predict.disc}}
}

\examples{
data(weatherc, package="dmr.data")

  # bottom-up discretization for the temperature attribute in the weatherc data
disc.bottomup1(weatherc$temperature, weatherc$play, 3,
               evalf=function(b, bl, br, v, class) b)

  # bottom-up discretization for the weatherc data
disc.bottomup(play~., weatherc, 3, evalf=function(b, bl, br, v, class) b)
}

\keyword{arith}
\keyword{models}
