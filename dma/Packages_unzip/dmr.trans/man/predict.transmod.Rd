\name{predict.transmod}
\alias{predict.transmod}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 17 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 17.2, Example 17.2.1.}

\arguments{See Section 17.2, Example 17.2.1.}

\details{See Section 17.2, Example 17.2.1.}

\value{See Section 17.2, Example 17.2.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{transmod.all}}
}

\examples{
data(weatherc, package="dmr.data")

  # simple centering (mean subtraction) transformation
center.m <- transmod.all(mean, is.numeric)
  # performed on the weatherc data
w.cm <- center.m(play~., weatherc)
  # applied to the weatherc data
predict.center.m <- predict.transmod(function(m, v) v-m)
predict.center.m(w.cm, weatherc)
}

\keyword{arith}
