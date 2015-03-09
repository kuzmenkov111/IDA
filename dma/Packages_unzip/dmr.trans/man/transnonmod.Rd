\name{transnonmod.all}
\alias{transnonmod.all}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 17 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 17.2, Example 17.2.2.}

\arguments{See Section 17.2, Example 17.2.2.}

\details{See Section 17.2, Example 17.2.2.}

\value{See Section 17.2, Example 17.2.2.}

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
center.nm <- transnonmod.all(function(v) v-mean(v), is.numeric)
  # performed on the weatherc data
center.nm(play~., weatherc)

  # simple round to a multiple of 10 transformation
divmod10 <- transnonmod.all(function(v) cbind(v \%/\% 10, v \%\% 10), is.numeric)
  # performed on the weatherc data
divmod10(play~., weatherc)
}

\keyword{arith}
