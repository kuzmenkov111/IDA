\name{entropy.cond}
\alias{entropy.cond}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.5, Example 2.5.5.}

\arguments{See Section 2.5, Example 2.5.5.}

\details{See Section 2.5, Example 2.5.5.}

\value{See Section 2.5, Example 2.5.5.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{entropy}}
\code{\link{mutinfo}}
\code{\link{symunc}}
}

\examples{
data(weather, package="dmr.data")

entropy.cond(weather$play, weather$outlook)
entropy.cond(weather$play, weather$outlook=="rainy")
}

\keyword{multivariate}
