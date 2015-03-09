\name{modal}
\alias{modal}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.19.}

\arguments{See Section 2.4, Example 2.4.19.}

\details{See Section 2.4, Example 2.4.19.}

\value{See Section 2.4, Example 2.4.19.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{weighted.modal}}
}

\examples{
data(weather, package="dmr.data")
data(weatherr, package="dmr.data")

modal(weather$outlook)
modal(weatherr$temperature)
}

\keyword{multivariate}
