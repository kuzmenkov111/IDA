\name{mmean}
\alias{mmean}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.33.}

\arguments{See Section 2.4, Example 2.4.33.}

\details{See Section 2.4, Example 2.4.33.}

\value{See Section 2.4, Example 2.4.33.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{mprob}}
\code{\link{mest}}
\code{\link{mvar}}
}

\examples{
data(weatherr, package="dmr.data")

mmean(weatherr$playability)
mmean(weatherr$playability, m=0)
mmean(weatherr$playability, m0=0.5)
mmean(weatherr$playability, 5, 0.5)
mmean(weatherr$playability[weatherr$temperature<25], m=0)
mmean(weatherr$playability[weatherr$temperature<25], m0=mean(weatherr$playability))
}

\keyword{univar}
