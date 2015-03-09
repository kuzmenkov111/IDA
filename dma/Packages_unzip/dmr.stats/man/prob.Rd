\name{prob}
\alias{prob}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.21.}

\arguments{See Section 2.4, Example 2.4.21.}

\details{See Section 2.4, Example 2.4.21.}

\value{See Section 2.4, Example 2.4.21.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{pdisc}}
\code{\link{pcond}}
\code{\link{weighted.prob}}
}

\examples{
data(weather, package="dmr.data")

prob(weather$outlook, "rainy")
}

\keyword{univar}
