\name{pcond}
\alias{pcond}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.23.}

\arguments{See Section 2.4, Example 2.4.23.}

\details{See Section 2.4, Example 2.4.23.}

\value{See Section 2.4, Example 2.4.23.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{prob}}
\code{\link{pdisc}}
}

\examples{
data(weather, package="dmr.data")

pcond(weather$outlook, weather$play)
}

\keyword{multivariate}
