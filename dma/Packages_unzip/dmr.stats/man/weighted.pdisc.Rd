\name{weighted.pdisc}
\alias{weighted.pdisc}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.25.}

\arguments{See Section 2.4, Example 2.4.25.}

\details{See Section 2.4, Example 2.4.25.}

\value{See Section 2.4, Example 2.4.25.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{pdisc}}
\code{\link{weighted.modal}}
\code{\link{prob}}
\code{\link{pcond}}
}

\examples{
data(weather, package="dmr.data")

weighted.pdisc(weather$outlook, w=ifelse(weather$play=="yes", 2, 1))
weighted.pdisc(weather$outlook, weather$temperature,
               w=ifelse(weather$play=="yes", 2, 1))
}

\keyword{univar}
\keyword{multivariate}
