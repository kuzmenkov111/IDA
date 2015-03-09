\name{g.test}
\alias{g.test}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.5, Example 2.5.4.}

\arguments{See Section 2.5, Example 2.5.4.}

\details{See Section 2.5, Example 2.5.4.}

\value{See Section 2.5, Example 2.5.4.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{bs.chisq.test}}
\code{\link{chisq.test}}
}

\examples{
data(weather, package="dmr.data")

g.test(weather$outlook, weather$play)
}

\keyword{multivariate}
