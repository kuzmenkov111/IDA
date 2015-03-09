\name{bs.chisq.test}
\alias{bs.chisq.test}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.5, Example 2.5.3.}

\arguments{See Section 2.5, Example 2.5.3.}

\details{See Section 2.5, Example 2.5.3.}

\value{See Section 2.5, Example 2.5.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{chisq.test}}
}

\examples{
data(weather, package="dmr.data")

bs.chisq.test(weather$outlook, weather$play)
chisq.test(weather$outlook, weather$play)
}

\keyword{univar}
