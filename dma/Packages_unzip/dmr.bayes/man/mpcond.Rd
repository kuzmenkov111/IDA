\name{mpcond}
\alias{mpcond}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 4 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 4.4, Example 4.4.1.}

\arguments{See Section 4.4, Example 4.4.1.}

\details{See Section 4.4, Example 4.4.1.}

\value{See Section 4.4, Example 4.4.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{mest}}
\code{\link{mprob}}
\code{\link{pcond}}
}

\examples{
library(dmr.stats)
data(weather, package="dmr.data")

pcond(weather$outlook, weather$play)
mpcond(weather$outlook, weather$play)
mpcond(weather$outlook, weather$play, m=0)
mpcond(weather$outlook, weather$play, m=1)
}

\keyword{multivariate}
