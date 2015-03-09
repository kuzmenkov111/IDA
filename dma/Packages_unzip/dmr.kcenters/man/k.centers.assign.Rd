\name{k.centers.assign}
\alias{k.centers.assign}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 12 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 12.2, Example 12.2.1.}

\arguments{See Section 12.2, Example 12.2.1.}

\details{See Section 12.2, Example 12.2.1.}

\value{See Section 12.2, Example 12.2.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{k.centers}}
}

\examples{
library(dmr.dissim)
data(weathercl, package="dmr.data")

  # dummy k-centers clustering for the weathercl data
k.centers(weathercl, 3)
}

\keyword{models}
