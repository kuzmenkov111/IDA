\name{predict.k.centers}
\alias{predict.k.centers}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 14 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 14.2, Example 14.2.1.}

\arguments{See Section 14.2, Example 14.2.1.}

\details{See Section 14.2, Example 14.2.1.}

\value{See Section 14.2, Example 14.2.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{k.centers}}
\code{\link{k.centers.adjust.mean}}
\code{\link{k.centers.adjust.median}}
\code{\link{k.centers.adjust.medoid}}
}

\examples{
library(dmr.dissim)
data(weathercl, package="dmr.data")

  # k-means clustering for the weathercl data
wkm <- k.centers(weathercl, 3, adjust=k.centers.adjust.mean)
wkm$centers
wkm$clustering

  # k-centers prediction for the weathercl data
predict(wkm, weathercl)
}

\keyword{models}
