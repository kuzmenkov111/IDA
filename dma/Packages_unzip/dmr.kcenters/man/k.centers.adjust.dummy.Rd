\name{k.centers.adjust.dummy}
\alias{k.centers.adjust.dummy}

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
\code{\link{k.centers.adjust.mean}}
\code{\link{k.centers.adjust.median}}
\code{\link{k.centers.adjust.medoid}}
}

\examples{
library(dmr.dissim)
library(dmr.trans)
data(weathercl, package="dmr.data")

wcl.std <- predict.std(std.all(.~., weathercl), weathercl)

  # dummy k-centers clustering for the weathercl data
k.centers(wcl.std, 3)
}

\keyword{models}
