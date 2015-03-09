\name{ahc}
\alias{ahc}
\alias{ahc.size}
\alias{as.hclust.hcl}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 13 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 13.3, Example 13.3.1.}

\arguments{See Section 13.3, Example 13.3.1.}

\details{See Section 13.3, Example 13.3.1.}

\value{See Section 13.3, Example 13.3.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{ahc.single}}
\code{\link{ahc.complete}}
\code{\link{ahc.average}}
\code{\link{ahc.center}}
\code{\link{ahc.ward}}
\code{\link{predict.hcl}}
\code{\link{plot.hcl}}
\code{\link{cut.hcl}}
\code{\link{hclust}}
\code{\link{dendrogram}}
}

\examples{
library(dmr.trans)

data(weathercl, package="dmr.data")
wcl.std <- predict.std(std.all(.~., weathercl), weathercl)

  # agglomerative clustering for the weathercl data
wcl.ahc.d <- ahc(wcl.std, linkf=ahc.size)
as.hclust(wcl.ahc.d)
}

\keyword{models}
