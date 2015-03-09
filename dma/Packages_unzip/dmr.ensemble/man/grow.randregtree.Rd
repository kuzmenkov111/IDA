\name{grow.randregtree}
\alias{grow.randregtree}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 15 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 15.3, Example 15.3.4.}

\arguments{See Section 15.3, Example 15.3.4.}

\details{See Section 15.3, Example 15.3.4.}

\value{See Section 15.3, Example 15.3.4.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{grow.randdectree}}
\code{\link{grow.regtree}}
\code{\link{predict.regtree}}
}

\examples{
library(dmr.regtree)
data("weatherr", package="dmr.data")

rrtree1 <- grow.randregtree(playability~., weatherr)
rrtree2 <- grow.randregtree(playability~., weatherr)
rrtree3 <- grow.randregtree(playability~., weatherr)

as.data.frame(rrtree1)
as.data.frame(rrtree2)
as.data.frame(rrtree3)
}

\keyword{models}
