\name{grow.randdectree}
\alias{grow.randdectree}

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
\code{\link{grow.randregtree}}
\code{\link{grow.dectree}}
\code{\link{predict.dectree}}
}

\examples{
library(dmr.dectree)
data("weather", package="dmr.data")

rdtree1 <- grow.randdectree(play~., weather)
rdtree2 <- grow.randdectree(play~., weather)
rdtree3 <- grow.randdectree(play~., weather)

as.data.frame(rdtree1)
as.data.frame(rdtree2)
as.data.frame(rdtree3)
}

\keyword{models}
