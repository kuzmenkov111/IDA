\name{leaf.mep.error}
\alias{leaf.mep.error}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 3 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 3.4, Example 3.4.5.}

\arguments{See Section 3.4, Example 3.4.5.}

\details{See Section 3.4, Example 3.4.5.}

\value{See Section 3.4, Example 3.4.5.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{rpart}}
\code{\link{leaf.error}}
\code{\link{node.mep.error}}
}

\examples{
library(rpart)
data(weather, package="dmr.data")
rptree <- rpart(play~., weather, minsplit=2)

  # MEP error of node 1, if treated as a leaf, for m=0, 2, 5
leaf.mep.error(rptree, 1, weather, weather$play, m=0)
leaf.mep.error(rptree, 1, weather, weather$play, m=2)
leaf.mep.error(rptree, 1, weather, weather$play, m=5)
  # MEP error of node 3, which is actually a leaf, for m=0, 2, 5
leaf.mep.error(rptree, 3, weather, weather$play, m=0)
leaf.mep.error(rptree, 3, weather, weather$play, m=2)
leaf.mep.error(rptree, 1, weather, weather$play, m=5)
  # MEP error of node 4, if treated as a leaf, for m=0, 2, 5
leaf.mep.error(rptree, 4, weather, weather$play, m=0)
leaf.mep.error(rptree, 4, weather, weather$play, m=2)
leaf.mep.error(rptree, 4, weather, weather$play, m=5)
}

\keyword{models}
