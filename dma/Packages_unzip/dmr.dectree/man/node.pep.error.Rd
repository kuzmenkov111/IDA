\name{node.pep.error}
\alias{node.pep.error}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 3 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 3.4, Example 3.4.4.}

\arguments{See Section 3.4, Example 3.4.4.}

\details{See Section 3.4, Example 3.4.4.}

\value{See Section 3.4, Example 3.4.4.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{rpart}}
\code{\link{node.error}}
\code{\link{node.mep.error}}
}

\examples{
library(rpart)
data(weather, package="dmr.data")
rptree <- rpart(play~., weather, minsplit=2)

  # PEP error of node 1
node.pep.error(rptree, 1, weather, weather$play)
  # PEP error of node 4
node.pep.error(rptree, 1, weather, weather$play)
}

\keyword{models}
