\name{node.card}
\alias{node.card}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 3 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 3.4, Example 3.4.3.}

\arguments{See Section 3.4, Example 3.4.3.}

\details{See Section 3.4, Example 3.4.3.}

\value{See Section 3.4, Example 3.4.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{rpart}}
\code{\link{extract.rule}}
\code{\link{leaf.error}}
\code{\link{node.error}}
}

\examples{
library(rpart)
data(weather, package="dmr.data")
rptree <- rpart(play~., weather, minsplit=2)

node.card(rptree, 1, weather)
node.card(rptree, 2, weather)
node.card(rptree, 3, weather)
node.card(rptree, 1, weather[1:7,])
node.card(rptree, 2, weather[1:7,])
node.card(rptree, 3, weather[1:7,])
}

\keyword{models}
