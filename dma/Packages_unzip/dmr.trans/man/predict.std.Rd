\name{predict.std}
\alias{predict.std}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 17 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 17.3, Example 17.3.1.}

\arguments{See Section 17.3, Example 17.3.1.}

\details{See Section 17.3, Example 17.3.1.}

\value{See Section 17.3, Example 17.3.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{std.all}}
\code{\link{predict.nrm}}
}

\examples{
data(weatherc, package="dmr.data")
data(Glass, package="mlbench")

set.seed(12)
rg <- runif(nrow(Glass))
g.train <- Glass[rg>=0.33,]
g.test <- Glass[rg<0.33,]

  # standardization model for the weatherc data
w.stdm <- std.all(play~., weatherc)
  # applied to the weatherc data
w.std <- predict.std(w.stdm, weatherc)

  # standardization model for the Glass data
g.stdm <- std.all(Type~., g.train)
  # applied to the training and test sets
g.train.std <- predict.std(g.stdm, g.train)
g.test.std <- predict.std(g.stdm, g.test)
}

\keyword{arith}
