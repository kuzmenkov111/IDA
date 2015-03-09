\name{nrm.all}
\alias{nrm.all}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 17 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 17.3, Example 17.3.2.}

\arguments{See Section 17.3, Example 17.3.2.}

\details{See Section 17.3, Example 17.3.2.}

\value{See Section 17.3, Example 17.3.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{nrm}}
\code{\link{predict.nrm}}
\code{\link{std.all}}
}

\examples{
data(weatherc, package="dmr.data")
data(Glass, package="mlbench")

set.seed(12)
rg <- runif(nrow(Glass))
g.train <- Glass[rg>=0.33,]
g.test <- Glass[rg<0.33,]

  # normalization model for the weatherc data
w.nrmm <- nrm.all(play~., weatherc)
  # normalization model for the Glass data
g.nrmm <- nrm.all(Type~., g.train)
}

\keyword{arith}
