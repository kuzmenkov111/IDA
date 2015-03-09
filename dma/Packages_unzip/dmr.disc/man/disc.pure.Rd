\name{disc.pure}
\alias{disc.pure}
\alias{disc.pure1}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 18 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 18.4, Example 18.4.1.}

\arguments{See Section 18.4, Example 18.4.1.}

\details{See Section 18.4, Example 18.4.1.}

\value{See Section 18.4, Example 18.4.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{disc.eqwidth}}
\code{\link{disc.eqfreq}}
\code{\link{disc.all}}
\code{\link{predict.disc}}
}

\examples{
data(weatherc, package="dmr.data")
data(Vehicle, package="mlbench")
data(Glass, package="mlbench")

set.seed(12)

rv <- runif(nrow(Vehicle))
v.train <- Vehicle[rv>=0.33,]
v.test <- Vehicle[rv<0.33,]

rg <- runif(nrow(Glass))
g.train <- Glass[rg>=0.33,]
g.test <- Glass[rg<0.33,]

  # pure-class discretization for the weatherc data
disc.pure(play~., weatherc)

  # pure-class discretization for the Vehicle Silhouettes data
v.disc.p <- disc.pure(Class~., v.train)
summary(predict(v.disc.p, v.train), maxsum=100)

  # pure-class discretization for the Glass data
g.disc.p <- disc.pure(Type~., g.train)
summary(predict(g.disc.p, g.train), maxsum=100)
}

\keyword{arith}
\keyword{models}
