\name{disc.eqwidth}
\alias{disc.eqwidth}
\alias{disc.eqwidth1}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 18 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 18.3, Example 18.3.1.}

\arguments{See Section 18.3, Example 18.3.1.}

\details{See Section 18.3, Example 18.3.1.}

\value{See Section 18.3, Example 18.3.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{disc.eqfreq}}
\code{\link{disc.pure}}
\code{\link{disc.all}}
\code{\link{predict.disc}}
\code{\link{discnm.eqwidth}}
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

  # equal-width discretization of the temperature attribute in the weatherc data
disc.eqwidth1(weatherc$temperature, 4)

  # equal-width discretization for the weatherc data
disc.eqwidth(play~., weatherc, 3)
disc.eqwidth(play~., weatherc, list(temperature=4, humidity=3))

  # equal-width discretization for the Vehicle Silhouettes data
v.disc.ew <- disc.eqwidth(Class~., v.train, 7)
summary(predict(v.disc.ew, v.train))

  # equal-width discretization for the Glass data
g.disc.ew <- disc.eqwidth(Type~., g.train, 7)
summary(predict(g.disc.ew, g.train))
}

\keyword{arith}
\keyword{models}
