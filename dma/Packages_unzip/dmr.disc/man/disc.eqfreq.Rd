\name{disc.eqfreq}
\alias{disc.eqfreq}
\alias{disc.eqfreq1}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 18 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 18.3, Example 18.3.2.}

\arguments{See Section 18.3, Example 18.3.2.}

\details{See Section 18.3, Example 18.3.2.}

\value{See Section 18.3, Example 18.3.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{disc.eqwidth}}
\code{\link{disc.pure}}
\code{\link{disc.all}}
\code{\link{predict.disc}}
\code{\link{discnm.eqfreq}}
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
disc.eqfreq1(weatherc$temperature, 4)

  # equal-frequency discretization for the weatherc data
disc.eqfreq(play~., weatherc, 3)
disc.eqfreq(play~., weatherc, list(temperature=4, humidity=3))

  # equal-frequency discretization for the Vehicle Silhouettes data
v.disc.ef <- disc.eqfreq(Class~., v.train, 7)
summary(predict(v.disc.ef, v.train))

  # equal-frequency discretization for the Glass data
g.disc.ef <- disc.eqfreq(Type~., g.train, 7)
summary(predict(g.disc.ef, g.train))
}

\keyword{arith}
\keyword{models}
