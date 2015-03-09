\name{evdisc.dccount1}
\alias{evdisc.dccount1}
\alias{evdisc.dccount2}
\alias{evdisc.dccount3}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 18 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 18.4, Example 18.4.4.}

\arguments{See Section 18.4, Example 18.4.4.}

\details{See Section 18.4, Example 18.4.4.}

\value{See Section 18.4, Example 18.4.4.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{evdisc.chisq}}
\code{\link{evdisc.entropy}}
\code{\link{evdisc.incount1}}
\code{\link{evdisc.incount2}}
\code{\link{evdisc.mcount}}
\code{\link{disc.bottomup}}
\code{\link{disc.topdown}}
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

  # dominating class count bottom-up discretization for the weatherc data
disc.bottomup(play~., weatherc, 3, evalf=evdisc.dccount1)
disc.bottomup(play~., weatherc, 3, evalf=evdisc.dccount2)
disc.bottomup(play~., weatherc, 3, evalf=evdisc.dccount3)

  # dominating class count bottom-up discretization for the Vehicle Silhouettes data
v.disc.bu.dc1 <- disc.bottomup(Class~., v.train, 7, evalf=evdisc.dccount1)
v.disc.bu.dc2 <- disc.bottomup(Class~., v.train, 7, evalf=evdisc.dccount2)
v.disc.bu.dc3 <- disc.bottomup(Class~., v.train, 7, evalf=evdisc.dccount3)
summary(predict.disc(v.disc.bu.dc1, v.train))
summary(predict.disc(v.disc.bu.dc2, v.train))
summary(predict.disc(v.disc.bu.dc3, v.train))

  # dominating class count bottom-up discretization for the Glass data
g.disc.bu.dc1 <- disc.bottomup(Type~., g.train, 7, evalf=evdisc.dccount1)
g.disc.bu.dc2 <- disc.bottomup(Type~., g.train, 7, evalf=evdisc.dccount2)
g.disc.bu.dc3 <- disc.bottomup(Type~., g.train, 7, evalf=evdisc.dccount3)
summary(predict.disc(v.disc.bu.dc1, g.train))
summary(predict.disc(v.disc.bu.dc2, g.train))
summary(predict.disc(v.disc.bu.dc3, g.train))
}

\keyword{arith}
\keyword{models}
