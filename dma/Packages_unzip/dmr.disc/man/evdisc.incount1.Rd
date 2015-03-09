\name{evdisc.incount1}
\alias{evdisc.incount1}
\alias{evdisc.incount2}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 18 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 18.4, Example 18.4.3.}

\arguments{See Section 18.4, Example 18.4.3.}

\details{See Section 18.4, Example 18.4.3.}

\value{See Section 18.4, Example 18.4.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{evdisc.chisq}}
\code{\link{evdisc.entropy}}
\code{\link{evdisc.dccount1}}
\code{\link{evdisc.dccount2}}
\code{\link{evdisc.dccount3}}
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

  # instance count bottom-up discretization for the weatherc data
disc.bottomup(play~., weatherc, 3, evalf=evdisc.incount1)
disc.bottomup(play~., weatherc, 3, evalf=evdisc.incount2)

  # instance count bottom-up discretization for the Vehicle Silhouettes data
v.disc.bu.ic1 <- disc.bottomup(Class~., v.train, 7, evalf=evdisc.incount1)
v.disc.bu.ic2 <- disc.bottomup(Class~., v.train, 7, evalf=evdisc.incount2)
summary(predict(v.disc.bu.ic1, v.train))
summary(predict(v.disc.bu.ic2, v.train))

  # instance count bottom-up discretization for the Glass data
g.disc.bu.ic1 <- disc.bottomup(Type~., g.train, 7, evalf=evdisc.incount1)
g.disc.bu.ic2 <- disc.bottomup(Type~., g.train, 7, evalf=evdisc.incount2)
summary(predict(g.disc.bu.ic1, g.train))
summary(predict(g.disc.bu.ic2, g.train))
}

\keyword{arith}
\keyword{models}
