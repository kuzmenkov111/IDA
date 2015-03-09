\name{evdisc.chisq}
\alias{evdisc.chisq}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 18 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 18.4, Example 18.4.7.}

\arguments{See Section 18.4, Example 18.4.7.}

\details{See Section 18.4, Example 18.4.7.}

\value{See Section 18.4, Example 18.4.7.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{evdisc.entropy}}
\code{\link{evdisc.incount1}}
\code{\link{evdisc.incount2}}
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

  # chi-square bottom-up discretization for the weatherc data
disc.bottomup(play~., weatherc, 3, evalf=evdisc.chisq)

  # chi-square bottom-up discretization for the Vehicle Silhouettes data
v.disc.bu.chi <- disc.bottomup(Class~., v.train, 7, evalf=evdisc.chisq)
summary(predict(v.disc.bu.chi, v.train))

  # chi-square bottom-up discretization for the Glass data
g.disc.bu.chi <- disc.bottomup(Type~., g.train, 7, evalf=evdisc.chisq)
summary(predict(g.disc.bu.chi, g.train))
}

\keyword{arith}
\keyword{models}
