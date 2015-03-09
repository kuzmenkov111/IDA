\name{predict.hcl}
\alias{predict.hcl}
\alias{predict.ahc}
\alias{predict.dhc}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 13 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 13.6, Example 13.6.2.}

\arguments{See Section 13.6, Example 13.6.2.}

\details{See Section 13.6, Example 13.6.2.}

\value{See Section 13.6, Example 13.6.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{ahc}}
\code{\link{dhc}}
\code{\link{cut.hcl}}
}

\examples{
library(dmr.trans)

data(weathercl, package="dmr.data")
data(iris)
data(Glass, package="mlbench")

set.seed(12)

ri <- runif(nrow(iris))
i.train <- iris[ri>=0.33,]
i.test <- iris[ri<0.33,]

rg <- runif(nrow(Glass))
g.train <- Glass[rg>=0.33,]
g.test <- Glass[rg<0.33,]

wcl.std <- predict.std(std.all(.~., weathercl), weathercl)

i.stdm <- std.all(Species~., i.train)
i.std.train <- predict.std(i.stdm, i.train)
i.std.test <- predict.std(i.stdm, i.test)

g.stdm <- std.all(Type~., g.train)
g.std.train <- predict.std(g.stdm, g.train)
g.std.test <- predict.std(g.stdm, g.test)

wcl.ahc.cl <- ahc(wcl.std, linkf=ahc.complete)
wcl.dhc <- dhc(wcl.std)

i.ahc.sl <- ahc(i.std.train[,-5], linkf=ahc.single)
i.ahc.cl <- ahc(i.std.train[,-5], linkf=ahc.complete)
i.ahc.al <- ahc(i.std.train[,-5], linkf=ahc.average)
i.ahc.ml <- ahc(i.std.train[,-5], linkf=ahc.center)
i.ahc.wl <- ahc(i.std.train[,-5], linkf=ahc.ward)
i.dhc <- dhc(i.std.train[,-5])
i.dhc.d3 <- dhc(i.std.train[,-5], maxdepth=3)

i.ahc.sl.cd3 <- cut(i.ahc.sl, max(i.dhc.d3$clustering))
i.ahc.cl.cd3 <- cut(i.ahc.cl, max(i.dhc.d3$clustering))
i.ahc.al.cd3 <- cut(i.ahc.al, max(i.dhc.d3$clustering))
i.ahc.ml.cd3 <- cut(i.ahc.ml, max(i.dhc.d3$clustering))
i.ahc.wl.cd3 <- cut(i.ahc.wl, max(i.dhc.d3$clustering))
i.dhc.cd3 <- cut(i.dhc, max(i.dhc.d3$clustering))

g.ahc.sl <- ahc(g.std.train[,-10], linkf=ahc.single)
g.ahc.cl <- ahc(g.std.train[,-10], linkf=ahc.complete)
g.ahc.al <- ahc(g.std.train[,-10], linkf=ahc.average)
g.ahc.ml <- ahc(g.std.train[,-10], linkf=ahc.center)
g.ahc.wl <- ahc(g.std.train[,-10], linkf=ahc.ward)
g.dhc <- dhc(g.std.train[,-10]  )
g.dhc.d3 <- dhc(g.std.train[,-10], maxdepth=3)

g.ahc.sl.cd3 <- cut(g.ahc.sl, max(g.dhc.d3$clustering))
g.ahc.cl.cd3 <- cut(g.ahc.cl, max(g.dhc.d3$clustering))
g.ahc.al.cd3 <- cut(g.ahc.al, max(g.dhc.d3$clustering))
g.ahc.ml.cd3 <- cut(g.ahc.ml, max(g.dhc.d3$clustering))
g.ahc.wl.cd3 <- cut(g.ahc.wl, max(g.dhc.d3$clustering))
g.dhc.cd3 <- cut(g.dhc, max(g.dhc.d3$clustering))

  # hierarchical clustering prediction for the weathercl data
predict(wcl.ahc.cl, wcl.std)
predict(wcl.dhc, wcl.std)

  # hierarchical clustering prediction for the iris data
i.ahc.cl.cd3.pred <- predict(i.ahc.cl.cd3, i.std.test[,-5])
i.ahc.sl.cd3.pred <- predict(i.ahc.sl.cd3, i.std.test[,-5])
i.ahc.al.cd3.pred <- predict(i.ahc.al.cd3, i.std.test[,-5])
i.ahc.ml.cd3.pred <- predict(i.ahc.ml.cd3, i.std.test[,-5])
i.ahc.wl.cd3.pred <- predict(i.ahc.wl.cd3, i.std.test[,-5])
i.dhc.cd3.pred <- predict(i.dhc.cd3, i.std.test[,-5])

  # hierarchical clustering prediction for the Glass data
g.ahc.cl.cd3.pred <- predict(g.ahc.cl.cd3, g.std.test[,-10])
g.ahc.sl.cd3.pred <- predict(g.ahc.sl.cd3, g.std.test[,-10])
g.ahc.al.cd3.pred <- predict(g.ahc.al.cd3, g.std.test[,-10])
g.ahc.ml.cd3.pred <- predict(g.ahc.ml.cd3, g.std.test[,-10])
g.ahc.wl.cd3.pred <- predict(g.ahc.wl.cd3, g.std.test[,-10])
g.dhc.cd3.pred <- predict(g.dhc.cd3, g.std.test[,-10])
}

\keyword{models}
