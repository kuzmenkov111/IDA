\name{cut.hcl}
\alias{cut.hcl}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 13 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 13.6, Example 13.6.1.}

\arguments{See Section 13.6, Example 13.6.1.}

\details{See Section 13.6, Example 13.6.1.}

\value{See Section 13.6, Example 13.6.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{ahc}}
\code{\link{dhc}}
\code{\link{plot.hcl}}
\code{\link{predict.hcl}}
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
wcl.ahc.sl <- ahc(wcl.std, linkf=ahc.single)
wcl.dhc <- dhc(wcl.std)

i.ahc.sl <- ahc(i.std.train[,-5], linkf=ahc.single)
i.ahc.cl <- ahc(i.std.train[,-5], linkf=ahc.complete)
i.ahc.al <- ahc(i.std.train[,-5], linkf=ahc.average)
i.ahc.ml <- ahc(i.std.train[,-5], linkf=ahc.center)
i.ahc.wl <- ahc(i.std.train[,-5], linkf=ahc.ward)
i.dhc <- dhc(i.std.train[,-5])
i.dhc.d3 <- dhc(i.std.train[,-5], maxdepth=3)

g.ahc.sl <- ahc(g.std.train[,-10], linkf=ahc.single)
g.ahc.cl <- ahc(g.std.train[,-10], linkf=ahc.complete)
g.ahc.al <- ahc(g.std.train[,-10], linkf=ahc.average)
g.ahc.ml <- ahc(g.std.train[,-10], linkf=ahc.center)
g.ahc.wl <- ahc(g.std.train[,-10], linkf=ahc.ward)
g.dhc <- dhc(g.std.train[,-10]  )
g.dhc.d3 <- dhc(g.std.train[,-10], maxdepth=3)

  # cutting hierarchical clustering trees for the weathercl data
wcl.ahc.sl.c4 <- cut(wcl.ahc.sl, 4)
wcl.ahc.cl.c4 <- cut(wcl.ahc.cl, 4)
wcl.dhc.c4 <- cut(wcl.dhc, 4)

  # cutting hierarchical clustering trees for the iris data
i.ahc.sl.cd3 <- cut(i.ahc.sl, max(i.dhc.d3$clustering))
i.ahc.cl.cd3 <- cut(i.ahc.cl, max(i.dhc.d3$clustering))
i.ahc.al.cd3 <- cut(i.ahc.al, max(i.dhc.d3$clustering))
i.ahc.ml.cd3 <- cut(i.ahc.ml, max(i.dhc.d3$clustering))
i.ahc.wl.cd3 <- cut(i.ahc.wl, max(i.dhc.d3$clustering))
i.dhc.cd3 <- cut(i.dhc, max(i.dhc.d3$clustering))
  # verify i.dhc.cd3 and i.dhc.d3 are the same
all(i.dhc.cd3$clustering==i.dhc.d3$clustering)
all(i.dhc.cd3$merge==i.dhc.d3$merge)
all(sapply(1:length(i.dhc.cd3$centers),
           function(d) all(i.dhc.cd3$centers[[d]]==i.dhc.d3$centers[[d]])))

  # cutting hierarchical clustering trees for the Glass data
g.ahc.sl.cd3 <- cut(g.ahc.sl, max(g.dhc.d3$clustering))
g.ahc.cl.cd3 <- cut(g.ahc.cl, max(g.dhc.d3$clustering))
g.ahc.al.cd3 <- cut(g.ahc.al, max(g.dhc.d3$clustering))
g.ahc.ml.cd3 <- cut(g.ahc.ml, max(g.dhc.d3$clustering))
g.ahc.wl.cd3 <- cut(g.ahc.wl, max(g.dhc.d3$clustering))
g.dhc.cd3 <- cut(g.dhc, max(g.dhc.d3$clustering))
  # verify g.dhc.cd3 and g.dhc.d3 are the same
all(g.dhc.cd3$clustering==g.dhc.d3$clustering)
all(g.dhc.cd3$merge==g.dhc.d3$merge)
all(sapply(1:length(g.dhc.cd3$centers),
           function(d) all(g.dhc.cd3$centers[[d]]==g.dhc.d3$centers[[d]])))
}

\keyword{models}
