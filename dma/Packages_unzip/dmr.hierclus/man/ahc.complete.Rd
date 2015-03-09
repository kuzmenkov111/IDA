\name{ahc.complete}
\alias{ahc.complete}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 13 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 13.3, Example 13.3.3.}

\arguments{See Section 13.3, Example 13.3.3.}

\details{See Section 13.3, Example 13.3.3.}

\value{See Section 13.3, Example 13.3.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{ahc}}
\code{\link{ahc.single}}
\code{\link{ahc.average}}
\code{\link{ahc.center}}
\code{\link{ahc.ward}}
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

  # agglomerative hierarchical complete-linkage clustering for the weathercl data
wcl.ahc.cl <- ahc(wcl.std, linkf=ahc.complete)

  # agglomerative hierarchical complete-linkage clustering for the iris data
i.ahc.cl <- ahc(i.std.train[,-5], linkf=ahc.complete)

  # agglomerative hierarchical complete-linkage clustering for the Glass data
g.ahc.cl <- ahc(g.std.train[,-10], linkf=ahc.complete)
}

\keyword{models}
