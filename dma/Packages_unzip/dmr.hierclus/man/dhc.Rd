\name{dhc}
\alias{dhc}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 13 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 13.4, Example 13.4.1.}

\arguments{See Section 13.4, Example 13.4.1.}

\details{See Section 13.4, Example 13.4.1.}

\value{See Section 13.4, Example 13.4.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{ahc}}
\code{\link{predict.hcl}}
\code{\link{plot.hcl}}
\code{\link{cut.hcl}}
\code{\link{pam}}
\code{\link{dendrogram}}
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

  # divisive clustering for the weathercl data
wcl.dhc <- dhc(wcl.std)

  # divisive hierarchical clustering for the iris data
i.dhc <- dhc(i.std.train[,-5])
i.dhc.km <- dhc(i.std.train[,-5], alg=kmeans, cls="cluster", cnt="centers")
i.dhc.d3 <- dhc(i.std.train[,-5], maxdepth=3)
i.dhc.km.d3 <- dhc(i.std.train[,-5], alg=kmeans, cls="cluster", cnt="centers",
                   maxdepth=3)

  # divisive hierarchical clustering for the Glass data
g.dhc <- dhc(g.std.train[,-10]  )
g.dhc.km <- dhc(g.std.train[,-10], alg=kmeans, cls="cluster", cnt="centers")
g.dhc.d3 <- dhc(g.std.train[,-10], maxdepth=3)
g.dhc.km.d3 <- dhc(g.std.train[,-10], alg=kmeans, cls="cluster", cnt="centers",
                   maxdepth=3)
}

\keyword{models}
