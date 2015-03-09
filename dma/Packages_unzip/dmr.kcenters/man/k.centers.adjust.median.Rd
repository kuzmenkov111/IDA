\name{k.centers.adjust.median}
\alias{k.centers.adjust.median}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 12 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 12.4, Example 12.4.1.}

\arguments{See Section 12.4, Example 12.4.1.}

\details{See Section 12.4, Example 12.4.1.}

\value{See Section 12.4, Example 12.4.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{k.centers}}
\code{\link{k.centers.adjust.dummy}}
\code{\link{k.centers.adjust.mean}}
\code{\link{k.centers.adjust.medoid}}
}

\examples{
library(dmr.dissim)
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

  # k-medians clustering
w.kmedians <- k.centers(wcl.std, 3, adjust=k.centers.adjust.median)
w.kmedians$centers

i.kmedians <- k.centers(i.std.train[,-5], 3, adjust=k.centers.adjust.median)
g.kmedians <- k.centers(g.std.train[,-10], 7, adjust=k.centers.adjust.median)

  # k-medians prediction
w.kmedians$clustering
predict(w.kmedians, wcl.std)

i.pred.kmedians <- predict(i.kmedians, i.std.test[,-5])
g.pred.kmedians <- predict(g.kmedians, g.std.test[,-10])

  # clusters vs. classes on the training set
table(i.kmedians$clustering, i.std.train$Species)
table(g.kmedians$clustering, g.std.train$Type)

  # clusters vs. classes on the test set
table(predict(i.kmedians, i.std.test[,-5]), i.std.test$Species)
table(predict(g.kmedians, g.std.test[,-10]), g.std.test$Type)

  # attribute distribution within clusters for the Iris data
par(mfrow=c(2, 2))
for (attr in names(i.std.train)[1:4])
  boxplot(i.std.train[[attr]]~i.kmedians$clustering, main=attr)
}

\keyword{models}
