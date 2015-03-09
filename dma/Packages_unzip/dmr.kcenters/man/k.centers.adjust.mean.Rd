\name{k.centers.adjust.mean}
\alias{k.centers.adjust.mean}
\alias{attr.mm}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 12 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 12.3, Example 12.3.1.}

\arguments{See Section 12.3, Example 12.3.1.}

\details{See Section 12.3, Example 12.3.1.}

\value{See Section 12.3, Example 12.3.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{k.centers}}
\code{\link{k.centers.adjust.dummy}}
\code{\link{k.centers.adjust.median}}
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

  # k-means clustering
w.kmeans <- k.centers(wcl.std, 3, adjust=k.centers.adjust.mean)
w.kmeans$centers

i.kmeans <- k.centers(i.std.train[,-5], 3, adjust=k.centers.adjust.mean)
g.kmeans <- k.centers(g.std.train[,-10], 7, adjust=k.centers.adjust.mean)

  # k-means prediction
w.kmeans$clustering
predict(w.kmeans, wcl.std)

i.pred.kmeans <- predict(i.kmeans, i.std.test[,-5])
g.pred.kmeans <- predict(g.kmeans, g.std.test[,-10])

  # clusters vs. classes on the training set
table(i.kmeans$clustering, i.std.train$Species)
table(g.kmeans$clustering, g.std.train$Type)

  # clusters vs. classes on the test set
table(predict(i.kmeans, i.std.test[,-5]), i.std.test$Species)
table(predict(g.kmeans, g.std.test[,-10]), g.std.test$Type)

  # attribute distribution within clusters for the Iris data
par(mfrow=c(2, 2))
for (attr in names(i.std.train)[1:4])
  boxplot(i.std.train[[attr]]~i.kmeans$clustering, main=attr)
}

\keyword{models}
