\name{randnaiveBayes}
\alias{randnaiveBayes}
\alias{predict.randnaiveBayes}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 15 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 15.5, Example 15.5.6.}

\arguments{See Section 15.5, Example 15.5.6.}

\details{See Section 15.5, Example 15.5.6.}

\value{See Section 15.5, Example 15.5.6.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{randforest}}
\code{\link{adaboost}}
\code{\link{bagging}}
\code{\link{gradboost}}
\code{\link{predict.ensemble.prob}}
}

\examples{
library(dmr.claseval)
data(HouseVotes84, package="mlbench")

set.seed(12)
rhv <- runif(nrow(HouseVotes84))
hv.train <- HouseVotes84[rhv>=0.33,]
hv.test <- HouseVotes84[rhv<0.33,]

  # random naive Bayes for the HouseVotes84 data
hv.rnb <- randnaiveBayes(Class~., hv.train, 50)

hv.pred.rnb <- predict(hv.rnb, hv.test)

  # random naive Bayes test set error for the HouseVotes84 data
hv.err.rnb <- list(nb = err(hv.pred.rnb, hv.test$Class))
}

\keyword{models}
