\name{predict.imp}
\alias{predict.imp}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 17 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 17.3, Example 17.3.4.}

\arguments{See Section 17.3, Example 17.3.4.}

\details{See Section 17.3, Example 17.3.4.}

\value{See Section 17.3, Example 17.3.4.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{imp.all}}
}

\examples{
data(weatherc, package="dmr.data")

data(weatherc, package="dmr.data")
data(Glass, package="mlbench")

set.seed(12)
rg <- runif(nrow(Glass))
g.train <- Glass[rg>=0.33,]
g.test <- Glass[rg<0.33,]

weathercm <- weatherc
weathercm$outlook[c(1, 3)] <- NA
weathercm$temperature[c(2, 4)] <- NA
weathercm$humidity[c(3, 5)] <- NA
weathercm$wind[c(4, 6)] <- NA

gm.train <- g.train
gm.train[sample.int(nrow(gm.train), 0.1*nrow(gm.train)),
         sample.int(ncol(gm.train)-1, 3)] <- NA
gm.test <- g.test
gm.test[sample.int(nrow(gm.test), 0.1*nrow(gm.test)),
        sample.int(ncol(gm.test)-1, 3)] <- NA

  # imputation model for the weatherc data
wm.impm <- imp.all(play~., weathercm, med=TRUE)
  # applied to the weatherc data
wm.imp <- predict.imp(wm.impm, weathercm)

  # imputation model for the Glass data
gm.impm <- imp.all(Type~., gm.train)
  # applied to the training and test sets
gm.train.imp <- predict.imp(gm.impm, gm.train)
gm.test.imp <- predict.imp(gm.impm, gm.test)
}

\keyword{arith}
