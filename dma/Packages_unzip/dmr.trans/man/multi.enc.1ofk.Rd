\name{multi.enc.1ofk}
\alias{multi.enc.1ofk}
\alias{multi.dec.1ofk}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 17 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 17.4, Example 17.4.2.}

\arguments{See Section 17.4, Example 17.4.2.}

\details{See Section 17.4, Example 17.4.2.}

\value{See Section 17.4, Example 17.4.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{multi.class}}
}

\examples{
library(rpart)
library(e1071)
library(dmr.claseval)

data(Vehicle, package="mlbench")
data(Glass, package="mlbench")

set.seed(12)

rv <- runif(nrow(Vehicle))
v.train <- Vehicle[rv>=0.33,]
v.test <- Vehicle[rv<0.33,]

rg <- runif(nrow(Glass))
g.train <- Glass[rg>=0.33,]
g.test <- Glass[rg<0.33,]

  # encoding class labels and decoding verification
v.1ofk <- multi.enc.1ofk(Vehicle$Class, "Class")
err(multi.dec.1ofk(v.1ofk, levels(Vehicle$Class)), Vehicle$Class)

  # 1-of-k encoding applied to rpart
rp.1 <- multi.class(rpart, predf=function(...) predict(...)[,2],
                    encode=multi.enc.1ofk, decode=multi.dec.1ofk)
v.tree.1 <- rp.1$alg(Class~., v.train)
v.tree.1.pred <- rp.1$predict(v.tree.1, v.test)
g.tree.1 <- rp.1$alg(Type~., g.train)
g.tree.1.pred <- rp.1$predict(g.tree.1, g.test)

  # 1-of-k encoding applied to naive Bayes
nb.1 <- multi.class(naiveBayes, predf=function(...) predict(..., type="r")[,2],
                    encode=multi.enc.1ofk, decode=multi.dec.1ofk)
v.nb.1 <- nb.1$alg(Class~., v.train)
v.nb.1.pred <- nb.1$predict(v.nb.1, v.test)
g.nb.1 <- nb.1$alg(Type~., g.train)
g.nb.1.pred <- nb.1$predict(g.nb.1, g.test)
}

\keyword{arith}
