\name{multi.class}
\alias{multi.class}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 17 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 17.4, Example 17.4.1.}

\arguments{See Section 17.4, Example 17.4.1.}

\details{See Section 17.4, Example 17.4.1.}

\value{See Section 17.4, Example 17.4.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{

\code{\link{multi.enc.nbc}}
\code{\link{multi.enc.nbc}}
\code{\link{multi.enc.1ofk}}
\code{\link{multi.enc.1ofk}}
\code{\link{multi.ecc}}
}

\examples{
library(rpart)
library(e1071)

data(Vehicle, package="mlbench")
data(Glass, package="mlbench")

set.seed(12)

rv <- runif(nrow(Vehicle))
v.train <- Vehicle[rv>=0.33,]
v.test <- Vehicle[rv<0.33,]

rg <- runif(nrow(Glass))
g.train <- Glass[rg>=0.33,]
g.test <- Glass[rg<0.33,]

  # basic encoding applied to rpart
rp.n <- multi.class(rpart, predf=function(...) predict(..., type="c"))
v.tree.n <- rp.n$alg(Class~., v.train)
v.tree.n.pred <- rp.n$predict(v.tree.n, v.test)
g.tree.n <- rp.n$alg(Type~., g.train)
g.tree.n.pred <- rp.n$predict(g.tree.n, g.test)

  # basic encoding applied to naive Bayes
nb.n <- multi.class(naiveBayes)
v.nb.n <- nb.n$alg(Class~., v.train)
v.nb.n.pred <- nb.n$predict(v.nb.n, v.test)
g.nb.n <- nb.n$alg(Type~., g.train)
g.nb.n.pred <- nb.n$predict(g.nb.n, g.test)
}

\keyword{arith}
