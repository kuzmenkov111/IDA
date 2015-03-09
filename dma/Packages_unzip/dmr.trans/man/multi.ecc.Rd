\name{multi.ecc}
\alias{multi.ecc}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 17 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 17.4, Example 17.4.4.}

\arguments{See Section 17.4, Example 17.4.4.}

\details{See Section 17.4, Example 17.4.4.}

\value{See Section 17.4, Example 17.4.4.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{multi.class}}
\code{\link{ecc}}
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

  # error correcting encoding/decoding for 4 and 6 classes
multi.ecc4 <- multi.ecc(4)
multi.ecc6 <- multi.ecc(6)

  # encoding class labels and decoding verification
v.ecc4 <- multi.ecc4$enc(Vehicle$Class, "Class")
err(multi.ecc4$dec(v.ecc4, levels(Vehicle$Class)), Vehicle$Class)

  # error-correcting encoding applied to rpart
rp.e4 <- multi.class(rpart, predf=function(...) predict(..., type="c"),
                     encode=multi.ecc4$enc, decode=multi.ecc4$dec)
rp.e6 <- multi.class(rpart, predf=function(...) predict(..., type="c"),
                     encode=multi.ecc6$enc, decode=multi.ecc6$dec)
v.tree.e <- rp.e4$alg(Class~., v.train)
v.tree.e.pred <- rp.e4$predict(v.tree.e, v.test)
g.tree.e <- rp.e6$alg(Type~., g.train)
g.tree.e.pred <- rp.e6$predict(g.tree.e, g.test)

  # error-correcting encoding applied to naive Bayes
nb.e4 <- multi.class(naiveBayes, encode=multi.ecc4$enc, decode=multi.ecc4$dec)
nb.e6 <- multi.class(naiveBayes, encode=multi.ecc6$enc, decode=multi.ecc6$dec)

v.nb.e <- nb.e4$alg(Class~., v.train)
v.nb.e.pred <- nb.e4$predict(v.nb.e, v.test)
g.nb.e <- nb.e6$alg(Type~., g.train)
g.nb.e.pred <- nb.e6$predict(g.nb.e, g.test)
}

\keyword{arith}
