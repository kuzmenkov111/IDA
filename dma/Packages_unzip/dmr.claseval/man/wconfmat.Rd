\name{wconfmat}
\alias{wconfmat}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 7 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 7.2, Example 7.2.9.}

\arguments{See Section 7.2, Example 7.2.9.}

\details{See Section 7.2, Example 7.2.9.}

\value{See Section 7.2, Example 7.2.9.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{confmat}}
\code{\link{tpr}}
\code{\link{fpr}}
\code{\link{precision}}
\code{\link{recall}}
\code{\link{sensitivity}}
\code{\link{specificity}}
\code{\link{f.measure}}
\code{\link{wroc}}
}

\examples{
library(rpart)
library(dmr.util)
data(Soybean, package="mlbench")

s01.labels <- c("other", "brown-spot")
Soybean01 <- Soybean
Soybean01$Class <- factor(ifelse(Soybean$Class=="brown-spot", "brown-spot", "other"),
                          levels=s01.labels)
set.seed(12)
rs <- runif(nrow(Soybean))
s01.train <- Soybean01[rs>=0.33,]
s01.test <- Soybean01[rs<0.33,]
s01.tree <- rpart(Class~., s01.train)

  # double weight for the brown-spot class
s01.w1test <- ifelse(s01.test$Class=="brown-spot", 2, 1)
s01.w1cm <- wconfmat(predict(s01.tree, s01.test, type="c"),
                     s01.test$Class, s01.w1test)
tpr(s01.w1cm)
fpr(s01.w1cm)

  # 10 times less weight for instances with plant.stand=1
s01.w2test <- ifelse(!is.na(s01.test$plant.stand) & s01.test$plant.stand=="1",
                     0.1, 1)
s01.w2cm <- wconfmat(predict(s01.tree, s01.test, type="c"),
                     s01.test$Class, s01.w2test)
tpr(s01.w2cm)
fpr(s01.w2cm)
}

\keyword{arith}
\keyword{models}
