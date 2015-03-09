\name{ols.threshold}
\alias{ols.threshold}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 5 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 5.3, Example 5.3.4.}

\arguments{See Section 5.3, Example 5.3.4.}

\details{See Section 5.3, Example 5.3.4.}

\value{See Section 5.3, Example 5.3.4.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{ols}}
\code{\link{repf.threshold}}
\code{\link{predict.par}}
}

\examples{
library(dmr.claseval)

  # least squares linear threshold for the artificial data
ols.th <- ols.threshold(c~., lcdat.train)
ols.th.ls <- ols.threshold(c~., lcdat.train.ls)

  # training set error
err(predict(ols.th, lcdat.train[,1:4]), lcdat.train$c)
err(predict(ols.th.ls, lcdat.train.ls[,1:4]), lcdat.train.ls$c)
  # test set error
err(predict(ols.th, lcdat.test[,1:4]), lcdat.test$c)
err(predict(ols.th.ls, lcdat.test.ls[,1:4]), lcdat.test.ls$c)
}

\keyword{models}
