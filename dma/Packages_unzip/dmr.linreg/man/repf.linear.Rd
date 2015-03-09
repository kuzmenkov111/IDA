\name{repf.linear}
\alias{repf.linear}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 8 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 8.2, Example 8.2.2.}

\arguments{See Section 8.2, Example 8.2.2.}

\details{See Section 8.2, Example 8.2.2.}

\value{See Section 8.2, Example 8.2.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{predict.par}}
\code{\link{grad.linear}}
}

\examples{
library(dmr.regeval)

  # perfect parameter vector for f1
w.perf1 <- c(3, 4, -2, 2, -3)
  # perfect model for f1
mod.perf1 <- `class<-`(list(w=w.perf1, repf=repf.linear), "par")
  # test set error
mse(predict(mod.perf1, lrdat.test[,1:4]), lrdat.test$f1)
}

\keyword{models}
