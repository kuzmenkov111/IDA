\name{predict.par}
\alias{predict.par}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 8 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 8.2, Example 8.2.1.}

\arguments{See Section 8.2, Example 8.2.1.}

\details{See Section 8.2, Example 8.2.1.}

\value{See Section 8.2, Example 8.2.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{repf.linear}}
\code{\link{grad.linear}}
}

\examples{
library(dmr.util)
library(dmr.regeval)

  # perfect representation function for f
repf.perf <- function(data, w)
{
  w[2*(n <- ncol(data))+3]*tanh(rowSums(cmm(data, w[1:n]))+w[n+1]) +
    w[2*n+4]*tanh(rowSums(cmm(data, w[(n+2):(2*n+1)]))+w[2*n+2]) + w[2*n+5]
}

  # perfect parameters for f
w.perf <- c(1, -2, 3, -1, 1, -2, 3, -2, 1, -1, 2, -3, 2)
  # perfect model for f
mod.perf <- `class<-`(list(w=w.perf, repf=repf.perf), "par")
  # test set error
mse(predict(mod.perf, prdat.test[,1:4]), prdat.test$f)

}

\keyword{models}
