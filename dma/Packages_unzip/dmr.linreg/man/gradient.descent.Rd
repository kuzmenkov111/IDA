\name{gradient.descent}
\alias{gradient.descent}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 8 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 8.3, Example 8.3.3.}

\arguments{See Section 8.3, Example 8.3.3.}

\details{See Section 8.3, Example 8.3.3.}

\value{See Section 8.3, Example 8.3.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{predict.par}}
\code{\link{repf.linear}}
\code{\link{grad.linear}}
\code{\link{ols}}
}

\examples{
library(dmr.util)
library(dmr.regeval)

  # perfect representation function for f
repf.perf <- function(av, w)
{
  n <- length(av)
  w[2*n+3]*tanh(sum(w[1:n]*av)+w[n+1]) +
    w[2*n+4]*tanh(sum(w[(n+2):(2*n+1)]*av)+w[2*n+2]) + w[2*n+5]
}

  # gradient of the perfect representation function for f4
grad.perf <- function(data, w)
{
  n <- ncol(data <- as.matrix(data))
  cbind(w[2*n+3]*(1-tanh(rowSums(cmm(data, w[1:n]))+w[n+1])^2)*data,
        w[2*n+3]*(1-tanh(rowSums(cmm(data, w[1:n]))+w[n+1])^2),
        w[2*n+4]*(1-tanh(rowSums(cmm(data, w[(n+2):(2*n+1)]))+w[2*n+2])^2)*data,
        w[2*n+4]*(1-tanh(rowSums(cmm(data, w[(n+2):(2*n+1)]))+w[2*n+2])^2),
        tanh(rowSums(cmm(data, w[1:n]))+w[n+1]),
        tanh(rowSums(cmm(data, w[(n+2):(2*n+1)]))+w[2*n+2]),
        1)
}

  # estimate parameters of the perfect representation function
gd <- gradient.descent(f~a1+a2+a3+a4, prdat.train, w=runif(13, min=-0.1, max=0.1),
                       repf=repf.perf, grad=grad.perf, eps=0.01)
  # test set error
mse(predict(gd$model, prdat.test[,1:4]), prdat.test$f)
}

\keyword{models}
