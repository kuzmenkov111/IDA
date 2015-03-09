\name{delta.mse}
\alias{delta.mse}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 8 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 8.3, Example 8.3.2.}

\arguments{See Section 8.3, Example 8.3.2.}

\details{See Section 8.3, Example 8.3.2.}

\value{See Section 8.3, Example 8.3.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{gradient.descent}}
\code{\link{repf.linear}}
\code{\link{grad.linear}}
}

\examples{
library(dmr.util)

  # perfect representation function for f
repf.perf <- function(data, w)
{
  n <- ncol(data)
  w[2*n+3]*tanh(rowSums(cmm(data, w[1:n]))+w[n+1]) +
    w[2*n+4]*tanh(rowSums(cmm(data, w[(n+2):(2*n+1)]))+w[2*n+2]) + w[2*n+5]
}

  # gradient of the perfect representation function for f
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

  # perfect parameters for f
w.perf <- c(1, -2, 3, -1, 1, -2, 3, -2, 1, -1, 2, -3, 2)
  # perfect model for f
mod.perf <- `class<-`(list(w=w.perf, repf=repf.perf), "par")

  # delta rule for the perfect model
delta.mse(prdat.train$f, predict(mod.perf, prdat.train[,1:4]),
          grad.perf(prdat.train[,1:4], w.perf), 0.1)
  # delta rule for the perfect model with modified target function values
delta.mse(prdat.train$f+1, predict(mod.perf, prdat.train[,1:4]),
          grad.perf(prdat.train[,1:4], w.perf), 0.1)

}

\keyword{models}
