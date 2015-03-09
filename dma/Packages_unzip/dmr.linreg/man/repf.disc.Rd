\name{repf.disc}
\alias{repf.disc}
\alias{grad.disc}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 8 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 8.4, Example 8.4.1.}

\arguments{See Section 8.4, Example 8.4.1.}

\details{See Section 8.4, Example 8.4.1.}

\value{See Section 8.4, Example 8.4.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{repf.linear}}
\code{\link{grad.linear}}
\code{\link{predict.par}}
\code{\link{gradient.descent}}
\code{\link{ols.disc}}
}

\examples{
library(dmr.util)
library(dmr.regeval)
data(weatherr, package="dmr.data")

  # simple representation function for the weatherr data
repf.sigmoid <- function(data, w)
{ 0.5*(tanh(rowSums(cmm(data, w[1:(n <- ncol(data))])) + w[n+1])+1) }

  # and its gradient
grad.sigmoid <- function(data, w)
{ 0.5*(1-tanh(rowSums(cmm(data, w[1:(n <- ncol(data))])) + w[n+1])^2)*cbind(data, 1) }

  # gradient descent for the weatherr data
w.gd <- gradient.descent(playability~., weatherr, w=runif(6, min=-0.01, max=0.01),
                         repf=repf.disc(repf.sigmoid), grad=grad.disc(grad.sigmoid),
                         beta=0.0001, eps=0.005, niter=10000)
mse(predict(w.gd$model, weatherr[,1:4]), weatherr$playability)
}

\keyword{models}
