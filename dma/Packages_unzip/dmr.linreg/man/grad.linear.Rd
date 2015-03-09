\name{grad.linear}
\alias{grad.linear}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 8 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 8.3, Example 8.3.1.}

\arguments{See Section 8.3, Example 8.3.1.}

\details{See Section 8.3, Example 8.3.1.}

\value{See Section 8.3, Example 8.3.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{repf.linear}}
\code{\link{predict.par}}
\code{\link{gradient.descent}}
}

\examples{
library(dmr.regeval)

  # gradient for the first 10 instances
grad.linear(lrdat.train[1:10,1:4], rep(0, 5))

  # estimate linear model parameters for f1
gd1 <- gradient.descent(f1~a1+a2+a3+a4, lrdat.train,
                        w=rep(0, 5),
                        repf=repf.linear, grad=grad.linear, beta=0.01, eps=0.0001)
  # test set error
mse(predict(gd1$model, lrdat.test[,1:4]), lrdat.test$f1)
}

\keyword{models}
