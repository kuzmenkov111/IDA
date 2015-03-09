\name{repf.gen}
\alias{repf.gen}
\alias{grad.gen}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 8 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 8.6, Example 8.6.1.}

\arguments{See Section 8.6, Example 8.6.1.}

\details{See Section 8.6, Example 8.6.1.}

\value{See Section 8.6, Example 8.6.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{ols.gen}}
\code{\link{repf.linear}}
\code{\link{grad.linear}}
\code{\link{predict.par}}
\code{\link{gradient.descent}}
}

\examples{
library(dmr.regeval)

  # perfect inverse link function for f2
link2.inv <- function(v) { tanh(v/10) }
  # and its derivative
link2.inv.deriv <- function(v) { (1-tanh(v/10)^2)/10 }

  # perfect generalized linear representation function for f2
repf.gen2 <- repf.gen(link2.inv)
  # and its gradient
grad.gen2 <- grad.gen(link2.inv.deriv)

  # gradient descent estimation of generalized linear model parameters for f2
gd2g <- gradient.descent(f2~a1+a2+a3+a4, lrdat.train, w=rep(0, 5),
                         repf=repf.gen2, grad=grad.gen2,
                         beta=0.5, eps=0.0001)
  # test set error
mse(predict(gd2g$model, lrdat.test[,1:4]), lrdat.test$f2)
}

\keyword{models}
