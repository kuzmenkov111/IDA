\name{enhance.poly}
\alias{enhance.poly}
\alias{repf.poly}
\alias{grad.poly}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 8 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 8.6, Example 8.6.4.}

\arguments{See Section 8.6, Example 8.6.4.}

\details{See Section 8.6, Example 8.6.4.}

\value{See Section 8.6, Example 8.6.4.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{repf.enh}}
\code{\link{grad.enh}}
\code{\link{repf.linear}}
\code{\link{grad.linear}}
\code{\link{predict.par}}
\code{\link{gradient.descent}}
\code{\link{ols.enh}}
}

\examples{
library(dmr.regeval)

  # gradient descent polynomial regression estimation for f3
gd3p <- gradient.descent(f3~a1+a2+a3+a4, lrdat.train, w=rep(0, 9),
                         repf=repf.poly(p=2), grad=grad.poly(p=2),
                         beta=0.001, eps=0.005)
  # test set error
mse(predict(gd3p$model, lrdat.test[,1:4]), lrdat.test$f3)

  # OLS polynomial regression estimation for f3
ols3p <- ols.enh(f3~a1+a2+a3+a4, lrdat.train, enhance.poly)
  # test set error
mse(predict(ols3p, lrdat.test[,1:4]), lrdat.test$f3)

}

\keyword{models}
