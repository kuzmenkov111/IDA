\name{repf.enh}
\alias{repf.enh}
\alias{grad.enh}
\alias{ols.enh}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 8 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 8.6, Example 8.6.3.}

\arguments{See Section 8.6, Example 8.6.3.}

\details{See Section 8.6, Example 8.6.3.}

\value{See Section 8.6, Example 8.6.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{enhance.poly}}
\code{\link{repf.linear}}
\code{\link{grad.linear}}
\code{\link{predict.par}}
\code{\link{gradient.descent}}
\code{\link{ols}}
}

\examples{
library(dmr.regeval)

  # perfect representation enhancement for f3
enhance3 <- function(data)
{
  cbind(data, sq=data^2)
}

  # gradient descent estimation for f3
gd3e <- gradient.descent(f3~a1+a2+a3+a4, lrdat.train, w=rep(0, 9),
                         repf=repf.enh(enhance3), grad=grad.enh(enhance3),
                         beta=0.001, eps=0.005)
  # test set error
mse(predict(gd3e$model, lrdat.test[,1:4]), lrdat.test$f3)

  # OLS estimation for f3
ols3e <- ols.enh(f3~a1+a2+a3+a4, lrdat.train, enhance3)
  # test set error
mse(predict(ols3e, lrdat.test[,1:4]), lrdat.test$f3)
}

\keyword{models}
