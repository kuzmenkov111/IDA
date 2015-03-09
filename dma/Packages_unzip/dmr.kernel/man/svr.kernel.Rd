\name{svr.kernel}
\alias{svr.kernel}
\alias{predict.svr.kernel}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 16 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 16.7, Example 16.7.2.}

\arguments{See Section 16.7, Example 16.7.2.}

\details{See Section 16.7, Example 16.7.2.}

\value{See Section 16.7, Example 16.7.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{svr.linear}}
\code{\link{svm.kernel}}
\code{\link{predict.kernel}}
\code{\link{kernel.linear}}
\code{\link{kernel.polynomial}}
\code{\link{kernel.radial}}
\code{\link{kernel.sigmoid}}
\code{\link{svr.linear}}
}

\examples{
library(dmr.regeval)

  # kernel-based SVR for f
svrf.kl <- svr.kernel(f~a1+a2+a3+a4, kmdat.train)
svrf.kp <- svr.kernel(f~a1+a2+a3+a4, kmdat.train,
                      kernel=kernel.polynomial, kernel.args=list(p=2, b=1))
svrf.kr <- svr.kernel(f~a1+a2+a3+a4, kmdat.train,
                      kernel=kernel.radial, kernel.args=list(gamma=0.02))
svrf.ks <- svr.kernel(f~a1+a2+a3+a4, kmdat.train,
                      kernel=kernel.sigmoid, kernel.args=list(gamma=0.2, b=0))

  # kernel-based SVR for g
svrg.kl <- svr.kernel(g~a1+a2+a3+a4, kmdat.train)
svrg.kp <- svr.kernel(g~a1+a2+a3+a4, kmdat.train,
                      kernel=kernel.polynomial, kernel.args=list(p=2, b=1))
svrg.kr <- svr.kernel(g~a1+a2+a3+a4, kmdat.train,
                      kernel=kernel.radial, kernel.args=list(gamma=0.1))
svrg.ks <- svr.kernel(g~a1+a2+a3+a4, kmdat.train,
                      kernel=kernel.sigmoid, kernel.args=list(gamma=0.02, b=-1))

  # training set MSE
mse(predict(svrf.kl, kmdat.train), kmdat.train$f)
mse(predict(svrf.kp, kmdat.train), kmdat.train$f)
mse(predict(svrf.kr, kmdat.train), kmdat.train$f)
mse(predict(svrf.ks, kmdat.train), kmdat.train$f)

mse(predict(svrg.kl, kmdat.train), kmdat.train$g)
mse(predict(svrg.kp, kmdat.train), kmdat.train$g)
mse(predict(svrg.kr, kmdat.train), kmdat.train$g)
mse(predict(svrg.ks, kmdat.train), kmdat.train$g)

  # test set MSE
mse(predict(svrf.kl, kmdat.test), kmdat.test$f)
mse(predict(svrf.kp, kmdat.test), kmdat.test$f)
mse(predict(svrf.kr, kmdat.test), kmdat.test$f)
mse(predict(svrf.ks, kmdat.test), kmdat.test$f)

mse(predict(svrg.kl, kmdat.test), kmdat.test$g)
mse(predict(svrg.kp, kmdat.test), kmdat.test$g)
mse(predict(svrg.kr, kmdat.test), kmdat.test$g)
mse(predict(svrg.ks, kmdat.test), kmdat.test$g)
}

\keyword{models}
