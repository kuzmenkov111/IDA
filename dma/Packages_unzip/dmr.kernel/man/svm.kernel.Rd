\name{svm.kernel}
\alias{svm.kernel}
\alias{predict.svm.kernel}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 16 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 16.7, Example 16.7.1.}

\arguments{See Section 16.7, Example 16.7.1.}

\details{See Section 16.7, Example 16.7.1.}

\value{See Section 16.7, Example 16.7.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{svm.linear}}
\code{\link{svr.kernel}}
\code{\link{predict.kernel}}
\code{\link{kernel.linear}}
\code{\link{kernel.polynomial}}
\code{\link{kernel.radial}}
\code{\link{kernel.sigmoid}}
\code{\link{kernel.linear}}
}

\examples{
library(dmr.claseval)

  # kernel-based SVM
svm.kl <- svm.kernel(c~a1+a2+a3+a4, kmdat.train)
svm.kp <- svm.kernel(c~a1+a2+a3+a4, kmdat.train,
                     kernel=kernel.polynomial, kernel.args=list(p=2, b=1))
svm.kr <- svm.kernel(c~a1+a2+a3+a4, kmdat.train,
                     kernel=kernel.radial, kernel.args=list(gamma=0.5))
svm.ks <- svm.kernel(c~a1+a2+a3+a4, kmdat.train,
                     kernel=kernel.sigmoid, kernel.args=list(gamma=0.04,b=-0.8))

  # training set misclassification error
err(predict(svm.kl, kmdat.train), kmdat.train$c)
err(predict(svm.kp, kmdat.train), kmdat.train$c)
err(predict(svm.kr, kmdat.train), kmdat.train$c)
err(predict(svm.ks, kmdat.train), kmdat.train$c)

  # test set misclassification error
err(predict(svm.kl, kmdat.test), kmdat.test$c)
err(predict(svm.kp, kmdat.test), kmdat.test$c)
err(predict(svm.kr, kmdat.test), kmdat.test$c)
err(predict(svm.ks, kmdat.test), kmdat.test$c)

}

\keyword{models}
