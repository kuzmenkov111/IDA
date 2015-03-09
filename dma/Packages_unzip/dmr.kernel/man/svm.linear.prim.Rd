\name{svm.linear.prim}
\alias{svm.linear.prim}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 16 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 16.2, Example 16.2.2.}

\arguments{See Section 16.2, Example 16.2.2.}

\details{See Section 16.2, Example 16.2.2.}

\value{See Section 16.2, Example 16.2.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{svm.linear.dual}}
\code{\link{svm.linear}}
\code{\link{svm.kernel}}
}

\examples{
library(dmr.linclas)
library(dmr.claseval)

  # estimate linear SVM model parameters
svm.p.ls <- svm.linear.prim(c~a1+a2+a3+a4, kmdat.train.ls)

  # misclassification error
err(predict(svm.p.ls$model, kmdat.train.ls[,1:4]), kmdat.train.ls$c)
err(predict(svm.p.ls$model, kmdat.test.ls[,1:4]), kmdat.test.ls$c)
}

\keyword{models}
