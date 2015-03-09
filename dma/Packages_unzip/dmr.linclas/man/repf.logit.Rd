\name{repf.logit}
\alias{repf.logit}
\alias{grad.logit}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 5 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 5.2, Example 5.2.7 and Section 5.3, Example 5.3.2.}

\arguments{See Section 5.2, Example 5.2.7 and Section 5.3, Example 5.3.2.}

\details{See Section 5.2, Example 5.2.7 and Section 5.3, Example 5.3.2.}

\value{See Section 5.2, Example 5.2.7 and Section 5.3, Example 5.3.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{logit.inv}}
\code{\link{repf.threshold}}
\code{\link{predict.par}}
\code{\link{gradient.descent}}
}

\examples{
library(dmr.claseval)

  # perfect inner representation function
repf.perf <- repf.poly(2)
  # perfect parameter vector
w.perf <- c(2, -3, 2, -3, 1, 2, -1, -2, 1)

  # perfect logit model
perf.logit <- `class<-`(list(repf=repf.logit(repf.perf), w=w.perf), "par")
  # test set error
err(ustep(predict(perf.logit, pcdat.test[,1:4]), 0.5), pcdat.test$c)
  # test set loglikelihood
loglik01(predict(perf.logit, pcdat.test[,1:4]), pcdat.test$c)
}

\keyword{models}
