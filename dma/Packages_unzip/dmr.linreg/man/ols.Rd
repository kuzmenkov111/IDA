\name{ols}
\alias{ols}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 8 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 8.3, Example 8.3.4.}

\arguments{See Section 8.3, Example 8.3.4.}

\details{See Section 8.3, Example 8.3.4.}

\value{See Section 8.3, Example 8.3.4.}

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

  # linear model for f1
ols1 <- ols(f1~a1+a2+a3+a4, lrdat.train)
  # test set error
mse(predict(ols1, lrdat.test[,1:4]), lrdat.test$f1)
}

\keyword{models}
