\name{ols.gen}
\alias{ols.gen}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 8 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 8.6, Example 8.6.2.}

\arguments{See Section 8.6, Example 8.6.2.}

\details{See Section 8.6, Example 8.6.2.}

\value{See Section 8.6, Example 8.6.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{repf.gen}}
\code{\link{ols}}
\code{\link{predict.par}}
}

\examples{
library(dmr.regeval)

  # perfect link function for f2
link2 <- function(v) { 10*atanh(v) }
  # perfect inverse link function for f2
link2.inv <- function(v) { tanh(v/10) }

  # estimate of generalized linear model parameters for f2
ols2g <- ols.gen(f2~a1+a2+a3+a4, lrdat.train, link=link2, link.inv=link2.inv)
  # test set error
mse(predict(ols2g, lrdat.test[,1:4]), lrdat.test$f2)
}

\keyword{models}
