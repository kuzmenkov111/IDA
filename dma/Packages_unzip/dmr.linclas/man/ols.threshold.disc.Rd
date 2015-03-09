\name{ols.threshold.disc}
\alias{ols.threshold.disc}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 5 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 5.4, Example 5.4.1.}

\arguments{See Section 5.4, Example 5.4.1.}

\details{See Section 5.4, Example 5.4.1.}

\value{See Section 5.4, Example 5.4.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{ols.threshold}}
\code{\link{repf.disc}}
}

\examples{
library(dmr.claseval)
data(weatherc, package="dmr.data")

  # gradient descent for the weatherc data
w.gdl <-  gradient.descent(play~., weatherc, w=rep(0, 6),
                           repf=repf.disc(repf.threshold(repf.linear)),
                           grad=grad.disc(grad.threshold(grad.linear)),
                           delta=delta.mse, perf=err,
                           beta=1, batch=TRUE, eps=0.2)
err(factor(ustep(predict(w.gdl$model, weatherc[,1:4]), 0.5),
           levels=0:1, labels=c("no", "yes")),
    weatherc$play)

  # OLS for the weatherc data
w.ols <- ols.threshold.disc(play~., weatherc)
err(factor(ustep(predict(w.ols, weatherc[,1:4]), 0.5),
           levels=0:1, labels=c("no", "yes")),
    weatherc$play)
}

\keyword{models}
