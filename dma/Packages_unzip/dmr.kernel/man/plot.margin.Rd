\name{plot.margin}
\alias{plot.margin}
\alias{fmarg}
\alias{gmarg}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 16 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 16.2, Example 16.2.2.}

\arguments{See Section 16.2, Example 16.2.1.}

\details{See Section 16.2, Example 16.2.2.}

\value{See Section 16.2, Example 16.2.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{svm.linear}}
\code{\link{svm.linear.prim}}
\code{\link{svm.linear.dual}}
}

\examples{
library(dmr.linclas)
library(dmr.util)

  # dataset for margin illustration (skip near-boundary instances from kmdat.plot)
kmdat.m <- kmdat.plot[abs(kmdat.plot$f)>2,c("a1", "a2", "c")]
kmdat.m <- kmdat.m[sample(nrow(kmdat.m), 100),]

  # parameter vector for margin demonstration
w.m <- c(1, -2)
  # predictions with intercept 0
p0.m <- predict.par(list(repf=repf.linear, w=c(w.m, 0)), kmdat.m[,1:2])
 # symmetric-margin intercept
w.m <- c(w.m, -(max(p0.m[kmdat.m$c==0])+min(p0.m[kmdat.m$c==1]))/2)

  # minimum functional margin
min(fmarg(w.m, kmdat.m[,1:2], 2*as.num0(kmdat.m$c)-1))
  # minimum geometric
min(gmarg(w.m, kmdat.m[,1:2], 2*as.num0(kmdat.m$c)-1))

  # scale parameters to get minimum functional margin of 1
w.m <- w.m/min(fmarg(w.m, kmdat.m[,1:2], 2*as.num0(kmdat.m$c)-1))
  # minimum functional margin after parameter scaling (1)
min(fmarg(w.m, kmdat.m[,1:2], 2*as.num0(kmdat.m$c)-1))
  # minimum geometric margin after parameter scaling (unchanged)
min(gmarg(w.m, kmdat.m[,1:2], 2*as.num0(kmdat.m$c)-1))

plot.margin(w.m, kmdat.m[,1:2], 2*as.num0(kmdat.m$c)-1)
}

\keyword{models}
