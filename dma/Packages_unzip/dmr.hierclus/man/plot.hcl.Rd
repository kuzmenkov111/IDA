\name{plot.hcl}
\alias{plot.hcl}
\alias{as.dendrogram.hcl}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 13 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 13.5, Example 13.5.1.}

\arguments{See Section 13.5, Example 13.5.1.}

\details{See Section 13.5, Example 13.5.1.}

\value{See Section 13.5, Example 13.5.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{ahc}}
\code{\link{dhc}}
\code{\link{cut.hcl}}
\code{\link{dendrogram}}
}

\examples{
library(dmr.trans)

data(weathercl, package="dmr.data")
wcl.std <- predict.std(std.all(.~., weathercl), weathercl)

wcl.ahc.sl <- ahc(wcl.std, linkf=ahc.single)
wcl.ahc.cl <- ahc(wcl.std, linkf=ahc.complete)
wcl.ahc.al <- ahc(wcl.std, linkf=ahc.average)
wcl.ahc.ml <- ahc(wcl.std, linkf=ahc.center)
wcl.ahc.wl <- ahc(wcl.std, linkf=ahc.ward)
wcl.dhc <- dhc(wcl.std)

as.dendrogram(wcl.ahc.sl)

  # dendrogram plots for the weathercl data
par(mfrow=c(3, 2))
plot(wcl.ahc.sl, main="Single linkage")
plot(wcl.ahc.cl, main="Complete linkage")
plot(wcl.ahc.al, main="Average linkage")
plot(wcl.ahc.ml, main="Center linkage")
plot(wcl.ahc.wl, main="Ward linkage")
plot(wcl.dhc, main="Divisive clustering")

}

\keyword{models}
