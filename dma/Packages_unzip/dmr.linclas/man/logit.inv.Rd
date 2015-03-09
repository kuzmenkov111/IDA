\name{logit.inv}
\alias{logit.inv}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 5 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 5.2, Example 5.2.6.}

\arguments{See Section 5.2, Example 5.2.6.}

\details{See Section 5.2, Example 5.2.6.}

\value{See Section 5.2, Example 5.2.6.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{repf.logit}}
}

\examples{
library(lattice)

curve(logit.inv(x), from=-5, to=5, xlab="q", ylab="logit.inv(q)", col="blue")

p1.lt <- function(a1, a2) { logit.inv(pcg.plot(a1, a2)) }
h.lt <- function(a1, a2) { ustep(p1.lt(a1, a2), 0.5) }

pcdat.plot$p1.lt <- p1.lt(pcdat.plot$a1, pcdat.plot$a2)
pcdat.plot$h.lt <- h.lt(pcdat.plot$a1, pcdat.plot$a2)

wf.g <- wireframe(g~a1+a2, pcdat.plot, col="green", zoom=0.8)
wf.p1.lt <- wireframe(p1.lt~a1+a2, pcdat.plot, drape=TRUE, at=c(0, 0.5, 1),
                      col="transparent", col.regions=c("blue", "red"),
                      colorkey=FALSE, zoom=0.8)
wf.h.lt <- wireframe(h.lt~a1+a2, pcdat.plot, col="orange", zoom=0.8)
l.h.lt <- levelplot(h.lt~a1+a2, pcdat.plot, at=c(-100, 0, 100),
                    col.regions=c("blue", "red"), colorkey=FALSE)

print(wf.g, split=c(1, 1, 2, 2), more=TRUE)
print(wf.p1.lt, split=c(2, 1, 2, 2), more=TRUE)
print(wf.h.lt, split=c(1, 2, 2, 2), more=TRUE)
print(l.h.lt, split=c(2, 2, 2, 2))
}

\keyword{arith}
