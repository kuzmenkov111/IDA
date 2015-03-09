\name{weighted.median}
\alias{weighted.median}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.4.}

\arguments{See Section 2.4, Example 2.4.4.}

\details{See Section 2.4, Example 2.4.4.}

\value{See Section 2.4, Example 2.4.4.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{bs.median}}
\code{\link{bs.weighted.mean}}
\code{\link{weighted.modal}}
\code{\link{weighted.var}}
}

\examples{
data(weatherc, package="dmr.data")

weighted.median(weatherc$temperature, ifelse(weatherc$play=="yes", 5, 1))
median(c(weatherc$temperature[weatherc$play=="no"],
         rep(weatherc$temperature[weatherc$play=="yes"], 5)))
weighted.median(weatherc$temperature, ifelse(weatherc$play=="yes", 0.2, 1))
median(c(weatherc$temperature[weatherc$play=="yes"],
         rep(weatherc$temperature[weatherc$play=="no"], 5)))
}

\keyword{univar}
