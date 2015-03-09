\name{var1}
\alias{var1}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.9.}

\arguments{See Section 2.4, Example 2.4.9.}

\details{See Section 2.4, Example 2.4.9.}

\value{See Section 2.4, Example 2.4.9.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
\code{\link{bs.var}}
\code{\link{var}}
}

\examples{
var1(1:2)
var1(1)

data(weatherr, package="dmr.data")

var1(weatherr$temperature[weatherr$playability<0.75])
var1(weatherr$temperature[weatherr$playability>=0.75])
var1(weatherr$temperature[weatherr$playability>=0.8])
}

\keyword{univar}
