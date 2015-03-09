\name{weighted.var1}
\alias{weighted.var1}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.11.}

\arguments{See Section 2.4, Example 2.4.11.}

\details{See Section 2.4, Example 2.4.11.}

\value{See Section 2.4, Example 2.4.11.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{var1}}
\code{\link{weighted.var}}
\code{\link{weighted.median}}
\code{\link{weighted.modal}}
}

\examples{
weighted.var1(1:2, 1:2)
weighted.var1(1, 2)

data(weatherr, package="dmr.data")

weighted.var1(weatherr$temperature[weatherr$playability<0.75],
              weatherr$playability[weatherr$playability<0.75])
weighted.var1(weatherr$temperature[weatherr$playability>=0.75],
              weatherr$playability[weatherr$playability>=0.75])
weighted.var1(weatherr$temperature[weatherr$playability>=0.8],
              weatherr$playability[weatherr$playability>=0.8])
}

\keyword{univar}
