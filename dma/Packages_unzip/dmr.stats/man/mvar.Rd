\name{mvar}
\alias{mvar}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.34.}

\arguments{See Section 2.4, Example 2.4.34.}

\details{See Section 2.4, Example 2.4.34.}

\value{See Section 2.4, Example 2.4.34.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{mmean}}
\code{\link{mest}}
\code{\link{mprob}}
}

\examples{
data(weatherr, package="dmr.data")

mvar(weatherr$playability)
mvar(weatherr$playability, m=0)
mvar(weatherr$playability, s02=0.05)
mvar(weatherr$playability, m=5, s02=0.05)
mvar(weatherr$playability[weatherr$temperature<25], m=0)
mvar(weatherr$playability[weatherr$temperature<25],
     m0=mean(weatherr$playability), s02=var(weatherr$playability))
}

\keyword{univar}
