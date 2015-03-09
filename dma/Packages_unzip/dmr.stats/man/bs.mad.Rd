\name{bs.mad}
\alias{bs.mad}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.15.}

\arguments{See Section 2.4, Example 2.4.15.}

\details{See Section 2.4, Example 2.4.15.}

\value{See Section 2.4, Example 2.4.15.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{bs.sd}}
\code{\link{qd}}
\code{\link{mad}}
}

\examples{
data(weatherr, package="dmr.data")

bs.mad(weatherr$playability, scale=1)
mad(weatherr$playability, constant=1)
bs.mad(weatherr$playability)
mad(weatherr$playability)
sd(weatherr$playability)
}

\keyword{univar}
