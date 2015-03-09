\name{ord}
\alias{ord}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.6.}

\arguments{See Section 2.4, Example 2.4.6.}

\details{See Section 2.4, Example 2.4.6.}

\value{See Section 2.4, Example 2.4.6.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{bs.rank}}
\code{\link{rank}}
}

\examples{
data(weatherr, package="dmr.data")

ord(weatherr$playability, 11)
weatherr$playability[rank(weatherr$playability, ties.method="first")==11]
ord(weatherr$playability, 10:13)
weatherr$playability[rank(weatherr$playability, ties.method="first") \%in\% 10:13]
}

\keyword{univar}
\keyword{nonparametric}
