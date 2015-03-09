\name{bs.wilcox.test}
\alias{bs.wilcox.test}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.5, Example 2.5.10.}

\arguments{See Section 2.5, Example 2.5.10.}

\details{See Section 2.5, Example 2.5.10.}

\value{See Section 2.5, Example 2.5.10.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{bs.kruskal.test}}
\code{\link{wilcox.test}}
}

\examples{
data(weatherc, package="dmr.data")

bs.wilcox.test(weatherc$temperature, weatherc$play)
wilcox.test(temperature~play, weatherc, exact=FALSE, correct=FALSE)
}

\keyword{multivariate}
\keyword{nonparametric}
