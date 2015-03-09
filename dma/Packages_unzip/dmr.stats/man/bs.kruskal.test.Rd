\name{bs.kruskal.test}
\alias{bs.kruskal.test}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.5, Example 2.5.11.}

\arguments{See Section 2.5, Example 2.5.11.}

\details{See Section 2.5, Example 2.5.11.}

\value{See Section 2.5, Example 2.5.11.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{bs.wilcox.test}}
\code{\link{kruskal.test}}
}

\examples{
data(weatherc, package="dmr.data")

bs.kruskal.test(weatherc$temperature, weatherc$play)
kruskal.test(temperature~play, weatherc)
bs.kruskal.test(weatherc$temperature, weatherc$outlook)
kruskal.test(temperature~outlook, weatherc)
}

\keyword{multivariate}
\keyword{nonparametric}
