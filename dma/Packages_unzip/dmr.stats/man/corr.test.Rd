\name{corr.test}
\alias{corr.test}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.5, Example 2.5.2.}

\arguments{See Section 2.5, Example 2.5.2.}

\details{See Section 2.5, Example 2.5.2.}

\value{See Section 2.5, Example 2.5.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{corl.test}}
\code{\link{cor.test}}
}

\examples{
data(weatherr, package="dmr.data")

corr.test(weatherr$temperature, weatherr$playability)
cor.test(weatherr$temperature, weatherr$playability, method="spearman")
corr.test(weatherr$temperature, -weatherr$playability)
cor.test(weatherr$temperature, -weatherr$playability, method="spearman")
}

\keyword{multivariate}
\keyword{nonparametric}
