\name{bs.t.test}
\alias{bs.t.test}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.5, Example 2.5.8.}

\arguments{See Section 2.5, Example 2.5.8.}

\details{See Section 2.5, Example 2.5.8.}

\value{See Section 2.5, Example 2.5.8.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{f.test}}
\code{\link{t.test}}
}

\examples{
data(weatherc, package="dmr.data")

bs.t.test(weatherc$temperature, weatherc$play)
t.test(temperature~play, weatherc, var.equal=TRUE)
}

\keyword{multivariate}
