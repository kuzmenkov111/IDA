\name{spearman.sim}
\alias{spearman.sim}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 11 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 11.4, Example 11.4.3.}

\arguments{See Section 11.4, Example 11.4.3.}

\details{See Section 11.4, Example 11.4.3.}

\value{See Section 11.4, Example 11.4.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{pearson.sim}}
\code{\link{cos.sim}}
\code{\link{dissmat}}
}

\examples{
data(weathercl, package="dmr.data")

  # Spearman similarity matrix for the weathercl data
dissmat(weathercl, spearman.sim )
}

\keyword{arith}
