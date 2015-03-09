\name{pearson.sim}
\alias{pearson.sim}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 11 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 11.4, Example 11.4.2.}

\arguments{See Section 11.4, Example 11.4.2.}

\details{See Section 11.4, Example 11.4.2.}

\value{See Section 11.4, Example 11.4.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{spearman.sim}}
\code{\link{cos.sim}}
\code{\link{dissmat}}
}

\examples{
data(weathercl, package="dmr.data")

  # Pearson similarity matrix for the weathercl data
dissmat(weathercl, pearson.sim )
}

\keyword{arith}
