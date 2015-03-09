\name{cos.sim}
\alias{cos.sim}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 11 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 11.4, Example 11.4.4.}

\arguments{See Section 11.4, Example 11.4.4.}

\details{See Section 11.4, Example 11.4.4.}

\value{See Section 11.4, Example 11.4.4.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{pearson.sim}}
\code{\link{spearman.sim}}
\code{\link{dissmat}}
}

\examples{
data(weathercl, package="dmr.data")

  # cosine similarity matrix for the weathercl data
dissmat(weathercl, cos.sim)
}

\keyword{arith}
