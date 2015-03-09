\name{nbc}
\alias{nbc}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 4 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 4.3, Example 4.3.3.}

\arguments{See Section 4.3, Example 4.3.3.}

\details{See Section 4.3, Example 4.3.3.}

\value{See Section 4.3, Example 4.3.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{predict.nbc}}
}

\examples{
data(weather, package="dmr.data")

  # naive Bayes classifier for the weather data
nbw <- nbc(play~., weather)
}

\keyword{models}
