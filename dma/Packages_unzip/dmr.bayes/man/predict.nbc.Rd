\name{predict.nbc}
\alias{predict.nbc}
\alias{predict1.nbc}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 4 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 4.3, Example 4.3.4.}

\arguments{See Section 4.3, Example 4.3.4.}

\details{See Section 4.3, Example 4.3.4.}

\value{See Section 4.3, Example 4.3.4.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{nbc}}
}

\examples{
data(weather, package="dmr.data")

  # naive Bayes classifier for the weather data
nbw <- nbc(play~., weather)

  # naive Bayes predictions for the first instance of the weather data
predict1.nbc(nbw, weather[1,])
  # naive Bayes predictions for the weather data
predict(nbw, weather)
}

\keyword{models}
