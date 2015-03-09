\name{predict.dectree.frac}
\alias{predict.dectree.frac}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 3 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 3.7, Example 3.7.2.}

\arguments{See Section 3.7, Example 3.7.2.}

\details{See Section 3.7, Example 3.7.2.}

\value{See Section 3.7, Example 3.7.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{grow.dectree.frac}}
\code{\link{predict.dectree}}
}

\examples{
data(weather, package="dmr.data")
data(weatherc, package="dmr.data")

  # grow a decision tree for the weather data with missing attribute values
weatherm <- weather
weatherm$outlook[1] <- NA
weatherm$humidity[1:2] <- NA
treem <- grow.dectree.frac(play~., weatherm)

  # grow a decision tree for the weatherc data with missing attribute values
weathercm <- weather
weathercm$temperature[1:2] <- NA
weathercm$humidity[1] <- NA
treecm <- grow.dectree.frac(play~., weathercm)

   # decision tree prediction for the weather data with missing attribute values
predict(treem, weatherm)

  # decision tree prediction for the weatherc data with missing attribute values
predict(treecm, weathercm)
}

\keyword{models}
