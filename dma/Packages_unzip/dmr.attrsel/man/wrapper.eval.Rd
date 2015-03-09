\name{wrapper.eval}
\alias{wrapper.eval}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 19 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 19.5, Example 19.5.1.}

\arguments{See Section 19.5, Example 19.5.1.}

\details{See Section 19.5, Example 19.5.1.}

\value{See Section 19.5, Example 19.5.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{wrapper.select}}
\code{\link{wrapper.filter.select}}
\code{\link{asel.search.greedy}}
}

\examples{
library(rpart)
data(weather, package="dmr.data")
data(weatherc, package="dmr.data")
data(weatherr, package="dmr.data")

  # wrapper evaluation for the weather data
wrapper.eval(c("outlook", "temperature"), "play", weather,
             rpart, args=list(minsplit=2),
             predf=function(...) predict(..., type="c"))
wrapper.eval(c("outlook", "temperature", "humidity"), "play", weather,
             rpart, args=list(minsplit=2),
             predf=function(...) predict(..., type="c"))
wrapper.eval(names(weather)[-5], "play", weather,
             rpart, args=list(minsplit=2),
             predf=function(...) predict(..., type="c"))

  # wrapper evaluation for the weatherc data
wrapper.eval(c("outlook", "temperature"), "play", weatherc,
             rpart, args=list(minsplit=2),
             predf=function(...) predict(..., type="c"))
wrapper.eval(c("outlook", "temperature", "humidity"), "play", weatherc,
             rpart, args=list(minsplit=2),
             predf=function(...) predict(..., type="c"))
wrapper.eval(names(weatherc)[-5], "play", weatherc,
             rpart, args=list(minsplit=2),
             predf=function(...) predict(..., type="c"))

  # wrapper evaluation for the weatherr data
wrapper.eval(c("outlook", "temperature"), "playability", weatherr,
             rpart, args=list(minsplit=2))
wrapper.eval(c("outlook", "temperature", "humidity"), "playability", weatherr,
             rpart, args=list(minsplit=2))
wrapper.eval(names(weatherr)[-5], "playability", weatherr,
             rpart, args=list(minsplit=2))
}

\keyword{models}
