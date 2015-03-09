\name{predict.dectree}
\alias{predict.dectree}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 3 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 3.5, Example 3.5.1.}

\arguments{See Section 3.5, Example 3.5.1.}

\details{See Section 3.5, Example 3.5.1.}

\value{See Section 3.5, Example 3.5.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{grow.dectree}}
\code{\link{predict.dectree.frac}}
}

\examples{
data(weather, package="dmr.data")
data(weatherc, package="dmr.data")

  # grow a decision tree for the weather data
tree <- grow.dectree(play~., weather)

  # grow a decision tree for the weatherc data
treec <- grow.dectree(play~., weatherc)

   # decision tree prediction for the weather data
predict(tree, weather)

  # decision tree prediction for the weatherc data
predict(treec, weatherc)
}

\keyword{models}
