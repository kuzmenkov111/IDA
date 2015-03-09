\name{grow.dectree}
\alias{grow.dectree}
\alias{as.data.frame.dectree}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 3 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 3.3, Example 3.3.8.}

\arguments{See Section 3.3, Example 3.3.8.}

\details{See Section 3.3, Example 3.3.8.}

\value{See Section 3.3, Example 3.3.8.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{predict.dectree}}
\code{\link{grow.dectree.frac}}
\code{\link{predict.dectree.frac}}
}

\examples{
data(weather, package="dmr.data")
data(weatherc, package="dmr.data")

  # grow a decision tree for the weather data
tree <- grow.dectree(play~., weather)

  # grow a decision tree for the weatherc data
treec <- grow.dectree(play~., weatherc)

  # data frame conversion
as.data.frame(tree)
as.data.frame(treec)

}

\keyword{models}
