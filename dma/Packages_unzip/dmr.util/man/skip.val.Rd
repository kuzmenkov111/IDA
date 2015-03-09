\name{skip.val}
\alias{skip.val}

\title{Skip vector elements equal to a value}

\description{This function skips all vector or list elements that are equal to a given value. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
skip.val(l, val)
}

\arguments{
  \item{l}{an input vector or list}
  \item{val}{a value that should be skipped}
}

\details{All elements of \code{l} are compared to \code{value} and
those that turn out equal are skipped.
}

\value{A copy of \code{l} with all elements equal \code{val} skipped.
}

\references{
}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
\code{\link{skip.cond}}
}

\examples{
skip.val(1:10, 3)
skip.val(as.list(1:10), 3)

data(weather, package="dmr.data")
skip.val(weather$outlook, "rainy")
}

\keyword{manip}
