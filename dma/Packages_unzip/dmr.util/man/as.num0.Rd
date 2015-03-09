\name{as.num0}
\alias{as.num0}

\title{Convert a factor to numeric values starting from 0}

\description{This function converts a factor to numeric values in the range from 0 to the number of levels minus one. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
as.num0(v)
}

\arguments{
  \item{v}{a vector or list of a factor type or another type that can be
  converted to numeric}
}

\details{The argument is converted to a numeric representation using
  \code{as.numeric}. The resulting values are reduced by \code{1} if
  \code{v} is a factor, to force them into the range between \code{0}
  and \code{nlevels(v)-1}.
}

\value{If \code{v} is a factor then \code{as.numeric(v)-1}, otherwise \code{as.numeric(v)}.
}

\references{
}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
\code{\link{as.numchar}}
}

\examples{
as.num0(as.factor(0:9))
as.num0(as.factor(1:10))
}

\keyword{manip}
\keyword{classes}
