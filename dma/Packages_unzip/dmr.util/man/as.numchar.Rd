\name{as.numchar}
\alias{as.numchar}

\title{Convert a factor to numeric values with the same character
  representation}

\description{This function converts a factor to numeric values which have the same character representation as the original factor labels (rather than consecutive integers from 1). It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
as.numchar(v)
}

\arguments{
  \item{v}{a vector or list of a factor type or another type that can be
  converted to numeric}
}

\details{The input factor is assumed to have labels that can be
  converted to numbers. It is converted to a character representation using
  \code{as.character} (yielding factor labels) and then to a numeric
  representation using \code{as.numeric}.
}

\value{If \code{v} is a factor then \code{as.numeric(as.character(v))}, otherwise \code{as.numeric(v)}.
}

\references{
}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
\code{\link{as.num0}}
}

\examples{
as.numchar(as.factor(0:9))
as.numchar(as.factor(1:10))
}


\keyword{manip}
\keyword{classes}
