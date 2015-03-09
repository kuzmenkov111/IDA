\name{insert.ord}
\alias{insert.ord}

\title{Insert a value to a vector preserving the existing order}

\description{This function inserts a new value to an ordered vector with preserving the existing order. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
insert.ord(v, v1, decreasing = FALSE)
}

\arguments{
  \item{v}{an ordered numeric or character vector}
  \item{v1}{a numeric value or a character string}
  \item{decreasing}{a flag indicating whether \code{v} is ordered decreasingly or increasingly}
}

\details{The new value \code{v1} is inserted into \code{v} in such a way
that the ordering of \code{v} is preserved.
}

\value{A copy of \code{v} with \code{v1} inserted at the appropriate
order-preserving position.
}

\references{
}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
}

\examples{
insert.ord(1:10, 3.5)
insert.ord(10:1, 3.5, decreasing=TRUE)
insert.ord(1:10, 0)
insert.ord(1:10, 11)
}

\keyword{manip}
\keyword{arith}
