\name{shift.right}
\alias{shift.right}

\title{Shift vector elements to the right}

\description{This function shifts vector elements to the right, filling the new first position with a specified value. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
shift.right(v, first = NA)
}

\arguments{
  \item{v}{a vector}
  \item{first}{a value to be inserted at the first position after shifting}
}

\details{All vector elements are shifted to the right: the original \code{v[length(v)]}
  disappears, the original \code{v[length(v)-1]} becomes the new \code{v[length(v)]},
  etc. The new \code{v[1]} value is set to \code{first} (defaulting to \code{NA}).

}

\value{The shifted version of \code{v}.
}

\references{
}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
\code{\link{shift.left}}
}

\examples{
shift.right(1:10)
}

\keyword{manip}
