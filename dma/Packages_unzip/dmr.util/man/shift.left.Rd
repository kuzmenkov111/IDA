\name{shift.left}
\alias{shift.left}

\title{Shift vector elements to the left}

\description{This function shifts vector elements to the left, filling the new last position with a specified value. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
shift.left(v, last = NA)
}
\arguments{
  \item{v}{a vector}
  \item{last}{a value to be inserted at the last position after shifting}
}

\details{All vector elements are shifted to the left: the original \code{v[1]}
  disappears, the original \code{v[2]} becomes the new \code{v[1]},
  etc. The new \code{v[length(v)]} value is set to \code{last}
  (defaulting to \code{NA}).
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
\code{\link{shift.right}}
}

\examples{
shift.left(1:10)
}

\keyword{manip}
