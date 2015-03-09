\name{clip.val}
\alias{clip.val}

\title{Clip numeric values to a given range}

\description{This function clips numeric values in an input vector to a specified range. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
clip.val(v, min, max)
}

\arguments{
  \item{v}{a numeric vector}
  \item{min}{the minimum value}
  \item{max}{the maximum value}
}

\details{The \code{min} and \code{max} arguments specify the permitted
  range of values in \code{v} and all values outsided this range are
  clipped to this range.
}

\value{A copy of \code{v} with all values below \code{min} replaced by
  \code{min} and all values above \code{max} replaced by \code{max}.

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
clip.val(1:20, 5, 15)
}

\keyword{manip}
\keyword{arith}
