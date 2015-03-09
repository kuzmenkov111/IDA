\name{arg.min}
\alias{arg.min}

\title{Find arguments that minimize a function}

\description{
This function applies a numeric-valued function to a vector or list of arguments and returns a specified number of arguments that yield the least function values. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
arg.min(args, fun, k = 1)
}

\arguments{
  \item{args}{a vector or list of function arguments}
  \item{fun}{a function that can be called with one argument and returns a numeric value}
  \item{k}{an integer specifying the number of arguments corresponding
  to the least function values to return}
}

\details{
The \code{fun} function is applied to each argument in \code{args} and
then the \code{k} arguments that produced the least function values are
returned. Ties are broken according to the original ordering of
arguments (using stable sorting to determine the least \code{k} values).
}

\value{
If \code{args} is a list, its sublist of length \code{k} containing the
\code{k} arguments that yield the least \code{fun} values. If
\code{args} is a vector, its subvector of length \code{k} containing the
\code{k} arguments that yield the least \code{fun} values.
}

\references{
}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
\code{\link{arg.max}}
}

\examples{
arg.min(1:10, sin)
arg.min(1:10, sin, 3)

data(weatherr, package="dmr.data")
arg.min(1:nrow(weatherr), function(i) weatherr$playability[i], 3)
arg.min(weatherr$temperature, function(temp) abs(temp-20), 3)
}

\keyword{arith}
