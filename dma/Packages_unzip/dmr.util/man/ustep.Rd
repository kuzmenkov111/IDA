\name{ustep}
\alias{ustep}

\title{Calculate the unit step function value}

\description{This function calculates the unit step values by comparing input argument against a threshold. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.

}

\usage{
ustep(v, thres = 0)
}

\arguments{
  \item{v}{a numeric vector}
  \item{thres}{a numeric threshold}
}

\details{The unit step value is \code{1} if the input argument is greater than
or equal to the threshold and \code{0} otherwise.
}

\value{A vector of unit step values for all input argument values from \code{v}.
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
ustep(seq(-1,1,0.25))
ustep(seq(-1,1,0.25), 0.5)
}

\keyword{arith}
