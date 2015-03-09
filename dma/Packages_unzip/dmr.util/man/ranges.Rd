\name{ranges}
\alias{ranges}

\title{Determine the ranges (max-min differences) of numeric columns in a dataset}

\description{This function determines the ranges (max-min diffrences) of all numeric columns in a dataset. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
ranges(data)
}

\arguments{
  \item{data}{a dataframe or matrix}
}

\details{For each numeric column the difference between its maximum and
minimum values is calculated.
}

\value{A numeric vector of length equal to the number of columns of the
  input dataset and names the same as their names, containing the
  difference between the maximum and minimum value for numeric columns
  and \code{NA} for non-numeric columns.
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
ranges(matrix(1:12, ncol=4))
ranges(matrix(1:12, ncol=4, byrow=TRUE))
ranges(data.frame(a1=1:10, a2=2*(1:10)))

data(weatherr, package="dmr.data")
ranges(weatherr)
}

\keyword{arith}
