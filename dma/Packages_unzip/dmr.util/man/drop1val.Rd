\name{drop1val}
\alias{drop1val}

\title{Drop attributes (columns) that only take single values for a dataset}

\description{This function drops data columns that only take a single value for all rows. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
drop1val(cnames, data)
}

\arguments{
  \item{cnames}{a character vector of column names to use}
  \item{data}{a dataframe}
}

\details{All columns with names not included in \code{cnames} or taking
only a single unique value for all rows of \code{data} are dropped and
the names of the remaining columns are returned.
}

\value{A vector of column names that appear in \code{cnames} and take at
least \code{2} unique values in \code{data}.
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
data(weather, package="dmr.data")

drop1val(names(weather), weather[1:3,])
drop1val(names(weather), weather[4:6,])
}

\keyword{manip}
