\name{x.vars}
\alias{x.vars}

\title{Extract the input attribute names from a formula for a dataset}

\description{This function extracts the input attribute names from an R formula for a given dataset. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
x.vars(formula, data)
}

\arguments{
  \item{formula}{a formula for R modeling functions}
  \item{data}{a dataframe}
}

\details{The names of all columns of \code{data} that are used as input
attributes according to \code{formula} are extracted.
}

\value{A character vector containing the input attribute names from \code{formula}.
}

\references{
}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
\code{\link{y.var}}, \code{\link{make.formula}}
}

\examples{
data(weather, package="dmr.data")

x.vars(play~outlook+temperature, weather)
x.vars(play~., weather)
}

\keyword{model}
