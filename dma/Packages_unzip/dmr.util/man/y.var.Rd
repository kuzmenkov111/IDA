\name{y.var}
\alias{y.var}

\title{Extract the target attribute name from a formula}

\description{This function extracts the target attribute name from an R formula for a given dataset. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
y.var(formula)
}

\arguments{
  \item{formula}{a formula for R modeling functions}
}

\details{The name of the column of \code{data} that is used as the target
attribute according to \code{formula} is extracted.
}

\value{A character vector containing the target attribute name from \code{formula}.
}

\references{
}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
\code{\link{x.vars}}, \code{\link{make.formula}}
}

\examples{
y.var(play~.)
}

\keyword{model}
