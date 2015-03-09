\name{make.formula}
\alias{make.formula}

\title{Make a formula using given target and input attributes}

\description{This function creates a formula object for R modeling with a given target attribute name and a set of input attribute names. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
make.formula(y.var, x.vars)
}

\arguments{
  \item{y.var}{a character string specifying the target attribute name}
  \item{x.vars}{a character vector specifying the set of input attribute names}
}

\details{The \code{x.vars} argument can be also \code{"."}, specifying that all
non-target attributes should be used as input attributes. Only simple single-attribute terms
combined using the \code{+} operator are used in the created formula.
}

\value{A formula \code{y~x1+x2+...} where \code{y} denotes the target
attribute name and \code{x1, x2, ...} denote the input attribute names,
or \code{y~.} if \code{x.vars=="."}.
}

\references{
}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
\code{\link{x.vars}}, \code{\link{y.var}}
}

\examples{
data(weather, package="dmr.data")

make.formula(names(weather)[5], names(weather)[1:3])
make.formula(names(weather)[5], ".")
}

\keyword{models}
