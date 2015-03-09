\name{flevels}
\alias{flevels}

\title{Get factor levels as a factor}

\description{This function gets the set of factor levels in the form of a   factor rather than a character vector. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
flevels(v)
}

\arguments{
  \item{v}{a factor}
}

\details{The set of levels of \code{v} is converted to a factor with the
very same set of levels.
}

\value{\code{factor(levels(v), levels = levels(v))}
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

flevels(weather$outlook)
flevels(weather$outlook)[as.integer(weather$outlook)]==weather$outlook
}

\keyword{attribute}
