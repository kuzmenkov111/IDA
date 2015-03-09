\name{attr.type}
\alias{attr.type}

\title{Get a discrete/numeric attribute type information for a dataset}

\description{This function determines whether particular dataset columns represent discrete or continuous attributes. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
attr.type(...)
}

\arguments{
  \item{\dots}{one or more vectors or dataframes}
}

\details{The type of each input vector or each column of each input dataframe is
  checked using \code{is.numeric} and considered a continuous attribute
  if it returns \code{TRUE} or a discrete attribute otherwise.
}

\value{A character string of length equal to the total number of
   input vectors, consisting of \code{"c"} (for continuous) or
  \code{"d"} (for discrete) characters, representing the types of the
   respective attributes.
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
data(weatherr, package="dmr.data")
attr.type(weatherr$outlook, weatherr$playability)
attr.type(weatherr)
}

\keyword{classes}
