\name{label}
\alias{label}

\title{Convert numerical values to a factor using specified labels}

\description{This function converts a numeric vector to a factor with a specified set of labels. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more
details.
}

\usage{
label(v, labels = NULL)
}

\arguments{
  \item{v}{an input vector}
  \item{labels}{a vector specifying the set of labels}
}

\details{The input vector \code{v} is converted to a factor with the set
of levels corresponding to the unique values appearing in \code{v}, assigned
labels specified by the \code{labels} argument. If missing, the default
\code{as.factor(v)} conversion is applied.
}

\value{If \code{labels!=NULL} then \code{factor(v, labels=labels)} and
\code{as.factor(v)} otherwise.
}

\references{
}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
\code{\link{label.all}}
}

\examples{
label(c(1, 0, 0, 1), c("neg", "pos"))
label(rep(1:3, 3), c("first", "second", "third"))
label(rep(3:1, 3), c("third", "second", "first"))

data(weather, package="dmr.data")
label(weather$play, c(0, 1))
}

\keyword{attribute}
