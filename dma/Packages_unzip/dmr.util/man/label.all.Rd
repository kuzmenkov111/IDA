\name{label.all}
\alias{label.all}

\title{Convert a numeric vector to a factor using all specified levels and labels}

\description{This function converts a numeric vector to a factor with specified sets of levels and labels, possibly including levels not occurring in the input vector (which also have the corresponding labels assigned). It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
label.all(v, levels = NULL, labels = levels)
}

\arguments{
  \item{v}{an input vector}
  \item{levels}{a vector specifying the set factor levels}
  \item{labels}{a vector specifying the set of factor labels}
}

\details{The input vector \code{v} is converted to a factor with the set
of levels specified by the \code{levels} argument and the set of
corresponding labels specified by the \code{labels} argument. All levels
are assigned the corresponding labels, including those not occurring in \code{v}.
Values from \code{v} which do not appear in \code{levels} are replaced by
\code{NA}.
}

\value{If \code{levels!=NULL} then \code{factor(v, levels=levels, labels=labels)}
and \code{as.factor(v)} otherwise.
}

\references{
}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
\code{\link{label}}
}

\examples{
label.all(rep(1, 4), levels=0:1)
label.all(rep(1, 4), levels=0:1, labels=c("neg", "pos"))

data(weather, package="dmr.data")
label.all(weather$play[weather$play=="yes"], levels=c("no", "yes"))
label.all(weather$play[weather$play=="yes"], levels=c("no", "yes"), labels=c(0, 1))
}

\keyword{attribute}
