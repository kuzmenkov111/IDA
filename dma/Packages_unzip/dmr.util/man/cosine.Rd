\name{cosine}
\alias{cosine}

\title{Calculate the cosine of two vectors}

\description{This function calculates the cosine of the angle between two numeric vectors. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
cosine(v1, v2)
}

\arguments{
  \item{v1}{a numeric vector}
  \item{v2}{a numeric vecor}
}

\details{The cosine of the angle between \code{v1} and \code{v2} is
calculated as their dot product divided by the product of their L2 norms.
}

\value{The cosine of the angle between \code{v1} and \code{v2}.

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
cosine(1:5, 1:5)
cosine(1:5, 5:1)
cosine(1:5, -1:-5)
cosine(1:5, rep(3,5))
}

\keyword{math}
