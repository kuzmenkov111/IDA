\name{skip.cond}
\alias{skip.cond}

\title{Skip vector/list elements that satisfy a condition}

\description{This function skips all vector or list elements that satisfy a given condition. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
skip.cond(l, cond)
}

\arguments{
  \item{l}{an input vector or list}
  \item{cond}{a one-argument boolean-valued function}
}

\details{The \code{cond} function is applied to all elements of \code{l}
and those for which it returns \code{TRUE} are skipped. In particular,
\code{l} can be a dataframe (a list of columns) and the function can be
then used to skip columns that satisfy a condition.
}

\value{A copy of \code{l} with all elements for which the \code{cond}
function returned \code{TRUE} skipped.
}

\references{
}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
\code{\link{skip.val}}
}

\examples{
skip.cond(1:10, function(e) e\%\%3==0)
skip.cond(as.list(1:10), function(e) e\%\%3==0)

data(weatherr, package="dmr.data")
skip.cond(weatherr, is.numeric)
}

\keyword{manip}
