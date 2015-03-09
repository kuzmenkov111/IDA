\name{discode}
\alias{discode}
\alias{discode.a}
\alias{discode1}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 17 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 17.3, Example 17.3.5.}

\arguments{See Section 17.3, Example 17.3.5.}

\details{See Section 17.3, Example 17.3.5.}

\value{See Section 17.3, Example 17.3.5.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
}

\examples{
data(weatherc, package="dmr.data")

  # encoding a single attribute value
discode1(weatherc$outlook[1])
discode1(weatherc$outlook[1], red=TRUE)
discode1(factor(NA, levels=levels(weatherc$outlook)))
discode1(factor(NA, levels=levels(weatherc$outlook)), na.all=TRUE)

  # encoding single attributes of the weatherc data
discode.a(weatherc$outlook)
discode.a(weatherc$temperature)
discode.a(weatherc$outlook, b=c(-1,1), red=TRUE)
discode.a(weatherc$wind, b=c(-1,1))

  # encoding single instances of the weatherc data
discode(~., weatherc[1,])
discode(~., weatherc[1,], red=TRUE)

  # encoding the complete weatherc data
discode(~., weatherc)
  # leave the target attribute unchanged
discode(play~., weatherc)
}

\keyword{arith}
