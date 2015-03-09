\name{discor}
\alias{discor}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 19 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 19.4, Example 19.4.2.}

\arguments{See Section 19.4, Example 19.4.2.}

\details{See Section 19.4, Example 19.4.2.}

\value{See Section 19.4, Example 19.4.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{cfs.filter}}
\code{\link{discode.a}}
}

\examples{
data(weatherc, package="dmr.data")
data(Soybean, package="mlbench")

  # two continuous attributes
discor(weatherc$temperature, weatherc$humidity)
  # one discrete and one continuous attribute
discor(weatherc$outlook, weatherc$temperature)
discor(weatherc$temperature, weatherc$play)
  # two discrete attributes
discor(weatherc$outlook, weatherc$play)
  # attributes with missing values
discor(Soybean$seed, Soybean$roots)
}

\keyword{models}
