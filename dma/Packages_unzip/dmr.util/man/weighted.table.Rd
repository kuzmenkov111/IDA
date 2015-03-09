\name{weighted.table}
\alias{weighted.table}

\title{Calculate the weighted table of discrete attribute value counts}

\description{This function generates a weighted frequency table for one or more value vectors and a given weight vector. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
weighted.table(v, ..., w=rep(1, length(v)), dnn=NULL)
}

\arguments{
  \item{v}{an input vector}
  \item{\dots}{possibly additional input vectors of the same length}
  \item{w}{a weights vector of the same length}
  \item{dnn}{a vector of dimension names for the output table}
}

\details{This function is a simple weighted counterpart of the
\code{table} function. For each combination of input vector values it
calculates the weighted occurrence count by summing up the corresponding
weights. If weights are all equal \code{1}, these are the same as the
ordinary occurrence counts returned by \code{table}.
}

\value{An object of the \code{table} class containing weighted
occurrence counts for all value combinations observed on input, i.e.,
for each input value combination, the sum of weights corresponding to
all observations for which it occurs.
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
weighted.table(weather$play)
weighted.table(weather$play, w=ifelse(weather$play=='yes', 2, 1))
weighted.table(weather$outlook, weather$play, dnn=c("outlook", "play"))
weighted.table(weather$outlook, weather$play, w=ifelse(weather$play=='yes', 2, 1))
}

\keyword{multivariate}
