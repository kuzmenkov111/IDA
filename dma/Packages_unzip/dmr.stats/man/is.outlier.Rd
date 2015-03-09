\name{is.outlier}
\alias{is.outlier}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.18.}

\arguments{See Section 2.4, Example 2.4.18.}

\details{See Section 2.4, Example 2.4.18.}

\value{See Section 2.4, Example 2.4.18.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{iqr}}
\code{\link{boxplot}}
}

\examples{
data(weatherc, package="dmr.data")

weatherc$temperature[is.outlier(weatherc$temperature, 0.5)]
boxplot(weatherc$temperature, range=0.5, plot=FALSE)
boxplot(weatherc$temperature, range=0.49, plot=FALSE)

}

\keyword{univar}
