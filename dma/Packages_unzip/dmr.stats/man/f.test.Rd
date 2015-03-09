\name{f.test}
\alias{f.test}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.5, Example 2.5.9.}

\arguments{See Section 2.5, Example 2.5.9.}

\details{See Section 2.5, Example 2.5.9.}

\value{See Section 2.5, Example 2.5.9.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{bs.t.test}}
\code{\link{t.test}}
\code{\link{anova}}
}

\examples{
data(weatherc, package="dmr.data")

f.test(weatherc$temperature, weatherc$outlook)
f.test(weatherc$temperature, weatherc$play)
anova(lm(temperature~outlook, weatherc))
anova(lm(temperature~play, weatherc))
abs(sqrt(f.test(weatherc$temperature, weatherc$play)$statistic)-abs(t.test(temperature~play, weatherc, var.equal=TRUE)$statistic))
}

\keyword{multivariate}
