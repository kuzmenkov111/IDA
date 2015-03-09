\name{linsep.sub}
\alias{linsep.sub}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 5 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 5.2, Example 5.2.5.}

\arguments{See Section 5.2, Example 5.2.5.}

\details{See Section 5.2, Example 5.2.5.}

\value{See Section 5.2, Example 5.2.5.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{repf.threshold}}
\code{\link{predict.par}}
}

\examples{
lcdat.ls <- linsep.sub(c~., lcdat)
lcdat.train.ls <- lcdat[1:200,][lcdat.ls[1:200],]
lcdat.test.ls <- lcdat[201:400,][lcdat.ls[201:400],]
}

\keyword{models}
