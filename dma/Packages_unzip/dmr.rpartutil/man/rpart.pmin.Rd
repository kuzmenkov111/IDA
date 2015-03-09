\name{rpart.pmin}
\alias{rpart.pmin}
\alias{rpart.p1sd}

\title{Utility functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Utility functions from Chapter 20 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 20.1.}

\arguments{See Section 20.1.}

\details{See Section 20.1.}

\value{See Section 20.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{cpmin}}
\code{\link{cp1sd}}
\code{\link{rpart}}
\code{\link{prune.rpart}}
}

\examples{
data(Vehicle, package="mlbench")

v.tree.pmin <- rpart.pmin(Class~., Vehicle)
v.tree.p1sd <- rpart.p1sd(Class~., Vehicle)
}

\keyword{models}
