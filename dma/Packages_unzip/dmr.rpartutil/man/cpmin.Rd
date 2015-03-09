\name{cpmin}
\alias{cpmin}
\alias{cp1sd}
\alias{cpminrange}

\title{Uitlity functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

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
\code{\link{rpart.pmin}}
\code{\link{rpart.p1sd}}
\code{\link{rpart}}
\code{\link{prune.rpart}}
}

\examples{
library(rpart)

data(Vehicle, package="mlbench")

  # fully grown tree
v.tree.f <- rpart(Class~., Vehicle, minsplit=2, cp=0)

  # minimum-error cost-complexity pruning
v.tree.pmin <- prune(v.tree.f, cpmin(v.tree.f$cptable))
  # 1-SD cost-complexity pruning
v.tree.p1sd <- prune(v.tree.f, cp1sd(v.tree.f$cptable))
  # 8 pruned trees aroung the minimum-error (4 smaller, 3 larger)
v.trees.pmin8 <- lapply(cpminrange(v.tree.f$cptable, s=4, l=3),
                        function(cp) prune(v.tree.f, cp))
}

\keyword{models}
