## pure-class discretization for a single attribute
disc.pure1 <- function(v, class, k=NULL)
{
  ord <- order(v)
  class <- class[ord]
  v <- v[ord]
  b <- diff(as.integer(class))!=0 & diff(v)!=0
  (v[1:(length(v)-1)][b]+v[2:length(v)][b])/2
}

## pure-class discretization for a dataset
disc.pure <- disc.all(disc.pure1)


if (FALSE)
{

  # pure-class discretization for the weatherc data
disc.pure(play~., weatherc)

  # pure-class discretization for the Vehicle Silhouettes data
v.disc.p <- disc.pure(Class~., v.train)
summary(predict(v.disc.p, v.train), maxsum=100)

  # pure-class discretization for the Glass data
g.disc.p <- disc.pure(Type~., g.train)
summary(predict(g.disc.p, g.train), maxsum=100)

}
