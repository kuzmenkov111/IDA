## entropy break evaluation
evdisc.entropy <- function(b, bl, br, v, class)
{
  sum(v>bl & v<=br)*(entropy(class[v>bl & v<=br])-
                       entropy.cond(class[v>bl & v<=br], v[v>bl & v<=br]<=b))
}


if (FALSE)
{

    # entropy bottom-up discretization for the weatherc data
disc.bottomup(play~., weatherc, 3, evalf=evdisc.entropy)

   # entropy bottom-up discretization for the Vehicle Silhouettes data
v.disc.bu.e <- disc.bottomup(Class~., v.train, 7, evalf=evdisc.entropy)
summary(predict(v.disc.bu.e, v.train))

   # entropy bottom-up discretization for the Glass data
g.disc.bu.e <- disc.bottomup(Type~., v.train, 7, evalf=evdisc.entropy)
summary(predict(g.disc.bu.e, v.train))

}
