## instance count break evaluation
## using the total instance count after merging
evdisc.incount1 <- function(b, bl, br, v, class) { sum(v>bl & v<=br) }

## instance count break evaluation
## using the minimum individual instance count before merging
evdisc.incount2 <- function(b, bl, br, v, class, gamma=1/length(v))
{
  min(sum(v>bl & v<=b), sum(v>b & v<=br)) +
    gamma*max(sum(v>bl & v<=b), sum(v>b & v<=br))
}


if (FALSE)
{

  # instance count bottom-up discretization for the weatherc data
disc.bottomup(play~., weatherc, 3, evalf=evdisc.incount1)
disc.bottomup(play~., weatherc, 3, evalf=evdisc.incount2)

  # instance count bottom-up discretization for the Vehicle Silhouettes data
v.disc.bu.ic1 <- disc.bottomup(Class~., v.train, 7, evalf=evdisc.incount1)
v.disc.bu.ic2 <- disc.bottomup(Class~., v.train, 7, evalf=evdisc.incount2)
summary(predict(v.disc.bu.ic1, v.train))
summary(predict(v.disc.bu.ic2, v.train))

  # instance count bottom-up discretization for the Glass data
g.disc.bu.ic1 <- disc.bottomup(Type~., g.train, 7, evalf=evdisc.incount1)
g.disc.bu.ic2 <- disc.bottomup(Type~., g.train, 7, evalf=evdisc.incount2)
summary(predict(g.disc.bu.ic1, g.train))
summary(predict(g.disc.bu.ic2, g.train))

}
