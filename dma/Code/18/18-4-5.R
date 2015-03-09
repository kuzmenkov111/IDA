## misclassification count break evaluation
evdisc.mcount <- function(b, bl, br, v, class)
{
  mcount <- function(cond) { sum(cond) - max(table(class[cond])) }

  mcount(v>bl & v<=br) - (mcount(v>bl & v<=b) + mcount(v>b & v<=br))
}

  # misclassification count bottom-up discretization for the weatherc data
disc.bottomup(play~., weatherc, 3, evalf=evdisc.mcount)

  # misclassification count bottom-up discretization for the Vehicle Silhouettes data
v.disc.bu.mc <- disc.bottomup(Class~., v.train, 7, evalf=evdisc.mcount)
summary(predict(v.disc.bu.mc, v.train))

  # misclassification count bottom-up discretization for the Glass data
g.disc.bu.mc <- disc.bottomup(Type~., g.train, 7, evalf=evdisc.mcount)
summary(predict(g.disc.bu.mc, g.train))
