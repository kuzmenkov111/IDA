## chi-square break evaluation
evdisc.chisq <- function(b, bl, br, v, class)
{
  chisq.test(class[v>bl & v<=br], v[v>bl & v<=br]<=b, correct=FALSE)$statistic
}


if (FALSE)
{

  # chi-square bottom-up discretization for the weatherc data
disc.bottomup(play~., weatherc, 3,evalf=evdisc.chisq)

  # chi-square bottom-up discretization for the Vehicle Silhouettes data
v.disc.bu.chi <- disc.bottomup(Class~., v.train, 7, evalf=evdisc.chisq)
summary(predict(v.disc.bu.chi, v.train))

  # chi-square bottom-up discretization for the Glass data
g.disc.bu.chi <- disc.bottomup(Type~., g.train, 7, evalf=evdisc.chisq)
summary(predict(g.disc.bu.chi, g.train))

}
