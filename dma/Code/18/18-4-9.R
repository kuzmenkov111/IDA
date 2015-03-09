   # misclassification count top-down discretization for the weatherc data
disc.topdown(play~., weatherc, 3, evalf=evdisc.mcount)
   # misclassification count top-down discretization for the Vehicle Silhouettes data
v.disc.td.mc <- disc.topdown(Class~., v.train, 7, evalf=evdisc.mcount)
summary(predict.disc(v.disc.td.mc, v.train))
   # misclassification count top-down discretization for the Glass data
g.disc.td.mc <- disc.topdown(Type~., g.train, 7, evalf=evdisc.mcount)
summary(predict.disc(g.disc.td.mc, g.train))

    # entropy top-down discretization for the weatherc data
disc.topdown(play~., weatherc, 3, evalf=evdisc.entropy)
   # entropy top-down discretization for the Vehicle Silhouettes data
v.disc.td.e <- disc.topdown(Class~., v.train, 7, evalf=evdisc.entropy)
summary(predict.disc(v.disc.td.e, v.train))
   # entropy top-down discretization for the Glass data
g.disc.td.e <- disc.topdown(Type~., g.train, 7, evalf=evdisc.entropy)
summary(predict.disc(g.disc.td.e, g.train))

    # chi-square top-down discretization for the weatherc data
disc.topdown(play~., weatherc, 3, evalf=evdisc.chisq)
   # chi-square top-down discretization for the Vehicle Silhouettes data
v.disc.td.chi <- disc.topdown(Class~., v.train, 7, evalf=evdisc.chisq)
summary(predict.disc(v.disc.td.chi, v.train))
   # chi-square top-down discretization for the Glass data
g.disc.td.chi <- disc.topdown(Type~., g.train, 7, evalf=evdisc.chisq)
summary(predict.disc(g.disc.td.chi, g.train))
