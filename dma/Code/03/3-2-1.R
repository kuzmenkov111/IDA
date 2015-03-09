dtdat <- expand.grid(a1=seq(1, 10, 3), a2=seq(1, 10, 3))
dtdat$c <- as.factor(ifelse(dtdat$a1<=7 & dtdat$a2<=1, 1,
                            ifelse(dtdat$a1<=7 & dtdat$a2<=7, 2,
                                   ifelse(dtdat$a1<=7, 3,
                                          ifelse(dtdat$a2<=4, 4, 5)))))
  # decision tree structure
prp(rpart(c~., dtdat, minsplit=2, cp=0))
  # the corresponding domain decomposition
levelplot(c~a1*a2, dtdat, at=0.5+0:5, col.regions=gray(seq(0.1, 0.9, 0.1)),
          colorkey=list(at=0.5+0:5))
