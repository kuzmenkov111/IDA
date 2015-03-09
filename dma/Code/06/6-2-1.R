v.rm <- matrix(0, nrow=nlevels(Vehicle$Class), ncol=nlevels(Vehicle$Class),
               dimnames=list(predicted=levels(Vehicle$Class),
                             true=levels(Vehicle$Class)))

v.rm["bus","opel"] <- 7
v.rm["bus","van"] <- 0.2
v.rm["bus","saab"] <- 7
v.rm["opel","bus"] <- 1.4
v.rm["opel","saab"] <- 1
v.rm["opel","van"] <- 1.4
v.rm["saab","bus"] <- 1.4
v.rm["saab","opel"] <- 1
v.rm["saab","van"] <- 1.4
v.rm["van","bus"] <- 0.2
v.rm["van","opel"] <- 7
v.rm["van","saab"] <- 7

  # two-class version
v01.rm <- matrix(0, nrow=nlevels(Vehicle01$Class), ncol=nlevels(Vehicle01$Class),
                 dimnames=list(predicted=levels(Vehicle01$Class),
                               true=levels(Vehicle01$Class)))
v01.rm["other","car"] <- 5
v01.rm["car","other"] <- 1

  # mean  misclassification cost for cost-insensitive models
v.mc.b <- list(tree=mean.cost(predict(v.tree, v.test, type="c"), v.test$Class, v.rm),
               nb=mean.cost(predict(v.nb, v.test), v.test$Class, v.rm))

v01.mc.b <- list(tree=mean.cost(predict(v01.tree, v01.test, type="c"),
                                v01.test$Class, v01.rm),
                 nb=mean.cost(predict(v01.nb, v01.test), v01.test$Class, v01.rm))
