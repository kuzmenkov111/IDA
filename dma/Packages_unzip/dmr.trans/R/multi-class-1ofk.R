## 1-of-k multiclass encoding function
multi.enc.1ofk <- function(d, class, clabs=levels(d))
{
  `colnames<-`(sapply(clabs, function(cl) as.integer(d==cl)),
               paste(class, clabs, sep="."))
}

## 1-of-k multiclass encoding function
multi.dec.1ofk <- function(pred, clabs)
{
  clabs[max.col(pred)]
}


if (FALSE)
{

  # encoding class labels and decoding verification
v.1ofk <- multi.enc.1ofk(Vehicle$Class, "Class")
err(multi.dec.1ofk(v.1ofk, levels(Vehicle$Class)), Vehicle$Class)

  # 1-of-k encoding applied to rpart
rp.1 <- multi.class(rpart, predf=function(...) predict(...)[,2],
                    encode=multi.enc.1ofk, decode=multi.dec.1ofk)
v.tree.1 <- rp.1$alg(Class~., v.train)
v.tree.1.pred <- rp.1$predict(v.tree.1, v.test)
g.tree.1 <- rp.1$alg(Type~., g.train)
g.tree.1.pred <- rp.1$predict(g.tree.1, g.test)

  # 1-of-k encoding applied to naive Bayes
nb.1 <- multi.class(naiveBayes, predf=function(...) predict(..., type="r")[,2],
                    encode=multi.enc.1ofk, decode=multi.dec.1ofk)
v.nb.1 <- nb.1$alg(Class~., v.train)
v.nb.1.pred <- nb.1$predict(v.nb.1, v.test)
g.nb.1 <- nb.1$alg(Type~., g.train)
g.nb.1.pred <- nb.1$predict(g.nb.1, g.test)

}
