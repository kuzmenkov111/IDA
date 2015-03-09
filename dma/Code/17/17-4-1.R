## natural binary multiclass encoding function
multi.enc.nbc <- function(d, class, clabs=levels(d))
{
  `colnames<-`(ed <- t(sapply(as.integer(d), int2binvec, length(clabs))),
               paste(class, 1:ncol(ed), sep="."))
}

## natural binary multiclass decoding function
multi.dec.nbc <- function(pred, clabs)
{
  clabs[clip.val(apply(pred, 1, binvec2int), 1, length(clabs))]
}

## generate a multiclass wrapper around alg using predf for prediction
## and the specified encoding and decoding functions
multi.class <- function(alg, predf=predict,
                        encode=multi.enc.nbc, decode=multi.dec.nbc)
{
  mc.alg <- function(formula, data, ...)
  {
    attributes <- x.vars(formula, data)
    class <- y.var(formula)
    aind <- names(data) %in% attributes
    clabs <- flevels(data[[class]])

    d.enc <- encode(data[[class]], class, clabs)
    binmodels <- lapply(1:ncol(d.enc),
                        function(i)
                        alg(make.formula(colnames(d.enc)[i], attributes),
                            `names<-`(cbind(data[,aind],
                                            factor(d.enc[,i], levels=0:1)),
                                      c(names(data[,aind]), colnames(d.enc)[i])),
                            ...))
    list(binmodels=binmodels, clabs=clabs)
  }

  mc.predict <- function(model, data, ...)
  {
    decode(sapply(model$binmodels, predf, data, ...), model$clabs)
  }

  list(alg=mc.alg, predict=mc.predict)
}

  # encoding class labels and decoding verification
v.nbc <- multi.enc.nbc(Vehicle$Class, "Class")
err(multi.dec.nbc(v.nbc, levels(Vehicle$Class)), Vehicle$Class)

  # basic encoding applied to rpart
rp.n <- multi.class(rpart, predf=function(...) predict(..., type="c"))
v.tree.n <- rp.n$alg(Class~., v.train)
v.tree.n.pred <- rp.n$predict(v.tree.n, v.test)
g.tree.n <- rp.n$alg(Type~., g.train)
g.tree.n.pred <- rp.n$predict(g.tree.n, g.test)

  # basic encoding applied to naive Bayes
nb.n <- multi.class(naiveBayes)
v.nb.n <- nb.n$alg(Class~., v.train)
v.nb.n.pred <- nb.n$predict(v.nb.n, v.test)
g.nb.n <- nb.n$alg(Type~., g.train)
g.nb.n.pred <- nb.n$predict(g.nb.n, g.test)
