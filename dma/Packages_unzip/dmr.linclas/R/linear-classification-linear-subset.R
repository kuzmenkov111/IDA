## identify a linearly separable subset of data
linsep.sub <- function(formula, data)
{
  class <- y.var(formula)
  attributes <- x.vars(formula, data)
  aind <- names(data) %in% attributes

  wlm <- lm(make.formula(paste("(2*as.num0(", class, ")-1)", sep=""), attributes),
            data)$coef
  wpar <- c(wlm[-1], wlm[1])  # rearrange for predict.par
  predict.par(list(repf=repf.threshold(repf.linear), w=wpar), data[,aind])==
    data[[class]]
}

  # linearly separable training and test subsets
lcdat.ls <- linsep.sub(c~., lcdat)
lcdat.train.ls <- lcdat[1:200,][lcdat.ls[1:200],]
lcdat.test.ls <- lcdat[201:400,][lcdat.ls[201:400],]
