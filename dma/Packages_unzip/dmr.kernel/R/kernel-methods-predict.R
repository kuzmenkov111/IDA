## predict using a kernel-based model
predict.kernel <- function(model, data)
{
  attributes <- x.vars(model$formula, data)
  aind <- names(data) %in% attributes
  amat <- as.matrix(data[,aind,drop=FALSE])
  kmat <- do.call(model$kernel, c(list(amat, model$mat), model$kernel.args))
  rowSums(cmm(kmat, model$coef))+model$intercept
}


if (FALSE)
{

  # kernel models for producing plots
kmplot <- list(coef=c(rep(1, 50), rep(-2, 50)),
                mat=as.matrix(kmdat.plot[sample(nrow(kmdat.plot), 100),1:2]),
                intercept=1, formula=f~a1+a2)
kmplot.l <- `class<-`(c(kmplot, kernel=kernel.linear), "kernel")
kmplot.p <- `class<-`(c(kmplot, kernel=kernel.polynomial), "kernel")
kmplot.r <- `class<-`(c(kmplot, kernel=kernel.radial), "kernel")
kmplot.s <- `class<-`(c(kmplot, kernel=kernel.sigmoid), "kernel")

  # generate predictions using different kernel functions
kmdat.plot$hl <- predict(kmplot.l, kmdat.plot)
kmdat.plot$hp <- predict(kmplot.p, kmdat.plot)
kmdat.plot$hr <- predict(kmplot.r, kmdat.plot)
kmdat.plot$hs <- predict(kmplot.s, kmdat.plot)

  # plot prediction surfaces
wf.kl <- wireframe(hl~a1+a2, kmdat.plot, col="black", zoom=0.8)
wf.kp <- wireframe(hp~a1+a2, kmdat.plot, col="blue", zoom=0.8)
wf.kr <- wireframe(hr~a1+a2, kmdat.plot, col="red", zoom=0.8)
wf.ks <- wireframe(hs~a1+a2, kmdat.plot, col="green", zoom=0.8)

print(wf.kl, split=c(1, 1, 2, 2), more=TRUE)
print(wf.kp, split=c(2, 1, 2, 2), more=TRUE)
print(wf.kr, split=c(1, 2, 2, 2), more=TRUE)
print(wf.ks, split=c(2, 2, 2, 2))

}
