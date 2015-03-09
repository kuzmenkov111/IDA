logit.inv <- function(q) { (e <- exp(q))/(e+1) }

curve(logit.inv(x), from=-5, to=5, xlab="q", ylab="logit.inv(q)")

p1.lt <- function(a1, a2) { logit.inv(lcg.plot(a1, a2)) }
h.lt <- function(a1, a2) { ustep(p1.lt(a1, a2), 0.5) }

lcdat.plot$p1.lt <- p1.lt(lcdat.plot$a1, lcdat.plot$a2)
lcdat.plot$h.lt <- h.lt(lcdat.plot$a1, lcdat.plot$a2)

wf.p1.lt <- wireframe(p1.lt~a1+a2, lcdat.plot, drape=TRUE, at=c(0, 0.5, 1),
                      col="transparent", col.regions=c("grey30", "grey70"),
                      colorkey=FALSE, zoom=0.8)
wf.h.lt <- wireframe(h.lt~a1+a2, lcdat.plot, col="grey50", zoom=0.8)
l.h.lt <- levelplot(h.lt~a1+a2, lcdat.plot, at=c(-100, 0, 100),
                    col.regions=c("grey30", "grey70"), colorkey=FALSE)

print(wf.g, split=c(1, 1, 2, 2), more=TRUE)
print(wf.p1.lt, split=c(2, 1, 2, 2), more=TRUE)
print(wf.h.lt, split=c(1, 2, 2, 2), more=TRUE)
print(l.h.lt, split=c(2, 2, 2, 2))
