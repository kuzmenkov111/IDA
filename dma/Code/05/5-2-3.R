h.t <- function(a1, a2) { ustep(lcg.plot(a1, a2)) }

lcdat.plot$h.t <- h.t(lcdat.plot$a1, lcdat.plot$a2)

wf.g.t <- wireframe(g~a1+a2, lcdat.plot, drape=TRUE, at=c(-100, 0, 100),
                    col="transparent", col.regions=c("grey30", "grey70"),
                    colorkey=FALSE, zoom=0.8)
wf.h.t <- wireframe(h.t~a1+a2, lcdat.plot, col="grey50", zoom=0.8)
l.h.t <- levelplot(h.t~a1+a2, lcdat.plot, at=c(0, 0.5, 1),
                   col.regions=c("grey30", "grey70"), colorkey=FALSE)

print(wf.g, split=c(1, 1, 2, 2), more=TRUE)
print(wf.g.t, split=c(2, 1, 2, 2), more=TRUE)
print(wf.h.t, split=c(1, 2, 2, 2), more=TRUE)
print(l.h.t, split=c(2, 2, 2, 2))
