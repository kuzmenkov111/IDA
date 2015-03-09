## functional margin of w with respect to instances from data
## using the cvec vector of {-1, 1} class labels
fmarg <- function(w, data, cvec)
{ cvec*predict.par(list(repf=repf.linear, w=w), data) }

## geometric margin of w with respect to instances from data
## using the cvec vector of {-1, 1} class labels
gmarg <- function(w, data, cvec) { fmarg(w, data, cvec)/l2norm(w[-length(w)]) }

## plot separating and b-margin lines for linear threshold classification
## with 2 attributes
plot.margin <- function(w, data, cvec, b=1, add=FALSE,
                        col.sep="black", col.pos="red", col.neg="blue", ...)
{
  # y value corresponding to x on the regression line represented by w
  lry <- function(x, w) {sum(-w[c(1,3)]/w[2]*c(x, 1)) }

  if (!add)
  {
    plot(data[,1][cvec==1], data[,2][cvec==1], col=col.pos,
       xlab="a1", ylab="a2", xlim=range(data[,1]), ylim=range(data[,2]), ...)
    points(data[,1][cvec!=1], data[,2][cvec!=1], col=col.neg, ...)
  }

  lines(range(data[,1]), c(lry(min(data[,1]), w),
                           lry(max(data[,1]), w)), col=col.sep, ...)
  lines(range(data[,1]), c(lry(min(data[,1]), w-c(0, 0, b)),
                           lry(max(data[,1]), w-c(0, 0, b))), col=col.pos, ...)
  lines(range(data[,1]), c(lry(min(data[,1]), w+c(0, 0, b)),
                           lry(max(data[,1]), w+c(0, 0, b))), col=col.neg, ...)
  list(fmargin=min(fmarg(w, data, cvec)), gmargin=min(gmarg(w, data, cvec)))
}


if (FALSE)
{

  # dataset for margin illustration (skip near-boundary instances from kmdat.plot)
kmdat.m <- kmdat.plot[abs(kmdat.plot$f)>2,c("a1", "a2", "c")]
kmdat.m <- kmdat.m[sample(nrow(kmdat.m), 100),]

  # parameter vector for margin demonstration
w.m <- c(1, -2)
  # predictions with intercept 0
p0.m <- predict.par(list(repf=repf.linear, w=c(w.m, 0)), kmdat.m[,1:2])
 # symmetric-margin intercept
w.m <- c(w.m, -(max(p0.m[kmdat.m$c==0])+min(p0.m[kmdat.m$c==1]))/2)

  # minimum functional margin
min(fmarg(w.m, kmdat.m[,1:2], 2*as.num0(kmdat.m$c)-1))
  # minimum geometric
min(gmarg(w.m, kmdat.m[,1:2], 2*as.num0(kmdat.m$c)-1))

  # scale parameters to get minimum functional margin of 1
w.m <- w.m/min(fmarg(w.m, kmdat.m[,1:2], 2*as.num0(kmdat.m$c)-1))
  # minimum functional margin after parameter scaling (1)
min(fmarg(w.m, kmdat.m[,1:2], 2*as.num0(kmdat.m$c)-1))
  # minimum geometric margin after parameter scaling (unchanged)
min(gmarg(w.m, kmdat.m[,1:2], 2*as.num0(kmdat.m$c)-1))

plot.margin(w.m, kmdat.m[,1:2], 2*as.num0(kmdat.m$c)-1)

}
