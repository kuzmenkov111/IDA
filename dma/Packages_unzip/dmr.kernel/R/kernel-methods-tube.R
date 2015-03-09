## plot regression tube lines for linear regression
## with a single attributes
plot.tube <- function(w, data, eps, add=FALSE,
                      col.point="black", col.line="blue", ...)
{
  # y value corresponding to x on the regression line represented by w
  lry <- function(x, w) {sum(w*c(x, 1)) }

  if (!add)
    plot(data[,1], data[,2], col=col.point,
         xlab="a1", ylab="h", xlim=range(data[,1]), ylim=range(data[,2]), ...)

  lines(range(data[,1]), c(lry(min(data[,1]), w), lry(max(data[,1]), w)),
        col=col.line)
  lines(range(data[,1]), c(lry(min(data[,1]), w-c(0, eps)),
                           lry(max(data[,1]), w-c(0, eps))), col=col.line, lty=3)
  lines(range(data[,1]), c(lry(min(data[,1]), w+c(0, eps)),
                           lry(max(data[,1]), w+c(0, eps))), col=col.line, lty=3)
}


if (FALSE)
{

  # dataset for tube demonstration (take instances with similar a2 values)
kmdat.t <- kmdat.plot[abs(kmdat.plot$a2-mean(kmdat.plot$a2))<1,]
kmdat.t <- kmdat.t[sample(nrow(kmdat.t), 100), c("a1", "f")]

  # parameter vector for tube demonstration
w.t <- lm(f~a1, kmdat.t)$coef[2:1]

plot.tube(w.t, kmdat.t, eps=1)

}
