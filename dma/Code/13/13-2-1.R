dg.l4 <- lapply(1:8, function(i)
                     {
                       d <- list(i)
                       attr(d, "members") <- 1
                       attr(d, "height") <- 0
                       attr(d, "leaf") <- TRUE
                       attr(d, "label") <- i
                       `class<-`(d, "dendrogram")
                     })

dgmerge <- function(dg, i1, i2)
{
  d <- list(dg[[i1]], dg[[i2]])
  attr(d, "members") <- attr(d[[1]], "members")+attr(d[[2]], "members")
  attr(d, "height") <- 1+max(attr(d[[1]], "height"), attr(d[[2]], "height"))
  attr(d, "leaf") <- FALSE
  lab <- if (is.null(attr(d[[1]], "edgetext"))) "label" else "edgetext"
  attr(d, "edgetext") <- paste(attr(d[[1]], lab), attr(d[[2]], lab), sep="+")
  `class<-`(d, "dendrogram")
}

dg.l3 <- lapply(seq(1, length(dg.l4)-1, 2), function(i) dgmerge(dg.l4, i, i+1))
dg.l2 <- lapply(seq(1, length(dg.l3)-1, 2), function(i) dgmerge(dg.l3, i, i+1))
dg.l1 <- lapply(seq(1, length(dg.l2)-1, 2), function(i) dgmerge(dg.l2, i, i+1))

plot(dg.l1[[1]], center=TRUE)
