dissmat <- function(data, diss)
{
  as.dist(outer(1:nrow(data), 1:nrow(data),
                Vectorize(function(i, j)
                          if (j<=i) diss(data[i,], data[j,]) else NA)),
          diag=TRUE, upper=TRUE)
}


if (FALSE)
{

  # dummy dissimilarity matrix for the weathercl data
dummy.diss <- function(x1, x2)
{ abs(as.integer(row.names(x1))-as.integer(row.names(x2))) }

dissmat(weathercl, dummy.diss)

}
