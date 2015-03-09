## multiply all rows of matrix m by the corresponding elements of vector v
rmm <- function(m, v) { as.matrix(m)*v }

## multiply all columns of matrix m by the corresponding elements of vector v
cmm <- function(m, v) { t(t(m)*v) }


if (FALSE)
{

  # usage examples
rmm(matrix(1:9, nrow=3), 10^(0:2))
rmm(matrix(1:12, nrow=4), 10^(0:3))

cmm(matrix(1:9, ncol=3), 10^(0:2))
cmm(matrix(1:12, ncol=4), 10^(0:3))

}
