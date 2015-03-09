## skip elements from list that satisfy the specified condition function
skip.cond <- function(l, cond)
{
  l[!sapply(l, cond)]
}

## skip elements from list that are equal to the specified value
skip.val <- function(l, val)
{
  skip.cond(l, function(e) e==val)
}


if (FALSE)
{

  # usage examples
skip.cond(1:10, function(e) e%%3==0)
skip.cond(as.list(1:10), function(e) e%%3==0)
skip.cond(weatherr, is.numeric)

skip.val(1:10, 3)
skip.val(as.list(1:10), 3)
skip.val(weather$outlook, "rainy")

}
