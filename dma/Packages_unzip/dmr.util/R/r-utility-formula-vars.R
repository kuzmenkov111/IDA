## extract input attribute names from a formula
x.vars <- function(formula, data)
{
  attr(terms(formula, data=data), "term.labels")
}

## extract the target attribute name from a formula
y.var <- function(formula)
{
  as.character(formula)[2]
}


if (FALSE)
{

  # usage examples
x.vars(play~outlook+temperature, weather)
x.vars(play~., weather)
y.var(play~.)

}
