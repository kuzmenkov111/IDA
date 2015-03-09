## create a simple y~x1+x2+... formula
make.formula <- function(y.var, x.vars)
{
  if (length(x.vars)==0)
    x.vars <- "1"
  as.formula(paste(y.var, paste(x.vars, collapse="+"), sep="~"))
}


if (FALSE)
{

  # usage example
make.formula(names(weather)[5], names(weather)[1:3])
make.formula(names(weather)[5], ".")

}
