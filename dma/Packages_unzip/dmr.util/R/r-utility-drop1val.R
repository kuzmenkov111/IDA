## drop column names with just single unique values in data
drop1val <- function(cnames, data)
{
  na.omit(sapply(cnames, function (cn) { ifelse(length(unique(data[[cn]]))==1, NA, cn) }))
}

if (FALSE)
{

  # usage example
drop1val(names(weather), weather[1:3,])
drop1val(names(weather), weather[4:6,])

}
