## get k arguments with the least values of the given function
arg.min <- function(args, fun, k=1) { args[order(sapply(args, fun))][1:min(k, length(args))] }

## get k arguments with the greatest values of the given function
arg.max <- function(args, fun, k=1) { args[order(sapply(args, fun), decreasing=T)][1:min(k, length(args))] }


if (FALSE)
{

  # usage examples
arg.min(1:10, sin)
arg.min(1:10, sin, 3)
arg.min(1:nrow(weatherr), function(i) weatherr$playability[i], 3)
arg.min(weatherr$temperature, function(temp) abs(temp-20), 3)

arg.max(1:10, sin)
arg.max(1:10, sin, 3)
arg.max(1:nrow(weatherr), function(i) weatherr$playability[i], 3)
arg.max(weatherr$temperature, function(temp) abs(temp-20), 3)

}
