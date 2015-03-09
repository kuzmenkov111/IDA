## calculate breaks in the middle between consecutive vector elements
midbrk <- function(v) { v[2:length(v)]-0.5*diff(v) }


if (FALSE)
{

   # usage example
midbrk(1:10)
midbrk(2*(1:10))
midbrk(sort(unique(weatherr$temperature)))

}
