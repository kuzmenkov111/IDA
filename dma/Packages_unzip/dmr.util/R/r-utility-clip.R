## clip each value in v into the min:max range
clip.val <- function(v, min, max) { ifelse(v<min, min, ifelse(v>max, max, v)) }

if (FALSE)
{

  # usage example
clip.val(1:20, 5, 15)

}
