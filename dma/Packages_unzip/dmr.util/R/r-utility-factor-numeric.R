## convert factor v to numeric starting from 0
as.num0 <- function(v) { if (is.factor(v)) as.numeric(v)-1 else as.numeric(v) }

## convert factor v to numeric preserving the character representation of levels
as.numchar <- function(v) { if (is.factor(v)) as.numeric(as.character(v)) else as.numeric(v) }


if (FALSE)
{

  # usage examples
as.num0(as.factor(0:9))
as.num0(as.factor(1:10))
as.numchar(as.factor(0:9))
as.numchar(as.factor(1:10))

}
