ranges <- function(data)
{
  `names<-`(sapply(1:ncol(data),
                   function(i) ifelse(is.numeric(data[,i]), diff(range(data[,i])), NA)),
            names(data))
}


if (FALSE)
{

  # usage examples
ranges(matrix(1:12, ncol=4))
ranges(matrix(1:12, ncol=4, byrow=TRUE))
ranges(data.frame(a1=1:10, a2=2*(1:10)))
ranges(weatherr)

}
