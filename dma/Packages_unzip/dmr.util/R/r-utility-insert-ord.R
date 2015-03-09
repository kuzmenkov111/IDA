## insert new value v1 into sorted numeric vector v preserving order
insert.ord <- function(v, v1, decreasing=FALSE)
{
  idx <- clip.val(ifelse(decreasing,
                         suppressWarnings(max(which(v>v1))),
                         suppressWarnings(max(which(v<v1)))),
                  0, length(v))
  append(v, v1, after=idx)
}


if (FALSE)
{

  # usage examples
insert.ord(1:10, 3.5)
insert.ord(10:1, 3.5, decreasing=TRUE)
insert.ord(1:10, 0)
insert.ord(1:10, 11)

}
