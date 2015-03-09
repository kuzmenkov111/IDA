k.centers.adjust.dummy <- function(clustering, data, k, diss, dm)
{
  do.call(rbind, lapply(1:k, function(d) data[which.max(clustering==d),]))
}


if (FALSE)
{

  # dummy k-centers clustering for the weathercl data
k.centers(wcl.std, 3)

}
