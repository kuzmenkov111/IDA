pearson.sim <- function(x1, x2)
{
  cor(unlist(discode(~., x1)), unlist(discode(~., x2)), method="pearson",
      use="pairwise.complete.obs")
}


if (FALSE)
{

  # Pearson similarity matrix for the weathercl data
dissmat(weathercl, pearson.sim )

}
