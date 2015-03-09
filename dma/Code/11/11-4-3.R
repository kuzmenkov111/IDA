spearman.sim <- function(x1, x2)
{
  cor(unlist(discode(~., x1)), unlist(discode(~., x2)), method="spearman",
      use="pairwise.complete.obs")
}

  # Spearman similarity matrix for the weathercl data
dissmat(weathercl, spearman.sim )
