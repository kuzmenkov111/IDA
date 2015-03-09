corr.test <- function(v1, v2) { corl.test(rank(v1), rank(v2)) }


if (FALSE)
{

  # demonstration
corr.test(weatherr$temperature, weatherr$playability)
cor.test(weatherr$temperature, weatherr$playability, method="spearman")
corr.test(weatherr$temperature, -weatherr$playability)
cor.test(weatherr$temperature, -weatherr$playability, method="spearman")

}
