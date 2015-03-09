## create a non-modeling discretization wrapper
disc.nm <- function(disc)
{ function(formula, data, k=5, ...) predict(disc(formula, data, k, ...), data) }

## non-modeling equal-width discretization
discnm.eqwidth <- disc.nm(disc.eqwidth)

## non-modeling equal-frequency discretization
discnm.eqfreq <- disc.nm(disc.eqfreq)


if (FALSE)
{

  # non-modeling discretization for the weatherc data
discnm.eqwidth(play~., weatherc, 4)
discnm.eqfreq(play~., weatherc, 3)
discnm.eqfreq(play~., weatherc, list(temperature=4, humidity=3))

}
