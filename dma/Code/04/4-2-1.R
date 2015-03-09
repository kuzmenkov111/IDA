## calculate the posterior probability given prior and inverse probabilities
bayes.rule <- function(prior, inv)
{
  prior*inv/sum(prior*inv)
}

  # posterior probabilities
bayes.rule(c(0.2, 0.3, 0.5), c(0.9, 0.9, 0.5))

  # let P(burglery)=0.001,
  # P(alarm|burglery)=0.95,
  # P(alarm|not burglery)=0.005
  # calculate P(burglery|alarm)
bayes.rule(c(0.001, 0.999), c(0.95, 0.005))[1]
