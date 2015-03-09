cos.sim <- function(x1, x2) { cosine(discode(~., x1), discode(~., x2)) }

  # cosine similarity matrix for the weathercl data
dissmat(weathercl, cos.sim)
