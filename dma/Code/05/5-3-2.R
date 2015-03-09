## logit representation gradient
grad.logit <- function(repf, grad)
{ function(data, w) rmm(grad(data, w), (p <- repf.logit(repf)(data, w))*(1-p)) }

    # linear logit gradient for the "perfect" parameter vector
grad.logit(repf.linear, grad.linear)(lcdat.train[1:10,1:4], w.perf)
