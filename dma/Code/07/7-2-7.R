f.measure <- function(cm) { 1/mean(c(1/precision(cm), 1/recall(cm))) }

f.measure(s01.cm)
