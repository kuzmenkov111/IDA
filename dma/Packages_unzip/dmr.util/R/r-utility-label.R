## convert v to factor with given labels, assigned to its unique values in increasing order
label <- function(v, labels=NULL) { if (is.null(labels)) as.factor(v) else factor(v, labels=labels) }

## convert v to factor with given labels, assigned to al the values given in levels
label.all <- function(v, levels=NULL, labels=levels) { if (is.null(levels)) as.factor(v) else factor(v, levels=levels, labels=labels) }


if (FALSE)
{

  # usage examples
label(c(1, 0, 0, 1), c("neg", "pos"))
label(rep(1:3, 3), c("first", "second", "third"))
label(rep(3:1, 3), c("third", "second", "first"))
label(weather$play, c(0, 1))

label.all(rep(1, 4), levels=0:1)
label.all(rep(1, 4), levels=0:1, labels=c("neg", "pos"))
label.all(weather$play[weather$play=="yes"], levels=c("no", "yes"))
label.all(weather$play[weather$play=="yes"], levels=c("no", "yes"), labels=c(0, 1))

}
