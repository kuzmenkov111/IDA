  # plot the entropy
curve(-x*log(x)-(1-x)*log(1-x), from=0, to=1,
      xlab="p", ylab="", ylim=c(-0.02, 0.7), lty=1)
  # and add the plot of the Gini index
curve(1-x^2-(1-x)^2, from=0, to=1, add=TRUE, lty=2)
legend("topright", legend=c("entropy", "gini"), lty=1:2)
