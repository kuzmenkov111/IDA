par(mfrow=c(1, 2))
boxplot(weatherr$playability, range=0.5, col="grey", main="playability")
boxplot(playability~outlook, weatherr, col="grey", main="playability")
