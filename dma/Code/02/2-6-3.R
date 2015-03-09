par(mar=c(7, 4, 4, 2))
barplot(`names<-`(ave(weatherr$playability, weatherr$outlook, weatherr$wind),
                  interaction(weatherr$outlook, weatherr$wind)),
        las=2, main="Mean playability in outlook-wind subsets")
lines(c(0, 17), rep(mean(weatherr$playability), 2), lty=2)
