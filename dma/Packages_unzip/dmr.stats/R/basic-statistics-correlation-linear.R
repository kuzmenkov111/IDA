corl.test <- function(v1, v2)
{
  rho <- sum((v1-(m1 <- mean(v1)))*(v2-(m2 <- mean(v2))))/
           sqrt(sum((v1-m1)^2)*sum((v2-m2)^2))
  ts <- rho*sqrt((df <- length(v1)-2)/(1-rho^2))
  list(rho=rho, statistic=ts, p.value=2*(1-pt(abs(ts), df)))
}


if (FALSE)
{

  # demonstration
corl.test(weatherr$temperature, weatherr$playability)
cor.test(weatherr$temperature, weatherr$playability, method="pearson")
corl.test(weatherr$temperature, -weatherr$playability)
cor.test(weatherr$temperature, -weatherr$playability, method="pearson")

}
