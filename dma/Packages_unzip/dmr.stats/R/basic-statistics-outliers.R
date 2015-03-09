is.outlier <- function(v, b=1.5)
{ v<(q <- quantile(v, c(0.25, 0.75)))[1]-b*(r <- diff(q)) | v>q[2]+b*r }


if (FALSE)
{

weatherc$temperature[is.outlier(weatherc$temperature, 0.5)]
boxplot(weatherc$temperature, range=0.5, plot=FALSE)
boxplot(weatherc$temperature, range=0.49, plot=FALSE)

}
