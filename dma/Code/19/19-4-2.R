## discrete attribute correlation using binary encoding
discor <- function(a1, a2,
                   corf=function(a1, a2)
                        abs(cor(a1, a2, method="spearman", use="complete.obs")))
{
  switch(attr.type(a1, a2),
         cc=p12<-1,
         dc=p12<-as.matrix(pdisc(a1)),
         cd=p12<-t(as.matrix(pdisc(a2))),
         dd=p12<-pdisc(a1, a2))

  a1dc <- discode.a(a1, red=TRUE, na.all=TRUE)
  a2dc <- discode.a(a2, red=TRUE, na.all=TRUE)
  cor12 <- outer(1:ncol(a1dc), 1:ncol(a2dc),
                 Vectorize(function(i, j) corf(a1dc[,i], a2dc[,j])))
  weighted.mean(cor12, p12)
}

  # two continuous attributes
discor(weatherc$temperature, weatherc$humidity)
  # one discrete and one continuous attribute
discor(weatherc$outlook, weatherc$temperature)
discor(weatherc$temperature, weatherc$play)
  # two discrete attributes
discor(weatherc$outlook, weatherc$play)
  # attributes with missing values
discor(Soybean$seed, Soybean$roots)
