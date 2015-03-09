randindex <- function(clustering, classes)
{
  mean(outer(1:length(clustering), 1:length(classes),
             function(i, j)
             ifelse(i!=j,
                    clustering[i]==clustering[j] & classes[i]==classes[j] |
                    clustering[i]!=clustering[j] & classes[i]!=classes[j], NA)),
       na.rm=TRUE)
}


if (FALSE)
{

  # training set Rand index
randindex(i.pam2.euc$clustering, i.std.train[,5])
randindex(i.pam3.euc$clustering, i.std.train[,5])
randindex(i.pam5.euc$clustering, i.std.train[,5])
randindex(i.pam7.euc$clustering, i.std.train[,5])

randindex(i.pam2.man$clustering, i.std.train[,5])
randindex(i.pam3.man$clustering, i.std.train[,5])
randindex(i.pam5.man$clustering, i.std.train[,5])
randindex(i.pam7.man$clustering, i.std.train[,5])

  # test set Rand index
randindex(i.pam2.man$clustering, i.std.test[,5])
randindex(i.pam3.man$clustering, i.std.test[,5])
randindex(i.pam5.man$clustering, i.std.test[,5])
randindex(i.pam7.man$clustering, i.std.test[,5])

randindex(i.pam2.man.pred, i.std.test[,5])
randindex(i.pam3.man.pred, i.std.test[,5])
randindex(i.pam5.man.pred, i.std.test[,5])
randindex(i.pam7.man.pred, i.std.test[,5])

}
