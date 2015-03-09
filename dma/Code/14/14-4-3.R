  # training set chi-square
chisq.test(i.pam2.euc$clustering, i.std.train[,5])
chisq.test(i.pam3.euc$clustering, i.std.train[,5])
chisq.test(i.pam5.euc$clustering, i.std.train[,5])
chisq.test(i.pam7.euc$clustering, i.std.train[,5])

chisq.test(i.pam2.man$clustering, i.std.train[,5])
chisq.test(i.pam3.man$clustering, i.std.train[,5])
chisq.test(i.pam5.man$clustering, i.std.train[,5])
chisq.test(i.pam7.man$clustering, i.std.train[,5])

  # test set chi-square
chisq.test(i.pam2.man.pred, i.std.test[,5])
chisq.test(i.pam3.man.pred, i.std.test[,5])
chisq.test(i.pam5.man.pred, i.std.test[,5])
chisq.test(i.pam7.man.pred, i.std.test[,5])

chisq.test(i.pam2.man.pred, i.std.test[,5])
chisq.test(i.pam3.man.pred, i.std.test[,5])
chisq.test(i.pam5.man.pred, i.std.test[,5])
chisq.test(i.pam7.man.pred, i.std.test[,5])

  # training set mutual information
mutinfo(i.pam2.euc$clustering, i.std.train[,5])
mutinfo(i.pam3.euc$clustering, i.std.train[,5])
mutinfo(i.pam5.euc$clustering, i.std.train[,5])
mutinfo(i.pam7.euc$clustering, i.std.train[,5])

mutinfo(i.pam2.man$clustering, i.std.train[,5])
mutinfo(i.pam3.man$clustering, i.std.train[,5])
mutinfo(i.pam5.man$clustering, i.std.train[,5])
mutinfo(i.pam7.man$clustering, i.std.train[,5])

  # test set mutual information
mutinfo(i.pam2.man.pred, i.std.test[,5])
mutinfo(i.pam3.man.pred, i.std.test[,5])
mutinfo(i.pam5.man.pred, i.std.test[,5])
mutinfo(i.pam7.man.pred, i.std.test[,5])

mutinfo(i.pam2.man.pred, i.std.test[,5])
mutinfo(i.pam3.man.pred, i.std.test[,5])
mutinfo(i.pam5.man.pred, i.std.test[,5])
mutinfo(i.pam7.man.pred, i.std.test[,5])
