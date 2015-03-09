  # base model training set errors for the HouseVotes84 data
boxplot(list(tree.sx=hv.train.err.tree.sx,
	     tree.wx=hv.train.err.tree.wx,
	     tree.sa=hv.train.err.tree.sa,
	     tree.rnd=hv.train.err.tree.rnd,
	     nb.sx=hv.train.err.nb.sx,
	     nb.sa=hv.train.err.nb.sa),
        main="HouseVotes84 (train)", las=2, col="grey", ylim=c(0, 0.26))

  # base model test set errors for the HouseVotes84 data
boxplot(list(tree.sx=hv.test.err.tree.sx,
	     tree.wx=hv.test.err.tree.wx,
	     tree.sa=hv.test.err.tree.sa,
	     tree.rnd=hv.test.err.tree.rnd,
	     nb.sx=hv.test.err.nb.sx,
	     nb.sa=hv.test.err.nb.sa),
        main="HouseVotes84 (test)", las=2, col="grey", ylim=c(0, 0.26))

  # base model training set MSE values for the BostonHousing data
boxplot(list(tree.sx=bh.train.mse.tree.sx,
	     tree.wx=bh.train.mse.tree.wx,
	     tree.sa=bh.train.mse.tree.sa,
	     tree.rnd=bh.train.mse.tree.rnd,
	     lm.sx=bh.train.mse.lm.sx,
	     lm.wx=bh.train.mse.lm.wx,
             lm.sa=bh.train.mse.lm.sa),
        main="BostonHousing (train)", las=2, col="grey", ylim=c(0, 130))

  # base model test set MSE values for the BostonHousing data
boxplot(list(tree.sx=bh.test.mse.tree.sx,
	     tree.wx=bh.test.mse.tree.wx,
	     tree.sa=bh.test.mse.tree.sa,
	     tree.rnd=bh.test.mse.tree.rnd,
	     lm.sx=bh.test.mse.lm.sx,
	     lm.wx=bh.train.mse.lm.wx,
	     lm.sa=bh.test.mse.lm.sa),
        main="BostonHousing (test)", las=2, col="grey", ylim=c(0, 130))
