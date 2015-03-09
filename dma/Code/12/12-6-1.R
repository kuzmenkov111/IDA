  # explicit decision tree representations of k-means models
i.kmeans.tree <- rpart(cluster~.,
                       cbind(i.train, cluster=as.factor(i.kmeans$clustering)),
                       minsplit=min(table(i.kmeans$clustering)), cp=0.05)
g.kmeans.tree <- rpart(cluster~.,
                       cbind(g.train, cluster=as.factor(g.kmeans$clustering)),
                       minsplit=min(table(g.kmeans$clustering)), cp=0.05)

  # cluster membership prediction tree plots
prp(i.kmeans.tree, varlen=0, faclen=0, main="Iris")
prp(g.kmeans.tree, varlen=0, faclen=0, main="Glass")

  # predicted vs. true clusters
confmat(predict(i.kmeans.tree, i.train, type="c"), i.kmeans$clustering)
confmat(predict(i.kmeans.tree, i.test, type="c"),
        predict(i.kmeans, i.std.test[,-5], euc.dist))

confmat(predict(g.kmeans.tree, g.train, type="c"), g.kmeans$clustering)
confmat(predict(g.kmeans.tree, g.test, type="c"),
        predict(g.kmeans, g.std.test[,-10], euc.dist))
