  # eps=0.5, cost=1
svr.t.05.1 <- svr.linear(f~., kmdat.t, solver="ipop", eps=0.5, cost=1)
w.t.05.1 <- svr.t.05.1$model$w
l2norm(w.t.05.1[-length(w.t.05.1)])

  # eps=0.5, cost=0.1
svr.t.05.01 <- svr.linear(f~., kmdat.t, solver="ipop", eps=0.5, cost=0.1)
w.t.05.01 <- svr.t.05.01$model$w
l2norm(w.t.05.01[-length(w.t.05.01)])

  # eps=0.5, cost=0.01
svr.t.05.001 <- svr.linear(f~., kmdat.t, solver="ipop", eps=0.5, cost=0.01)
w.t.05.001 <- svr.t.05.001$model$w
l2norm(w.t.05.001[-length(w.t.05.001)])

  # eps=1, cost=1
svr.t.1.1 <- svr.linear(f~., kmdat.t, solver="ipop", eps=1, cost=1)
w.t.1.1 <- svr.t.1.1$model$w
l2norm(w.t.1.1[-length(w.t.1.1)])

  # eps=1, cost=0.1
svr.t.1.01 <- svr.linear(f~., kmdat.t, solver="ipop", eps=1, cost=0.1)
w.t.1.01 <- svr.t.1.01$model$w
l2norm(w.t.1.01[-length(w.t.1.01)])

  # eps=1, cost=0.01
svr.t.1.001 <- svr.linear(f~., kmdat.t, solver="ipop", eps=1, cost=0.01)
w.t.1.001 <- svr.t.1.001$model$w
l2norm(w.t.1.001[-length(w.t.1.001)])

par(mfcol=c(3, 2))

plot.tube(w.t.05.1, kmdat.t, eps=0.5, main="eps=0.5, cost=1")
plot.tube(w.t.05.01, kmdat.t, eps=0.5, main="eps=0.5, cost=0.1")
plot.tube(w.t.05.001, kmdat.t, eps=0.5, main="eps=0.5, cost=0.01")

plot.tube(w.t.1.1, kmdat.t, eps=1, main="eps=1, cost=1")
plot.tube(w.t.1.01, kmdat.t, eps=1, main="eps=1, cost=0.1")
plot.tube(w.t.1.001, kmdat.t, eps=1, main="eps=1, cost=0.01")
