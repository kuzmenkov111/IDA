  #  parameter vector for the lcg.plot function
w.plot <- c(2, -3, 4)
repf.linear(lcdat.plot[1:10,1:2], w.plot)
grad.linear(lcdat.plot[1:10,1:2], w.plot)
  # parametric model for the lcg.plot function
m.plot <- `class<-`(list(repf=repf.linear, w=w.plot), "par")
predict(m.plot, lcdat.plot[1:10,1:2])
