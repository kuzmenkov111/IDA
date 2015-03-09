## dominating class count break evaluation
## using the total dominating class count after merging
evdisc.dccount1 <- function(b, bl, br, v, class) { max(table(class[v>bl & v<=br])) }

## dominating class count break evaluation
## using the sum of individual dominating class counts before merging
evdisc.dccount2 <- function(b, bl, br, v, class)
{
  max(table(class[v>bl & v<=b])) + max(table(class[v>b & v<=br]))
}

## dominating class count break evaluation
## using the minimum individual dominating class count before merging
evdisc.dccount3 <- function(b, bl, br, v, class, gamma=1/length(v))
{
  ccl <- table(class[v>bl & v<=b])  # class counts: left
  ccr <- table(class[v>b & v<=br])  # and right

  dcl <- which.max(ccl)  # dominating classes: left
  dcr <- which.max(ccr)  # and right

  dcmin <- ifelse(ccl[dcl]<=ccr[dcr], dcl, dcr)
  min(ccl[dcl], ccr[dcr]) - gamma*max(ccl[dcmin], ccr[dcmin])
}

  # dominating class count bottom-up discretization for the weatherc data
disc.bottomup(play~., weatherc, 3, evalf=evdisc.dccount1)
disc.bottomup(play~., weatherc, 3, evalf=evdisc.dccount2)
disc.bottomup(play~., weatherc, 3, evalf=evdisc.dccount3)

  # dominating class count bottom-up discretization for the Vehicle Silhouettes data
v.disc.bu.dc1 <- disc.bottomup(Class~., v.train, 7, evalf=evdisc.dccount1)
v.disc.bu.dc2 <- disc.bottomup(Class~., v.train, 7, evalf=evdisc.dccount2)
v.disc.bu.dc3 <- disc.bottomup(Class~., v.train, 7, evalf=evdisc.dccount3)
summary(predict(v.disc.bu.dc1, v.train))
summary(predict(v.disc.bu.dc2, v.train))
summary(predict(v.disc.bu.dc3, v.train))

  # dominating class count bottom-up discretization for the Glass data
g.disc.bu.dc1 <- disc.bottomup(Type~., g.train, 7, evalf=evdisc.dccount1)
g.disc.bu.dc2 <- disc.bottomup(Type~., g.train, 7, evalf=evdisc.dccount2)
g.disc.bu.dc3 <- disc.bottomup(Type~., g.train, 7, evalf=evdisc.dccount3)
summary(predict(v.disc.bu.dc1, g.train))
summary(predict(v.disc.bu.dc2, g.train))
summary(predict(v.disc.bu.dc3, g.train))
