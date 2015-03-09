library(dmr.claseval)
library(dmr.linreg)
library(dmr.regeval)
library(dmr.trans)
library(dmr.util)

library(lattice)

data(weatherc, package="dmr.data")
library(dmr.claseval)
library(dmr.linreg)
library(dmr.regeval)
library(dmr.trans)
library(dmr.util)

library(lattice)

data(weatherc, package="dmr.data")
data(PimaIndiansDiabetes, package="mlbench")

set.seed(12)
rpid <- runif(nrow(PimaIndiansDiabetes))
pid.train <- PimaIndiansDiabetes[rpid>=0.33,]
pid.test <- PimaIndiansDiabetes[rpid<0.33,]

set.seed(12)

  # dataset for surface plots
lcg.plot <- function(a1, a2) { 2*a1-3*a2+4 }
lcdat.plot <- `names<-`(expand.grid(seq(1, 5, 0.05), seq(1, 5, 0.05)), c("a1", "a2"))
lcdat.plot$g <- lcg.plot(lcdat.plot$a1, lcdat.plot$a2)
lcdat.plot$c <- as.factor(ustep(lcdat.plot$g))

  # datasets for parameter estimation examples
lcg <- function(a1, a2, a3, a4) { a1^2+2*a2^2-a3^2-2*a4^2+2*a1-3*a2+2*a3-3*a4+1 }
lcdat <- data.frame(a1=runif(400, min=1, max=5), a2=runif(400, min=1, max=5),
                    a3=runif(400, min=1, max=5), a4=runif(400, min=1, max=5))
lcdat$c <- as.factor(ustep(lcg(lcdat$a1, lcdat$a2, lcdat$a3, lcdat$a4)))
lcdat.train <- lcdat[1:200,]
lcdat.test <- lcdat[201:400,]

print(wf.g <- wireframe(g~a1+a2, lcdat.plot, col="grey50", zoom=0.8))
