set.seed(12)

  # dataset for surface plots
pcg.plot <- function(a1, a2) { sin(a1+a2)*(a1^2+2*a2^2-3*a1-5*a2) }
pcdat.plot <- `names<-`(expand.grid(seq(1, 5, 0.05), seq(1, 5, 0.05)), c("a1", "a2"))
pcdat.plot$g <- pcg.plot(pcdat.plot$a1, pcdat.plot$a2)

  # datasets for parameter estimation examples
pcg <- function(a1, a2, a3, a4) { a1^2+2*a2^2-a3^2-2*a4^2+2*a1-3*a2+2*a3-3*a4+1}
pcdat <- data.frame(a1=runif(400, min=1, max=5), a2=runif(400, min=1, max=5),
                    a3=runif(400, min=1, max=5), a4=runif(400, min=1, max=5))
pcdat$c <- as.factor(ustep(pcg(pcdat$a1, pcdat$a2, pcdat$a3, pcdat$a4)))
pcdat.train <- pcdat[1:200,]
pcdat.test <- pcdat[201:400,]


if (FALSE)
{

print(wf.g <- wireframe(g~a1+a2, pcdat.plot, col="green", zoom=0.8))

}
