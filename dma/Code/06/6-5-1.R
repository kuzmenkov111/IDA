## run misclassification costs experiments
mc.experiment <- function(algs, datasets, classes, rmax=10, m=25, k=10, n=1,
                          palgs=NULL, args.c=NULL, args.p=NULL, pargs=NULL,
                          predfs=predict, ppredfs.algs=predict,
                          ppredfs.palgs=predict)
{
  crossval.rs <- function(rs, ...) { .Random.seed <<- rs; crossval(...) }

  results <- NULL
  for (dn in 1:length(datasets))
  {
    res <- NULL

    data <- get(datasets[dn])
    class <- classes[dn]
    formula <- make.formula(class, ".")

    for (i in 1:m)
    {
      rho <- matrix(round(runif(nlevels(data[[class]])^2, min=1, max=rmax)-0.5),
                    nrow=nlevels(data[[class]]),
                    ncol=nlevels(data[[class]]),
                    dimnames=list(predicted=levels(data[[class]]),
                                  true=levels(data[[class]])))
      diag(rho) <- 0
      rhoc <- rhom2c(rho)

      for (an in 1:length(algs))
      {
        alg <- get(algs[[an]])
        palg <- if (!is.null(palgs[[an]])) get(palgs[[an]])
        arg.c <- args.c[[an]]
        arg.p <- args.p[[an]]
        parg <- pargs[[an]]
        predf <- if (is.vector(predfs)) predfs[[an]] else predfs
        ppredf.alg <- if (is.vector(ppredfs.algs)) ppredfs.algs[[an]]
                      else ppredfs.algs
        ppredf.palg <- if (is.vector(ppredfs.palgs)) ppredfs.palgs[[an]]
                       else ppredfs.palgs
        aname <- paste(algs[an], palgs[an], sep=".")

        if (is.null(palg))
        {
          alg.w <- mc.weight(alg, predf)
          alg.s <- mc.resample(alg, predf)
          alg.m <- mc.mincost(alg, ppredf.alg)
          alg.l <- mc.relabel(alg, pargs=arg.p, predf=predf, ppredf=ppredf.alg)
        }
        else
          alg.lp <- mc.relabel(alg, palg, pargs=parg,
                               predf=predf, ppredf=ppredf.palg)

        rs <- .Random.seed
        cv.b <- crossval(alg, formula, data, args=arg.c, predf=predf, k=k, n=n)
        if (is.null(palg))
        {
          cv.w <- crossval.rs(rs, alg.w$alg, formula, data,
                              args=c(list(rhoc), arg.c),
                              predf=alg.w$predict, k=k, n=n)
          cv.s <- crossval.rs(rs, alg.s$alg, formula, data,
                              args=c(list(rhoc), arg.c),
                              predf=alg.s$predict, k=k, n=n)
          cv.m <- crossval.rs(rs, alg.m$alg, formula, data,
                              args=c(list(rho), arg.p),
                              predf=alg.m$predict, k=k, n=n)
          cv.l <- crossval.rs(rs, alg.l$alg, formula, data,
                              args=c(list(rho), arg.c),
                              predf=alg.l$predict, k=k, n=n)
          mc <- data.frame(b=mean.cost(cv.b$pred, cv.b$true, rho),
                           w=mean.cost(cv.w$pred, cv.w$true, rho),
                           s=mean.cost(cv.s$pred, cv.s$true, rho),
                           m=mean.cost(cv.m$pred, cv.m$true, rho),
                           l=mean.cost(cv.l$pred, cv.l$true, rho))
          mc$d.w <- (mc$b-mc$w)/mc$b
          mc$d.s <- (mc$b-mc$s)/mc$b
          mc$d.m <- (mc$b-mc$m)/mc$b
          mc$d.l <- (mc$b-mc$l)/mc$b
          e <- data.frame(b=err(cv.b$pred, cv.b$true),
                          w=err(cv.w$pred, cv.w$true),
                          s=err(cv.s$pred, cv.s$true),
                          m=err(cv.m$pred, cv.m$true),
                          l=err(cv.l$pred, cv.l$true))
        }
        else
        {
          cv.lp <- crossval.rs(rs, alg.lp$alg, formula, data,
                               args=c(list(rho), arg.c),
                               predf=alg.lp$predict, k=k, n=n)
          mc <- data.frame(b=mean.cost(cv.b$pred, cv.b$true, rho),
                           lp=mean.cost(cv.lp$pred, cv.lp$true, rho))
          e <- data.frame(b=err(cv.b$pred, cv.b$true),
                          lp=err(cv.lp$pred, cv.lp$true))
          mc$d.lp <- (mc$b-mc$lp)/mc$b
        }

        res[[aname]]$mc <- rbind(res[[aname]]$mc, mc)
        res[[aname]]$e <- rbind(res[[aname]]$e, e)
      }
    }
    results <- c(results, list(res))
  }
  `names<-`(results, datasets)
}

  # experiments with decision trees and naive Bayes
  # for the Vehicle and Vehicle01 datasets
mc.res <- mc.experiment(c("rpart", "rpart", "naiveBayes"),
                        c("Vehicle", "Vehicle01"), c("Class", "Class"),
                        palgs=list(NULL, "bagging", NULL),
                        args.p=list(list(cp=0.025), list(cp=0.025), NULL),
                        pargs=list(NULL, list(control=rpart.control(cp=0.025)),
                                   NULL),
                        predfs=c(function(...) predict(..., type="c"),
                                 function(...) predict(..., type="c"), predict),
                        ppredfs.algs=c(predict, predict,
                                       function(...) predict(..., type="r")),
                        ppredfs.palgs=list(NULL,
                                           function(...) predict(..., type="p",
                                                                 aggregation="a"),
                                           NULL))

barplot(colMeans(cbind(mc.res$Vehicle$rpart.NULL$mc[,6:9],
                       mc.res$Vehicle$rpart.bagging$mc[,3])),
        main="Four-class, rpart", ylab="Cost reduction",
        las=2, ylim=c(-0.01, 0.11),
        names.arg=c("weight", "resample", "mincost", "relabel", "relabel.b"))
barplot(colMeans(mc.res$Vehicle$naiveBayes.NULL$mc[,7:9]),
        main="Four-class, naiveBayes", ylab="Cost reduction",
        las=2, ylim=c(-0.01, 0.11),
        names.arg=c("resample", "mincost", "relabel"))

barplot(colMeans(cbind(mc.res$Vehicle01$rpart.NULL$mc[,6:9],
                       mc.res$Vehicle01$rpart.bagging$mc[,3])),
        main="Two-class, rpart", ylab="Cost reduction",
        las=2, ylim=c(-0.26, 0.15),
        names.arg=c("weight", "resample", "mincost", "relabel", "relabel.b"))
barplot(colMeans(mc.res$Vehicle01$naiveBayes.NULL$mc[,7:9]),
        main="Two-class, naiveBayes", ylab="Cost reduction",
        las=2, ylim=c(-0.26, 0.15),
        names.arg=c("resample", "mincost", "relabel"))
