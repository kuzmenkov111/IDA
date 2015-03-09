## subset evaluation for attribute selection wrappers
wrapper.eval <- function(subset, target, data, alg, args=NULL, predf=predict,
                         perf=if (is.numeric(data[[target]])) mse else err,
                         evproc=crossval, evargs=list(k=5),
                         minrelsd=0.01, maxn=5)
{
  if (length(subset <- unique(subset))>0)
  {
    aind <- names(data) %in% subset
    ev <- do.call(evproc, c(list(alg, make.formula(target, subset), data,
                                 args=args, predf=predf),
                            evargs))
    p <- perf(ev$pred, ev$true)
    repeat
    {
      ev <- do.call(evproc, c(list(alg, make.formula(target, subset), data,
                                   args=args, predf=predf),
                              evargs))
      p <- c(p, perf(ev$pred, ev$true))
      if (length(p)>=maxn || sd(p)/abs(mean(p))<=minrelsd)
        break;
    }
    -mean(p)
  }
  else
    -Inf
}

  # wrapper evaluation for the weather data
wrapper.eval(c("outlook", "temperature"), "play", weather,
             rpart, args=list(minsplit=2),
             predf=function(...) predict(..., type="c"))
wrapper.eval(c("outlook", "temperature", "humidity"), "play", weather,
             rpart, args=list(minsplit=2),
             predf=function(...) predict(..., type="c"))
wrapper.eval(names(weather)[-5], "play", weather,
             rpart, args=list(minsplit=2),
             predf=function(...) predict(..., type="c"))

  # wrapper evaluation for the weatherc data
wrapper.eval(c("outlook", "temperature"), "play", weatherc,
             rpart, args=list(minsplit=2),
             predf=function(...) predict(..., type="c"))
wrapper.eval(c("outlook", "temperature", "humidity"), "play", weatherc,
             rpart, args=list(minsplit=2),
             predf=function(...) predict(..., type="c"))
wrapper.eval(names(weatherc)[-5], "play", weatherc,
             rpart, args=list(minsplit=2),
             predf=function(...) predict(..., type="c"))

  # wrapper evaluation for the weatherr data
wrapper.eval(c("outlook", "temperature"), "playability", weatherr,
             rpart, args=list(minsplit=2))
wrapper.eval(c("outlook", "temperature", "humidity"), "playability", weatherr,
             rpart, args=list(minsplit=2))
wrapper.eval(names(weatherr)[-5], "playability", weatherr,
             rpart, args=list(minsplit=2))
