### S3 Class
## set up classes and methods
die <- list(trials = character(0))
class(die) <- "Die"

coin <- list(trials = character(0))
class(coin) <- "Coin"

reset <- function(obj) {
  UseMethod("reset", obj)
  print("Reset the trials")
}

reset.default <- function(obj) {
  print("Uh oh, not sure what to do here!\n")
  obj
}

reset.Die <- function(obj) {
  obj$trials <- character(0)
  print("Reset the die\n")
  obj
}

reset.Coin <- function(obj) {
  obj$trials <- character(0)
  print("Reset the coin\n")
  obj
}

## test reset methods
die$trials <- c("3", "4", "1")
die

die <- reset(die) #reset.Die()
die

coin$trials <- c("H", "H", "T")
coin

coin <- reset(coin) #reset.Coin()
coin

reset(1:3) #reset.default()

## defining objects and inheritance
geo_trial <- function() {
  me <- list(history = character(0))
  class(me) <- append(class(me), "geo_trial")
  me
}

die_trial <- function() {
  me <- geo_trial()
  class(me) <- append(class(me), "die_trial")
  me
}

coin_trial <- function() {
  me <- geo_trial()
  class(me) <- append(class(me), "coin_trial")
  me
}

sim <- function(obj) {
  UseMethod("sim", obj)
}

sim.default <- function(obj) {
  warning("Default simulation method called on unrecognized object.")
  obj
}

sim.geo_trial <- function(obj) {
  obj <- reset(obj)
  repeat {
    this_trial <- single_trial(obj)
    obj <- append_event(obj, this_trial$result)
    if(this_trial$success) break
  }
  obj
}

single_trial.default <- function(obj) {
  warning("Unrecognized object found for single_trial().")
  list(result = "1", success = TRUE)
}

single_trial.geo_trial <- function(obj) {
  NextMethod("single_trial", obj)
}

single_trial.coin_trial <- function(obj) {
  value <- as.character(cut(as.integer(1 + trunc(runif(1, 0, 2))), c(0, 1, 2), lebels = c("M", "T")))
  list(result = value, success = (value == 1))
}

single_trial.die_trial <- function(obj) {
  value <- as.integer(1 + trunc(runif(1, 0, 6)))
  list(result = value, success = (value == 1))
}

coin <- coin_trial()
coin <- sim(coin)