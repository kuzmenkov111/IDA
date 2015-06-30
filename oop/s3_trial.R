### S3 Class
## setting up independent classes and methods
# setting up line by line
die <- list(trial = character(0))
class(die) <- "Die"

coin <- list(trial = character(0))
class(coin) <- "Coin"

# setting up by constructors
get_die <- function(trials) {
  out <- list()
  out$trial <- trials
  class(out) <- "Die"
  out
}

get_coin <- function(trials) {
  out <- list()
  out$trial <- trials
  class(out) <- "Coin"
  out
}

# reset_events method
reset_events <- function(obj) {
  UseMethod("reset_events", obj)
  cat("Reset the events")
}

reset_events.default <- function(obj) {
  cat(paste("Not sure what to do with the object", class(obj)))
}

reset_events.Die <- function(obj) {
  obj$trial <- character(0)
  cat("Reset the events of die")
  obj
}

reset_events.Coin <- function(obj) {
  obj$trial <- character(0)
  cat("Reset the events of coin")
  obj
}

# using S3 generic function
print.Die <- function(obj) {
  if (length(obj$trial) == 0) {
    cat("No trial is made.")
  } else {
    cat(paste(length(obj$trial), "trials are made: "))
    cat(paste(obj$trial, collapse = ","))
  }
}

print.Coin <- function(obj) {
  if (length(obj$trial) == 0) {
    cat("No trial is made.")
  } else {
    cat(paste(length(obj$trial), "trials are made: "))
    cat(paste(obj$trial, collapse = ","))
  }
}

# test reset methods
die_trials <- c("3", "4", "1")
coin_trials <- c("H", "H", "T")

die$trial <- die_trials
die

all.equal(die, get_die(die_trials))

die <- reset_events(die)

coin$trial <- coin_trials
coin

coin <- reset_events(coin)
coin

reset_events(1:3)

## defining objects and inheritance
# constructors
get_base <- function() {
  out <- list(trial = character(0))
  class(out) <- append(class(out), "Base")
  out
}

get_die <- function() {
  out <- get_base()
  class(out) <- append(class(out), "Die")
  out
}

get_coin <- function() {
  out <- get_base()
  class(out) <- append(class(out), "Coin")
  out
}

# simulate_events method
simulate_events <- function(obj) {
  UseMethod("simulate_events", obj)
}

simulate_events.default <- function(obj) {
  warning("Default simulation method called on unrecognized object.")
  obj
}

simulate_events.Base <- function(obj) {
  obj <- reset_trials(obj)
  repeat {
    this_trial <- simulate_new(obj)
    obj <- append_event(obj, this_trial$trial)
    if (this_trial$success) break    
  }
  obj
}

# simulate_new method
simulate_new <- function(obj) {
  UseMethod("simulate_new", obj)
}

simulate_new.default <- function(obj) {
  cat("Unrecognized object found for simulate_new().")
}

simulate_new.Base <- function(obj) {
  NextMethod("simulate_new", obj)
}

simulate_new.Die <- function(obj) {
  trial <- as.integer(trunc(1 + runif(1, 0, 6)))
  list(trial = trial, success = (trial == 1))
}

simulate_new.Coin <- function(obj) {
  trial <- c("H", "T")[as.integer(trunc(1 + runif(1, 0, 2)))]
  list(trial = trial, success = (trial == "H"))
}

# append_event method
append_event <- function(obj, new_trial = NULL) {
  UseMethod("append_event", obj)
}

append_event.default <- function(obj, new_trial = NULL) {
  cat("Unsupported object entered")
}

append_event.Base <- function(obj, new_trial = NULL) {
  obj$trial <- c(obj$trial, new_trial)
  obj
}

# show_trials method
show_trials <- function(obj) {
  UseMethod("show_trials", obj)
}

show_trials.default <- function(obj) {
  cat("Unsupported object entered")
}

show_trials.Base <- function(obj) {
  obj$trial
}

# reset method
reset_trials <- function(obj) {
  UseMethod("reset_trials", obj)
}

reset_trials.default <- function(obj) {
  cat("An unsupported object entered")
}

reset_trials.Base <- function(obj) {
  obj$trial <- character(0)
  obj
}