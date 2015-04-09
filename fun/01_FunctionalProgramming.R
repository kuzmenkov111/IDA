### Motivation
## DRY - don't repeat yourself
set.seed(1014)
df <- data.frame(replicate(6, sample(c(1:10, -99), 6, rep = TRUE)))
names(df) <- letters[1:6]

fix_missing <- function(x) {
  x[x == -99] <- NA
  x
}
df[] <- lapply(df, fix_missing)

## 1. closure
missing_fixer <- function(na_value) {
  function(x) {
    x[x == na_value] <- NA
    x
  }
}
fix_missing_99 <- missing_fixer(-99)
fix_missing_999 <- missing_fixer(-999)

df[] <- lapply(df, fix_missing_99)

## extra argument - possible but not always work
fix_missing_any <- function(x, na_value) {
  x[x == na.value] <- NA
  x
}

## 2. storing functions in a list
mySummary <- function(x) {
  funs <- c(mean, median, sd, mad, IQR)
  lapply(funs, function(f) f(x, na.rm = TRUE))
}

sapply(df, mySummary)

### Anonymous functions
# check Common Higher-Order Functions in Functional Programming Languages - Filter?
# Reduce, Filter, Find, Map, Negate, Position

## function is not called
function(x) 3()

## need () for function to be called
(function(x) 3)()

## compare
f <- function(x) x + 3
f(10)
(function(x) x + 3)(10)

## exercises
sapply(mtcars, function(x) sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))

integrate((function(x) x*(x - 1)), 0, 10)
integrate((function(x) sin(x) - cos(x)), -pi, pi)
integrate((function(x) exp(x) / x), 10, 20)

### Closures
power <- function(exponent) {
  function(x) x ^ exponent
}
square <- power(2)
square(2)
square(4)

cube <- power(3)
cube(2)
cube(4)

# check parent environment (variables etc)
as.list(environment(square))
pryr::unenclose(square)

# parent environment is kept, memory is not cleared until it's removed
# function factory is a factory for making new functions eg missing_fixer(), power()

## Mutable state
new_counter <- function() {
  i <- 0
  function() {
    i <<- i + 1
    i
  }
}

counter_one <- new_counter()
counter_two <- new_counter()

counter_one()
counter_one()

counter_two()














