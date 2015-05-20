power <- function(x, n) {
  if(n >= 1) x * power(x, n-1)
  else 1
}

## error to run power 10000 times with defalut option
options()$expressions
#[1] 5000

power(2, 1e3)
#[1] 1.071509e+301

power(2, 1e4)
#Error: evaluation nested too deeply: infinite recursion / options(expressions=)?
#Error during wrapup: evaluation nested too deeply: infinite recursion / options(expressions=)?

## expressions parameter updated and results in stack overflow
options(expressions = 100000)
power(2, 1e4)
#Error: protect(): protection stack overflow

## tail recursion supported in Scala
power.rec <- function(x, n, t = 1) {
  if(n < 1) t
  else power.rec(x, n-1, x*t)
}

# turn to default and even worse
options(expressions = 5000)
power.rec(2, 1e3)
#Error: evaluation nested too deeply: infinite recursion / options(expressions=)?
#Error during wrapup: evaluation nested too deeply: infinite recursion / options(expressions=)?

# works in updated parameter
options(expressions = 100000)
power.rec(2, 1e3)
#[1] 1.071509e+301

# stack overflow error again
power.rec(2, 1e4)
#Error: protect(): protection stack overflow

#Getting paired permutations in R
#http://stackoverflow.com/questions/29724857/getting-paired-permutations-in-r/29725717#29725717
names <- c("a", "b", "c", "d")

lst <- lapply(combn(names, 2, simplify = FALSE), function(x) {
  list(x, names[!(names %in% x)])
})

fst <- lapply(lst, function(x) x[[1]])
snd <- lapply(lst, function(x) x[[2]])

lapply(snd, function(x) {
  x %in% fst
})


allocate <- function(data, n) {
  lst <- lapply(combn(data, n, simplify = FALSE), function(x) {
    cbind(paste(sort(x), collapse=","), paste(sort(data[!(data %in% x)]), collapse=","))
  })
  lst[1:as.integer(length(lst)/2)]
}

allocate.rec <- function(data, n) {
  if(n == 4) {
    lst <- lapply(combn(data, n, simplify = FALSE), function(x) {
      list(x, data[!(data %in% x)])
    })
    lst[1:as.integer(length(lst)/2)]
  } else {
    
  }
}

lapply(combn(names, 2, simplify = FALSE), function(x) {
  cbind(paste(x, collapse = ","), paste(names[!(names %in% x)], collapse = ","))
})


allocate <- function(data) {
  if(length(data) == 4) {
    lst <- lapply(combn(data, 2, simplify = FALSE), function(x) {
      cbind(paste(x, collapse = ","), paste(data[!(data %in% x)], collapse = ","))
    })
    as.data.frame(do.call(rbind, lst[1:as.integer(length(lst)/2)]))
  }
  else {
    lapply(combn(data, 2, simplify = FALSE), function(x) {
      data.frame(V0 = paste(x, collapse = ","), allocate(data[!(data %in% x)]))
    })
  }
}

tmp <- allocate(letters[1:6])




































