### Subsetting applications
## lookup tables (character subsetting)
x <- c("m","f","u","f","f","m","m")
lookup <- c(m = "Male", f = "Female", u = NA)
lookup[x]
unname(lookup[x])

## matching and merging by hand (integer subsetting)
grades <- c(1, 2, 2, 3, 1)
info <- data.frame(grade = 3:1, desc = c("Excellent", "Good", "Poor"), fail = c(F, F, T))

# using match
id <- match(grades, info$grade)
info[id, ]

# using rownames
rownames(info) <- info$grade
info[as.character(grades), ]

# using merge
merge(data.frame(grade = grades), info, by = "grade", all.x = TRUE)

# by multiple columns
# interaction() for factors and collapse() for others

## random samples/bootstrap (integer subsetting)
df <- data.frame(x = rep(1:3, each = 2), y = 6:1, z = letters[1:6])
set.seed(10)
df[sample(nrow(df)), ]
df[sample(nrow(df), 3), ]
df[sample(nrow(df), 6, rep = T), ]

## ordering (integer subsetting)
x <- c("b", "c", "a")
order(x)
x[order(x)]

set.seed(10)
df2 <- df[sample(nrow(df)), 3:1]
df2[order(df2$x), ]
df2[order(names(df2))]
df2[, order(names(df2))]

## expanding aggregated counts (integer subsetting)
df <- data.frame(x = c(2, 4, 1), y = c(9, 11, 6), n = c(3, 5, 1))
rep(1:nrow(df), df$n)
df[rep(1:nrow(df), df$n), ]

## removing columns from data frames (character subsetting)
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df$z <- NULL

df[["z"]] <- letters[1:3]
df$z <- NULL
df["z"] <- list(letters[1:3])
df[["z"]] <- NULL

df[["z"]] <- letters[1:3]
df[setdiff(names(df), "z")]

## selecting rows based on a condition (logical subsetting)
mtcars[mtcars$gear == 5, ]
subset(mtcars, gear == 5)
# don't use short circuit operators (&& or ||)
subset(mtcars, gear == 5 & cyl == 4)

## boolean algebra vs sets (logical & integer subsetting)
# integer subsetting is more effective when
# 1. to find the first (or last) TRUE
# 2. very few TRUEs <- faster and less storage required

set.seed(10)
x <- sample(10) < 4

# boolean to integer
which(x)
unwhich <- function(x, n) {
  out <- rep_len(FALSE, n)
  out[x] <- TRUE
  out
}

unwhich(which(x), 10)

x1 <- 1:10 %% 2 == 0
x2 <- which(x1)

y1 <- 1:10 %% 5 == 0
y2 <- which(y1)

x1 & y1
intersect(x2, y2)

x1 | y1
union(x2, y2)

x1 & !y1
setdiff(x2, y2)

xor(x1, y1)
setdiff(union(x2, y2), intersect(x2, y2))

#How would you randomly permute the columns of a data frame? 
#(This is an important technique in random forests.) 
#Can you simultaneously permute the rows and columns in one step?
set.seed(10)
var0 <- sample(1:10, 10)
var1 <- sample(10:1, 10)
var2 <- sample(20:11, 10)
var3 <- sample(30:21, 10)
var4 <- sample(40:31, 10)
df <- data.frame(var0 = var0, var1 = var1, var2 = var2, var3 = var3, var4 = var4)

df[sample(1:nrow(df), as.integer(nrow(df)/2)), sample(names(df), 3)]

#How would you select a random sample of m rows from a data frame? 
#What if the sample had to be contiguous 
#(i.e., with an initial row, a final row, and every row in between)?
rows <- c(rownames(df)[1], sample(rownames(df), 5), rownames(df)[length(rownames(df))])
df[rows, ]

#How could you put the columns in a data frame in alphabetical order?
df <- as.data.frame(outer(1:2, 1:10))
df[order(names(df))]