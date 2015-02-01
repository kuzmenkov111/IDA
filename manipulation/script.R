#### Summary of data manipulation
### looping functions
# source: https://www.coursera.org/course/rprog

## lapply(X, FUN, ...) - apply a function to a list
## sapply(X, FUN, ..., simplyfy = TRUE, USE.NAMES = TRUE)
# apply mean to x$a and x$b
x <- list(a=1:5, b=rnorm(10))
lapply(x, mean)
sapply(x, mean)

# generate 4 uniform random vectors, numbers of elements 1 to 4
lapply(1:4, runif, min=0, max=10)

# take first columns of x$a and x$b
x <- list(a=matrix(1:6,3,2), b=matrix(1:8,4,2))
lapply(x, function(elt) elt[,1])
sapply(x, function(elt) elt[,1]) # same as above as different lengths

## apply(X, MARGIN, FUN, ...) - apply a function to a margin of an array
x <- matrix(rnorm(200),20,10)
apply(x, 2, mean) # to column
apply(x, 1, mean) # to row
apply(x, 1, quantile, probs=c(0.25,0.75))

# better to use built-in functions
colSums(x)
colMeans(x)
rowSums(x)
rowMeans(x)

## tapply(X, INDEX, FUN = NULL, ..., simplify = TRUE) - apply a function over a subset of vector
x <- c(rnorm(10), runif(10), rnorm(10,1))
f <- gl(3,10)

tapply(x, f, mean)
sapply(split(x, f), mean) # split(x, f, drop = FALSE, ...) - create a list by f

# l(s)apply with split seems to cover tapply
s <- split(airquality, airquality$Month)
sapply(s, function(x) colMeans(x[,c("Ozone","Solar.R","Wind")], na.rm=TRUE))

## mapply(FUN, ..., MoreArgs = NULL, Simplify = TRUE, USE.NAMES = TRUE) - multivariate version of sapply
mapply(rep, 1:4, 4:1)

rnorm(5, 0, 1)
mapply(rnorm, 1:5, 1:5, 2)
mapply(rnorm, rep(10,2), 0:1, 1)
cbind(sapply(10, rnorm, 0, 1), sapply(10, rnorm, 1, 1))

# lapply, sapply (with split), apply and mapply except for tapply

## reshape2 and plyr
# reshape
# http://seananderson.ca/2013/10/19/reshape.html
# http://reasoniamhere.com/2013/09/26/switching-between-long-and-wide-formats-in-r/
# http://www.hselab.org/content/getting-started-r-plyr-and-ggplot2-group-analysis

# https://github.com/seananderson/datawranglR
# reshape2
# melt: wide to long, case: long to wide
library(reshape2)
airQlty <- airquality
head(airQlty)

# melt assumes all columns with numeric values are variables with values
airQltyL <- melt(airQlty)
head(airQltyL)

# id variables can be set (column lables of Excel pivot table or group by in SQL)
airQltyL <- melt(airQlty, id.vars = c("Month","Day"))
head(airQltyL)

# change column names
airQltyL <- melt(airQlty, id.vars = c("Month","Day"), 
                 variable.name = "ClimateVar", value.name = "ClimateVal")
head(airQltyL)

airQltyW <- dcast(airQltyL, Month + Day ~ ClimateVar, value.var = "ClimateVal")
head(airQltyW)

dcast(airQltyL, Month ~ ClimateVar, value.var = "ClimateVal",
      fun.aggregate = mean, na.rm = TRUE) # if multiple values per row, aggregate

# plyr
# check aggregate, by, replicate
library(plyr)

airQlty <- airquality[,c("Month","Day","Wind")]
airQlty$Dt <- airQlty$Day > 15
head(airQlty)

ddply(airQlty, "Month",
      function(x) {
            mean <- mean(x$Wind, na.rm = TRUE)
            sd <- sd(x$Wind, na.rm = TRUE)
            cv <- sd / mean
            data.frame(WindCV = cv)
      }) # ddply(data, by, function)

ddply(airQlty, "Month", summarise, WindCV = sd(Wind, na.rm = TRUE)/mean(Wind, na.rm = TRUE))
ddply(airQlty, c("Month","Dt"), summarise, WindCV = sd(Wind, na.rm = TRUE)/mean(Wind, na.rm = TRUE))

head(ddply(airQlty, "Month", transform, WinSum = sum(Wind)))
head(ddply(airQlty, c("Month","Dt"), transform, WinSum = sum(Wind)))

head(ddply(airQlty, "Month", mutate, Mean = mean(Wind, na.rm = TRUE), Sd = sd(Wind, na.rm = TRUE), CV = Sd/Mean))

bb <- baseball[,"year" > 2000]
head(ddply(bb, c("year","team"), summarise, hrs = sum(hr)))

# error handling tip
f <- function(x) if(x == 1) stop("Error!") else 1
safe.f <- failwith(NA, f, quiet = TRUE)
llply(1:2, f)
llply(1:2, safe.f)

# parallel processing
x <- c(1:10)
wait <- function(i) Sys.sleep(0.1)
system.time(llply(x, wait))
system.time(sapply(x, wait))

# check later
library(doMC)
registerDoMc(2)
system.time(llply(x, wait, .parallel = TRUE))

# 3 ways to consider faster manipulation
# apply function
system.time(ddply(baseball, "id", summarize, length(year)))
system.time(tapply(baseball$year, baseball$id, length))
system.time(sapply(split(baseball$year, baseball$id), length))

# idata.frame
system.time(ddply(idata.frame(baseball), "id", summarize, length(year)))

# data.table
library(data.table)
dt <- data.table(baseball, key = "id")
system.time(dt[, length(year), by = list(id)])

## tidyr and dplyr
# http://scicomp2014.edc.uri.edu/posts/2014-04-14-Smith.html
# http://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html

# dplyr
# Source: http://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
library(dplyr)
library(ggplot2)
library(nycflights13)

?flights
dim(flights)

flights <- tbl_df(flights) # for on screen

# single table verbs - *filter, *select, slice, *arrange, rename, distinct, *mutate, transform, transmute, *summarise, sample_*

# filter - subsetting by condition
filter(flights, month == 1, day == 1)
# same as flights[flights$month == 1 & flights$day == 1,]
filter(flights, month == 1 | month == 2)

# slice - subsetting by row positions
slice(flights, 1:10)

# arrange - reorder data by condition
arrange(flights, year, month, day, dep_delay)
# same as flights[order(flights$year, flights$month, flights$day),]
arrange(flights, desc(arr_delay))
# same as flights[order(desc(flights$arr_delay)),]

# select
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))

select(flights, starts_with("arr"))
select(flights, ends_with("time"))
select(flights, matches("p_")) # check if accepts regular expressions
select(flights, contains("_"))

select(flights, tail_num = tailnum) # column name not changed

# rename
rename(flights, tail_num = tailnum) # column name changed

# distinct
distinct(select(flights, tailnum))
distinct(select(flights, origin, dest))

# mutate, transform, transmute - add new columns
mutate(flights, gain = arr_delay - dep_delay, speed = distance / air_time * 60)
# internal variable can be used in mutate but not in transform
mutate(flights, gain = arr_delay - dep_delay, gain_per_hour = gain / (air_time / 60))
# error: transform(flights, gain = arr_delay - dep_delay, gain_per_hour = gain / (air_time / 60))
# keep only new variables
transmute(flights, gain = arr_delay - dep_delay, gain_per_hour = gain / (air_time / 60))

# summarise
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

# sample_n and sample_frac
sample_n(flights, 10)
sample_frac(flights, 0.1)

sample_n(flights, 10, replace = TRUE)
sample_n(flights, 10, weight = 1 / month) # check weight

# grouped operations
planes <- group_by(flights, tailnum)
delay <- summarise(planes,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dist < 2000)
# aggregate functions
# from base - min(), max(), mean(), sum(), sd(), median(), IQR()
# from dplyr - n(), n_distinct(x), fist(x), last(x), nth(x, n)

ggplot(delay, aes(dist, delay)) +
      geom_point(aes(size = count), alpha = 1/2) +
      geom_smooth() +
      scale_size_area()

destinations <- group_by(flights, dest)
summarise(destinations, planes = n_distinct(tailnum), flights = n())

# rolling up data set - but careful
daily <- group_by(flights, year, month, day)
(per_day <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year <- summarise(per_month, flights = sum(flights)))

# chaining
# inefficient
a1 <- group_by(flights, year, month, day)
a2 <- select(a1, arr_delay, dep_delay)
a3 <- summarise(a2,
                arr = mean(arr_delay, na.rm = TRUE),
                dep = mean(arr_delay, na.rm = TRUE))
a4 <- filter(a3, arr > 30 | dep > 30)

# comfusing
filter(
      summarise(
            select(
                  group_by(flights, year, month, day)
                  ,arr_delay, dep_delay
            ),
            arr = mean(arr_delay, na.rm = TRUE),
            dep = mean(dep_delay, na.rm = TRUE)
      ),
      arr > 30 | dep > 30
)

# pipelining
flights %>%
      group_by(year, month, day) %>%
      select(arr_delay, dep_delay) %>%
      summarise(arr = mean(arr_delay, na.rm = TRUE)
                ,dep = mean(dep_delay, na.rm = TRUE)) %>%
      filter(arr > 30 | dep > 30)

## data.table
#http://blog.yhathq.com/posts/fast-summary-statistics-with-data-dot-table.html
# example(data.table)

## magrittr
# http://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html
library(magrittr)
weekly <-
      airquality %>% 
      transform(Date = paste(1973, Month, Day, sep = "-") %>% as.Date) %>% 
      aggregate(. ~ Date %>% format("%W"), ., mean)

trans1 <- transform(airquality, Date = as.Date(paste(1973, Month, Day, sep="-")))
weekly1 <- aggregate(. ~ Date, data = trans1, mean)

## ggplot2

## knitr

## jvmr