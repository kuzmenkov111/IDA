#### Getting your data into shape
library(reshape2)
library(plyr)
library(dplyr)

### creating, adding, deleting, renaming and reordering columns
## creating
df <- data.frame(g=c("A","B","C"), x=1:3)

lst <- list(g = c("A","B","C"), x = 1:3)
df <- as.data.frame(lst)

## adding
# df$less <- 1:2 error - less elements
# df$more <- 1:4 error - more elements
df$nc <- 12:14
df

## deleting
df$nc <- NULL
df

subset(df, select = c(g))
subset(df, select = c(-x))

## renaming
names(df) <- c("group","value")
df

names(df)[names(df) == "group"] <- c("g") # rename by name
names(df)[2] <- "x" # rename by index
names(df)

## reordering
df[c(2, 1)] # list style by index (data frame is a list of vectors/factors)
df[, c(2, 1)] # matrix style by index
df[c("g", "x")] # by name

df[2] # data frame
df[,2, drop = FALSE] # data frame

df[[2]] # vector
df[,2] # vector

### subsetting data frames
set.seed(12345)
df <- data.frame(letter = sample(letters[1:5], size=200, replace=TRUE),
                 num = sample(1:20, size=200, replace=TRUE),
                 rnd = rnorm(200))

subset(df, subset = letter %in% c("a","b","d"), select = c(letter, rnd))
subset(df, subset = letter %in% c("a","b","d") & rnd > 1, select = c(letter, rnd))

df %>%
      filter(letter %in% c("a","b","d") & rnd > 1) %>%
      select(letter, rnd)

# less elegant ways
df[df$letter %in% c("a","b","d") & df$rnd > 1, c("letter", "rnd"), drop=FALSE]
df[1:3, c(1,3), drop=FALSE]

### dealing with factors and character vectors
## changing the order of factor levels
ltt <- factor(c("a","b","c","d","e"), levels = c("e","d","c","b","a"))
levels(ltt)

levels(ltt) <- rev(levels(ltt))
levels(ltt)

## changing the order of factor levels by other values
df$letter <- reorder(df$letter, df$rnd, FUN = mean)
levels(df$letter)

## renaming factor levels
fac <- df$letter
levels(fac)
fac <- revalue(fac, replace=c(a="A",b="B","c"="C",d="D","e"="E")) # renamed individually, quote can be used
levels(fac)
fac <- mapvalues(fac, from=levels(fac), to=tolower(levels(fac))) # mapped by vectors
levels(fac)

# comparison with traditional ways
levels(fac)[levels(fac)=="a"] <- "A"
levels(fac)
fac <- revalue(fac, replace=c(A="a"))
levels(fac)

levels(fac) <- list(A="a",B="b",C="c",D="d",E="e")
levels(fac)
fac <- mapvalues(fac, from=levels(fac), to=tolower(levels(fac)))
levels(fac)

# better not to revalue a factor by index

## removing unused levels
levels(fac)
levels(droplevels(fac[1:5])) # check except argument

## renaming charac vector
cha <- as.character(df$letter)
cha <- revalue(cha, replace=c(a="A",b="B","c"="C",d="D","e"="E"))
unique(cha)
cha <- mapvalues(cha, from=unique(cha), to=tolower(unique(cha)))
unique(cha)

# comparison with traditional ways
cha[cha=="a"] = "A"
unique(cha)
cha <- revalue(cha, replace=c(A="a"))
unique(cha)

### recoding variables
## categorical into another
set.seed(12345)
cat <- data.frame(letter = factor(sample(letters[1:4], 100, replace = TRUE),
                                  levels=c("a","b","c","d")),
                  rnd = rnorm(100, sd=10))

oldVals <- levels(cat$letter) # c("a","b","c","d")
newVals <- c("Yes","Yes","No","Unknown")

cat$cat <- as.factor(newVals[match(cat$letter, oldVals)])
# match(cat$letter, oldVals) returns cat$letter's position of oldVals

# traditional way
# not easy to handle complicated conditions with match?
cat$cat <- NULL
cat$cat[cat$letter=="a" | cat$letter=="b"] <- "Yes"
cat$cat[cat$letter=="c"] <- "No"
cat$cat[cat$letter=="d"] <- "Unknown"
cat$cat <- as.factor(cat$cat)
levels(cat$cat) <- c("Yes","No","Unknown")
levels(cat$cat)

# columns can be concatenated with a dot (.)
cat$inter <- interaction(cat$cat, cat$letter)

## continuous into categorical
rndCat <- cut(cat$rnd, breaks=quantile(cat$rnd), 
              labels = c("25%", "50%", "75%", "100%"),
              include.lowest = TRUE) # open on the left by default
cat$rndCat <- rndCat
levels(rndCat)

### transforming variables
air <- airquality

## adding or modifying variables
transform(air, Ozone = -Ozone)[1:2,]
mutate(air, Ozone = -Ozone)[1:2,]

transform(air, New = -Ozone, Temp = (Temp-32)/1.8)[1:2,]
mutate(air, New = -Ozone, Temp = (Temp-32)/1.8)[1:2,]

# mutate can use newly created variables to obtain new variables
# error: head(transform(air, Tmp = (Temp-32)/1.8, Oz = Ozone/Tmp))
head(mutate(air, Tmp = (Temp-32)/1.8, Oz = Ozone/Tmp))

# transform calculates values simutaneously - Oz = 41/67 = 0.6119403
# mutate calculates values sequentially - Oz = 41/19.4444 = 2.108571
air[1,c("Temp","Ozone")] # c(67, 41)
transform(air, Temp = (Temp-32)/1.8, Oz = Ozone/Temp)[1,c("Temp","Ozone","Oz")]
# c(19.44444, 41, *0.6119403)
mutate(air, Temp = (Temp-32)/1.8, Oz = Ozone/Temp)[1,c("Temp","Ozone","Oz")]
# c(19.44444, 41, *2.108571)

## transforming by group
air <- mutate(air, Tmp = floor(Temp/10))
air$Month <- as.factor(air$Month)
air$Tmp <- as.factor(air$Tmp)

# Temp - overall mean Temp
mutate(air, TempDev = Temp - mean(Temp, na.rm=TRUE))[1:4,]
# Temp - mean Temp by Month
ddply(air, .(Month), mutate, TempDev = Temp - mean(Temp, na.rm=TRUE))[1:4,]

air %>%
      group_by(Month) %>%
      mutate(TempDev = Temp - mean(Temp, na.rm=TRUE)) %>%
      slice(1:4)

# Temp - mean Temp by Month and Tmp, data sorted
ddply(air, .(Month, Tmp), mutate, TempDev = Temp - mean(Temp, na.rm=TRUE))[1:4,]

### summarise data
## by group
summarise(air, mean = mean(Temp, na.rm=TRUE), sd = sd(Temp, na.rm=TRUE))
ddply(air, .(Month), .drop=FALSE, # missing combination kept
      summarise, obj = length(Temp), mean = mean(Temp, na.rm=TRUE), sd = sd(Temp, na.rm=TRUE))
ddply(air, .(Month, Tmp), .drop=FALSE,
      summarise, obj = length(Temp), mean = mean(Temp, na.rm=TRUE), sd = sd(Temp, na.rm=TRUE))

## by standard error and confidence interval
ddply(air, .(Month), .drop=FALSE, summarise,
      obj = sum(!is.na(Temp)), mean = mean(Temp, na.rm=TRUE),
      sd = sd(Temp, na.rm=TRUE), se = sd/sqrt(obj), ci = se * qt(.975, obj-1),
      lbound = mean - ci, ubound = mean + ci)

### convert data formats
air <- airquality
tempCat <- cut(air$Temp, breaks=quantile(air$Temp),
               labels = c(25, 50, 75, 100),
               include.lowest = TRUE)
air$TempCat <- as.factor(tempCat)
air <- tbl_df(air)

## convert to long format
# id.vars - character/factor vectors, if not specified, all such columns are taken
# measure.vars - numeric vectors, if not specified, all such columns are taken
# 5 numeric or int (Ozone, Solar.R, Wind, Temp, Day) and 2 factors (Month, TempCat)
longAir <- melt(air,
                id.vars = c("Month","TempCat"), # can be chosen
                measure.vars = c("Ozone","Solar.R","Wind","Temp","Day"), # can be chosen
                variable.name = "VarName",
                value.name = "ValName",
                na.rm = FALSE, # default
                factorsAsStrings = TRUE) # default
longAir <- tbl_df(longAir) # 765 (153 * 5) records

# select 'Month' for id variable and 'Temp', 'Ozone' for measure variables
longAir <- melt(air,
                id.vars = c("Month"),
                measure.vars = c("Temp","Ozone"),
                variable.name = "VarName",
                value.name = "ValName",
                na.rm = FALSE, # default
                FactorsAsStrings = TRUE) # default
longAir <- tbl_df(longAir) # 306 (153 * 2) records

## convert to wide format
# formula: x_variable + x_2 ~ y_variable + y_2 ~ z_variable ~
# x - row, y - column, ...
wideAir <- dcast(longAir,
                 Month ~ VarName,
                 fun.aggregate = function(x) { mean(x, na.rm=TRUE) },
                 drop = FALSE, # default: TRUE
                 value.var = "ValName")
wideAir <- tbl_df(wideAir)

# only if each row is determined by id variables,
# a data frame can be recovered by melt + dcast
air <- airquality # 153
air <- tbl_df(air)

longAir <- melt(air,
                id.vars = c("Month","Day"),
                measure.vars = c("Ozone","Solar.R","Wind","Temp"),
                variable.name = "VarName",
                value.name = "ValName") # 612 (153 * 4)
longAir <- tbl_df(longAir)

wideAir <- dcast(longAir,
                 Month + Day ~ VarName,
                 drop = TRUE, # default
                 value.var = "ValName") # 153
wideAir <- tbl_df(wideAir)

wideAir <- dcast(longAir,
                 Month + Day ~ VarName,
                 drop = FALSE,
                 value.var = "ValName") # 155
wideAir <- tbl_df(wideAir)

# day values from 1 to 31 despite 30 days in Jun and Sep
# Jun (Sep) + 31 casted producing all NA
# drop = TRUE, above cases dropped <- 153
# drop = FALSE, they are kept <- 155