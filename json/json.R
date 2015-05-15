# a R json parser
# install.packages("jsonlite")
# curl export to R
# install.packages("curl")

# http
library(jsonlite)
zips <- stream_in(url("http://media.mongodb.org/zips.json"))

# https
# library(jsonlite)
# library(curl)
# zips <- stream_in(curl("https://media.mongodb.org/zips.json"))

# process to export
df <- zips
# loc object is JSON array and separated to export into CSV
df$loc1 <- do.call(rbind, lapply(df$loc, unlist))[,1]
df$loc2 <- do.call(rbind, lapply(df$loc, unlist))[,2]
# remove loc list column
df <- df[, -3]

# writing tab delimited file - ?write.table for details
write.table(df, file = "zips.csv", sep = ",")

data1 <- fromJSON("https://api.github.com/users/hadley/orgs")
data2 <- fromJSON("https://api.github.com/users/hadley/repos")

