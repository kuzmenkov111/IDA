## Stanford Seminar - Expressing yourself in R
# https://www.youtube.com/watch?v=wki0BqlztCo

# download log file
# https://github.com/hadley/cran-logs-dplyr
library(plyr)
library(data.table)
library(dplyr)

# download logs
message("Download logs")
start <- as.Date('2014-01-01')
yesterday <-  Sys.Date() - 1
days <- seq(start, yesterday, by = "day")
years <- as.POSIXlt(days)$year + 1900
urls <- paste0("http://cran-logs.rstudio.com/", years, "/", days, ".csv.gz")
paths <- paste0("logs/", days, ".csv.gz")

if(!file.exists("logs")) dir.create("logs")
missing <- !(paths %in% dir("logs", full.name = TRUE))

downloadFile <- function(url, destfile, ...) {
      # remove file
      if(file.exists(destfile)) file.remove(destfile)
      # download file
      tryCatch(
            download.file(url, destfile, ...), error = function(c) {
                  # remove file
                  if(file.exists(destfile)) file.remove(destfile)                  
                  # create error message
                  matchReg <- gregexpr("\\d\\d\\d\\d-\\d\\d-\\d\\d", destfile, perl = TRUE)
                  matchStr <- sapply(regmatches(destfile, matchReg), function(elt) elt[1])
                  c$message <- paste0("Log file on ",matchStr," fails to be downloaded.")
                  message(c$message)
            }            
      )
}
Map(downloadFile, urls[missing], paths[missing])

# check missing dates
missingFiles <- paths[!(paths %in% dir("logs", full.name = TRUE))]
missingDates <- function(missingFiles) {
      matchReg <- gregexpr("\\d\\d\\d\\d-\\d\\d-\\d\\d", missingFiles, perl = TRUE)
      matchStr <- sapply(regmatches(missingFiles, matchReg), function(elt) elt[1])
      as.Date(matchStr)
}
missingDates(missingFiles)

# read and merge csv files
message("Parsing logs")
logs <- dir("logs", full.name = TRUE)
all_pkgs <- llply(logs, function(file){
      #data <- read.csv(file, stringsAsFactors = FALSE)
      data <- fread(file, stringsAsFactors = FALSE)
      data$r_version <- as.character(data$r_version)
      data$r_arch <- as.character(data$r_arch)
      data$r_os <- as.character(data$r_os)
      data$date <- as.Date(data$date)
      data
}, .progress = "text")

all <- rbind_all(all_pkgs)
class(all) <- c("tbl_df", "tbl", "data.frame")

saveRDS(all, file = "logs.rds")
write.csv(all, file = "logs.csv", row.names = FALSE)

## dplyr
# 5 key single table verbs
      # select, filter, mutate, summarise, arrange
# + group by
logs <- readRDS("logs.rds")
packages <- group_by(logs, package)
counts <- summarise(packages, n = n())
head(arrange(counts, desc(n)), 20)

logs %>%
      group_by(package) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      head(20)

saveRDS(filter(logs, date >= as.Date("2014-09-01")), file="logs_short.rds")

# multi-table verbs
# left join: all x + matching y
# inner join: matching x + y
# semi join: all x with match in y
# anti join: all x without match in y

#hflights
hflights <- hflights_postgres("hflights")
hflights <- hflights_postgres() %>% tbl("hflights")
ranked <- hflights %>%
      group_by(TailNum) %>%
      mutate(Rank = rank(desc(ArrDelay))) %>%
      select(TailNum, ArrDelay, Rank) # lazy evaluation
ranked$query # show SQL query
explain(ranked)
ranked

# SELECT
#     *,
#     RANK() OVER (PARTITION BY "TailNum"
#           ORDER BY "ArrDelay" DESC) AS "rank"
# FROM "hflights"

worst <- hflights %>%
      group_by(TailNum) %>%
      filter(ArrDelay == max(ArrDelay)) %>%
      select(TailNum, Dest, Origin, ArrDelay)
worst$query
explain(worst)

## ggvis
downloads <- logs %>%
      group_by(date) %>%
      summarise(n = n(), n_ip = n_distinct(ip_id))
saveRDS(downloads, "downloads.rds")

downloads <- readRDS("downloads.rds")
qvis(downloads, ~date, ~n)
qvis(downloads, ~date, ~n, layers = "line")
qvis(downlaods, ~date, ~n_ip, layers = "line")
qvis(downloads, ~date, ~n / n_ip, layers = "line")


















