library(lubridate)
library(stringr)
library(reshape2)
library(plyr)
library(dplyr)

# create data folder
dataDir <- paste0("data","_",format(Sys.Date(),"%Y-%m-%d"))
if(file.exists(dataDir)) { 
      unlink(dataDir, recursive = TRUE)
      dir.create(dataDir)
} else {
      dir.create(dataDir)
}

# assumes codes are known beforehand
codes <- c("MSFT", "TCHC")
urls <- paste0("http://www.google.com/finance/historical?q=NASDAQ:",
               codes,"&output=csv")
paths <- paste0(dataDir,"\\",codes,".csv") # forward slash on non-windows (/)

# simple error handling in case file doesn't exists
downloadFile <- function(url, path, ...) {
      # remove file if exists already
      if(file.exists(path)) file.remove(path)
      # download file
      tryCatch(            
            download.file(url, path, ...), error = function(c) {
                  # remove file if error
                  if(file.exists(path)) file.remove(path)
                  # create error message
                  c$message <- paste(substr(path, 1, 4),"failed")
                  message(c$message)
            }
      )
}
# wrapper of mapply
Map(downloadFile, urls, paths)

# read all csv files and merge
files <- dir(dataDir, full.name = TRUE)
dataList <- llply(files, function(file){
      # get code from file path
      pattern <- "/[A-Z][A-Z][A-Z][A-Z]"
      code <- substr(str_extract(file, pattern), 2, nchar(str_extract(file, pattern)))      
      tryCatch({
            data <- read.csv(file, stringsAsFactors = FALSE)
            # first column's name is funny
            names(data) <- c("Date","Open","High","Low","Close","Volume")
            data$Date <- dmy(data$Date)
            data$Close <- as.numeric(data$Close)
            data$Code <- code
            # optional
            data$Open <- as.numeric(data$Open)
            data$High <- as.numeric(data$High)
            data$Low <- as.numeric(data$Low)            
            data$Volume <- as.integer(data$Volume)            
            # select only 'Date', 'Close' and 'Code'
            # raw data should be arranged in an ascending order
            arrange(subset(data, select = c(Date, Close, Code)), Date)
      },
      error = function(c){
            c$message <- paste(code,"failed")
            message(c$message)
            # return a dummy data frame not to break function
            data <- data.frame(Date=dmy(format(Sys.Date(),"%d%m%Y")), Close=0, Code="NA")
            data
      })
}, .progress = "text")

# data is combined to create a long format
# dummy data frame values are filtered out
data <- filter(rbind_all(dataList), Code != "NA")

# data is converted into a wide format
data <- dcast(data, Date ~ Code, value.var="Close")

# Date column is removed
data <- select(data, -Date)

# apply log difference column wise
dailyRet <- apply(log(data), 2, diff, lag=1)

# obtain daily return, variance and correlation
returns <- apply(dailyRet, 2, sum, na.rm = TRUE)
variance <- apply(dailyRet, 2, var, na.rm = TRUE)
correlation <- cor(dailyRet)

returns
variance
correlation

# remove data folder
if(file.exists(dataDir)) { unlink(dataDir, recursive = TRUE) }