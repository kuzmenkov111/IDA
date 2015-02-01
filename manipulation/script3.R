## data.table
# http://blog.yhathq.com/posts/fast-summary-statistics-with-data-dot-table.html
library(data.table)
library(plyr)
library(dplyr)

# check if required data sets exist
file <- c("boxscore.csv", "team_game_count.csv")
msg <- function(file) {
      ifelse(!file.exists(file),paste(file,"doesn't exists"), paste(file,"exists"))
}
sapply(file, msg)

boxScoreDF <- read.csv("boxscore.csv")
boxScoreDF$date <- as.Date(boxScoreDF$date)
boxScoreDF <- tbl_df(boxScoreDF)

boxScoreDT <- fread("boxscore.csv")
setkey(boxScoreDT, date, team)

gamesDF <- read.csv("team_game_count.csv")
gamesDF$date <- as.Date(gamesDF$date)
gamesDF <- tbl_df(gamesDF)

system.time(ddply(boxScoreDF, ~ date + team, summarise,
      points = sum(points), fgm = sum(fgm), fga = sum(fga)))

system.time(boxScoreDT[, list(points = sum(points),
                              fgm = sum(fgm),
                              fga = sum(fga)),
                        by = list(date, team)])

system.time(boxScoreDT %>%
      group_by(date, team) %>%
      summarise(points = sum(points), fgm = sum(fgm), fga = sum(fga)))











