library(plyr)
library(data.table)

boxScore <- fread("boxscore.csv")
boxScore$date <- as.Date(boxScore$date)
str(boxScore)
setkey(boxScore, date, team, player)

teamCount <- fread("team_game_count.csv")
teamCount$date <- as.Date(teamCount$date)
setkey(teamCount, date, team)

# summary by groups
ddply(as.data.frame(boxScore), c("date","team"), summarise,
      points = sum(points), fgm = sum(fgm), fga = sum(fga))

boxScore[, list(points = sum(points), fgm = sum(fgm), fga = sum(fga)), by = list(date, team)]

boxScore %>%
      group_by(date, team) %>%
      summarise(points = sum(points), fgm = sum(fgm), fga = sum(fga))

# merge examples
filterBase <- teamCount[teamCount$team=="HOU" & teamCount$game_num==82 & teamCount$season=="2012-2013"]
merge(as.data.frame(boxScore), as.data.frame(filterBase), by=c("date","team"))

merge(boxScore, filterBase, by=c("date","team"))

filterDplyr <- teamCount %>% filter(team=="HOU", game_num==82, season=="2012-2013")
inner_join(boxScore, filterDplyr, by=c("date","team"))

boxScore[filterBase]

inner_join(boxScore, filter(teamCount, team=="HOU", game_num==82, season=="2012-2013"), by=c("date","team"))







