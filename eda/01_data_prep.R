library(data.table)
library(lubridate)

dt <- fread("../raw_data/player_season_data.csv")
homegrowns <- fread("../raw_data/homegrowns.csv")
hglist <- homegrowns$mls_id
draftees <- fread("../raw_data/draftees.csv")
mlsusl <- fread("../raw_data/mls_usl_team.csv")
## drop goalkeeper
dt <- dt[position != 'Goalkeeper', ]

## determine age in years as of 3/1 of the season
dt[, season_start := paste0("03/01/",year)]
calc_age <- function(birthday, enddate){
        period <- as.period(interval(mdy(birthday), mdy(enddate)), unit = "year")
            return(period$year)
}
dt[, age := calc_age(birthday, season_start)]

## roll up to player-season level - take the team the player played more games with
dt.season <- dt[, .(gp = sum(gp), gs = sum(gs), mins = sum(mins)), by = c('mls_id', 'year', 'age', 'position')]
majority.team <- dt[, .SD[which.max(gp), club], by = c("mls_id", "year")]
player.season <- merge(dt.season, majority.team, by = c('mls_id', 'year'))

## get quantile numbers by season to define starter
lg.ave <- player.season[, .(mean = mean(mins), qtile = quantile(mins, .65)), by=year]
player.season <- merge(player.season, lg.ave, by='year')
player.season[, starter := mins >= qtile]

## get the first season and starting age for each player that joined 2007 or later
first.season <- player.season[, .(first.yr = min(year), entry.age = min(age)), by = mls_id][first.yr >= 2007, ]
player.season <- merge(player.season, first.season, by = 'mls_id')

## drop players who weren't prospects when they entered
prospect.season <- player.season[entry.age <= 21, ]
first.starter.season <- prospect.season[starter == TRUE, .(starter.yr = min(year)), by = 'mls_id']
last.season <- prospect.season[, .(last.yr = max(year)), by = 'mls_id']
prospect.season <- merge(prospect.season, first.starter.season, by = 'mls_id', all.x = TRUE)
prospect.season <- merge(prospect.season, last.season, by = 'mls_id', all.x = TRUE)


## do whatever other data prep - add homegrowns, draft picks, think
## some further data cleaning and bolstering
prospect.season[position == "Midfielder/Forward", position := "Midfielder"]
prospect.season[position == "Defender/Midfielder", position := "Defender"]
prospect.season[position == "Midfielder/Defender", position := "Midfielder"]
prospect.season[position == "Forward/Midfielder", position := "Forward"]
prospect.season[V1 == "Kansas City Wizards", V1 := "Sporting Kansas City"]
prospect.season[V1 == "NY Red Bulls", V1 := "New York Red Bulls"]
prospect.season[V1 == "Los Angeles Galaxy", V1 := "LA Galaxy"]
prospect.season[V1 == "Columbus Crew", V1 := "Columbus Crew SC"]
## add homegrown and draft flags
prospect.season[mls_id %in% hglist, hgflag := 1]
prospect.season[mls_id %in% draftees$mls_id, draftflag := 1]
prospect.season[is.na(hgflag), hgflag := 0]
prospect.season[is.na(draftflag), draftflag := 0]
prospect.season <- merge(prospect.season, draftees, by = 'mls_id', all.x = TRUE)
## prune off the stuff from draftees
prospect.season$team <- NULL
prospect.season$season <- NULL
prospect.season[is.na(pick), pick := -1]
## add flag for if a USL 2nd team is available
prospect.season <- merge(prospect.season, mlsusl, by.x = 'V1', by.y = 'club', all.x = TRUE)
prospect.season[first.yr >= start.yr, mls2flag := 1]
prospect.season[is.na(mls2flag), mls2flag := 0]

## write person-level dataset
person.level <- prospect.season[year == first.yr, ]
## calculate duration in the league
person.level[is.na(starter.yr), duration := last.yr - first.yr + 1]
person.level[is.na(starter.yr), event := 0]
person.level[is.na(duration), event := 1]
person.level[is.na(duration), duration := starter.yr - first.yr + 1]
keep.cols <- c("mls_id", "position", "V1", "mls2flag", "hgflag", "draftflag", "pick", "first.yr", "entry.age", "duration", "event")
person.level <- person.level[, keep.cols, with = FALSE]
colnames(person.level)[3] <- "club"
fwrite(person.level, "../person_level.csv")

## write person-period dataset
prospect.season$start.yr <- NULL
person.period <- prospect.season[starter.yr >= year | is.na(starter.yr), ]
person.period[, duration := year - first.yr + 1]
person.period[starter.yr == year, event := 1]
person.period[is.na(event), event := 0]
person.period <- person.period[, keep.cols, with = FALSE][order(mls_id)]
colnames(person.period)[3] <- "club"
fwrite(person.period, "../person_period.csv")
## how to handle players switching teams here?
