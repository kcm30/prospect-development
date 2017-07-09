library(data.table)
library(ggplot2)
library(lubridate)

dt <- fread("../player_season_data.csv")
# drop goalkeeper
dt <- dt[position != 'Goalkeeper', ]
# determine age in years as of 3/1 of the season
dt[, season_start := paste0("03/01/",year)]

calc_age <- function(birthday, enddate){
    period <- as.period(interval(mdy(birthday), mdy(enddate)), unit = "year")
    return(period$year)
}
dt[, age := calc_age(birthday, season_start)]

# roll up stats to season level
dt.season <- dt[, .(gp = sum(gp), gs = sum(gs), mins = sum(mins)), by = c('mls_id', 'year', 'age')]
prospect.by.year <- dt.season[year >= 2007 & age <= 21, .N, by = year][order(year)]

## get prospects per team by year, and plot
prospect.by.year$teams <- c(13, 14, 15, 16, 18, 19, 19, 19, 20, 20)
prospect.by.year[, per := N/teams]
p <- ggplot(prospect.by.year, aes(x=year, y=per)) +
     geom_line(color = 'navy', lwd = 2) +
     ylim(0, 10) +
     xlab("Season") +
     ylab('"Prospects" per Team') +
     ggtitle("Roster Spots Devoted to Prospects, by Year") +
     theme(text = element_text(size=18))

## get # of prospects in top x% of minutes
lg.ave <- dt.season[year >= 2007, .(mean = mean(mins), qtile = quantile(mins, .5)), by=year]
season.ave <- merge(dt.season[year >= 2007 & age <= 21, ], lg.ave, by='year')
prospect.qtile <- season.ave[, .(above.mean = mean(mins > mean), qtile = mean(mins > qtile)), by = year]
q <- ggplot(prospect.qtile, aes(x = year, y = above.mean)) +
     geom_line(color = 'navy', lwd = 2) +
     ylim(0, .4) +
     xlab("Season") +
     ylab("% of 'Prospects' Above the Lg. Ave in Minutes") +
     ggtitle("Quality Prospects by Season") +
     theme(text = element_text(size=18))


## get minutes by prospect
prospect.min <- dt.season[year >= 2007 & age <= 21, .(mins = sum(mins), .N, med = mean(mins), st = sum(mins > 1500)/.N), by = year][order(year)]
prospect.min$total.games <- c(195, 210, 225, 240, 306, 323, 323, 323, 340, 340)

q <- ggplot(prospect.min, aes(y=st, x=year)) +
     geom_line() +
     ylim(0, .25)

## count of 'prospects' entering the league, by year
first.season <- dt.season[, .SD[which.min(year)], by = mls_id]
prospect.counts <- first.season[age <= 21 & year >= 2007, .N, by = year][order(year)]
p <- ggplot(prospect.counts, aes(x = year, y = N)) + geom_line()

## homegrowns
hg <- fread('../homegrowns.csv')
hg[, season := substr(date_signed, nchar(date_signed)-3, nchar(date_signed))]
hg[, .N, by = season][order(season)]
