library(data.table)
library(survival)
library(ggplot2)

dt <- fread("../person_period.csv")
person.level <- fread('../person_level.csv')
dt <- merge(dt, person.level[, c("mls_id", "club"), with = FALSE], by = 'mls_id')
colnames(dt)[15] <- 'first.club'

## set up CV scheme - really should use caret
set.seed(1112)
mls_ids <- data.table(mls_id = unique(dt$mls_id), rand = floor(10*runif(496,0,1)))
dt <- merge(dt, mls_ids)

## really should use caret
########### fit 1 #########################
fit1.concord <- c()
for(fold in seq(0,9)){
    fit1 <- coxph(Surv(duration, event) ~ position + draftflag + mls2flag + pick + entry.age + hgflag + frailty(first.club),
              data = dt[rand != fold, ],
              ties='exact')
    concord <- survConcordance(Surv(duration, event) ~ predict(fit1, newdata=dt[rand == fold, ]), data=dt[rand == fold, ])$concordance
    fit1.concord <- c(fit1.concord, concord)
}
print(mean(fit1.concord))
###############################################


########### fit 2 #########################
fit2.concord <- c()
for(fold in seq(0,9)){
    fit2 <- coxph(Surv(duration, event) ~ position + draftflag + pick + entry.age + hgflag + frailty(first.club),
              data = dt[rand != fold, ],
              ties='exact')
    concord <- survConcordance(Surv(duration, event) ~ predict(fit2, newdata=dt[rand == fold, ]), data=dt[rand == fold, ])$concordance
    fit2.concord <- c(fit2.concord, concord)
}
print(mean(fit2.concord)) ## improves dropping mls2flag
###############################################

########### fit 3 #########################
fit3.concord <- c()
for(fold in seq(0,9)){
    fit3 <- coxph(Surv(duration, event) ~ position + draftflag + pick + entry.age + hgflag,
              data = dt[rand != fold, ],
              ties='exact')
    concord <- survConcordance(Surv(duration, event) ~ predict(fit3, newdata=dt[rand == fold, ]), data=dt[rand == fold, ])$concordance
    fit3.concord <- c(fit3.concord, concord)
}
print(mean(fit3.concord)) ## dropping random team effect doesnt matter
###############################################

########### fit 4 #########################
fit4.concord <- c()
for(fold in seq(0,9)){
    fit4 <- coxph(Surv(duration, event) ~ position + draftflag + pick + hgflag,
              data = dt[rand != fold, ],
              ties='exact')
    concord <- survConcordance(Surv(duration, event) ~ predict(fit4, newdata=dt[rand == fold, ]), data=dt[rand == fold, ])$concordance
    fit4.concord <- c(fit4.concord, concord)
}
print(mean(fit4.concord)) ## gets worse after dropping entry age
###############################################

## let's fix draft pick first
dt[draftflag==0, pick := 0]
dt[draftflag == 0, undraft := 1]
dt[draftflag == 1, undraft := 0]
## so model three is the final fit
final.fit <- coxph(Surv(duration, event) ~ position + draftflag + pick + entry.age + hgflag, data = dt, ties='exact')

##############################################
## now get curves for chris durkin and ian harkes
dcu <- data.table(mls_id = c('chris-durkin', 'chris-durkin', 'ian-harkes'),
                  position = c('Midfielder', 'Midfielder', 'Midfielder'),
                  draftflag = c(0, 0, 0),
                  pick = c(0, 0, 0),
                  entry.age = c(16, 17, 21),
                  hgflag = c(1, 1, 1),
                  duration = c(1, 2, 1),
                  event = c(0, 0, 0))

plot(survfit(final.fit, newdata = dcu[mls_id == 'ian-harkes', ]), main = 'Ian Harkes Development Curve', xlab = 'Season', col='red')
lines(harkes$surv)
dev.new()
plot(survfit(final.fit, newdata = dcu[mls_id == 'chris-durkin' & duration == 2, ]), main = 'Chris Durkin Development Curve', xlab = 'Season', col='red')
## combined survival curve for at least one of them becoming a starter
harkes <- survfit(final.fit, newdata = dcu[mls_id == 'ian-harkes', ])
durkin <- survfit(final.fit, newdata = dcu[mls_id == 'chris-durkin' & duration == 2, ])
comb.surv <- data.table(surv = harkes$surv * durkin$surv, t = seq(2017, 2026), lower = harkes$lower * durkin$lower, upper = harkes$upper, durkin$upper)
p <- ggplot(comb.surv, aes(y = surv, x= t)) +
    geom_line(col='navy', lwd=2) +
    ylim(0, 1) +
    ylab("P(Harkes or Durkin Becomes a Starter)") +
    xlab("Season") +
    ggtitle("Risk Profile for DCU's Youth MF Prospects") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2) +
    geom_line(aes(y=lower, x = t), linetype='dashed') +
    geom_line(aes(y=upper, x = t), linetype='dashed')


