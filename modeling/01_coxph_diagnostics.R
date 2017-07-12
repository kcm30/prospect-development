library(data.table)
library(survival)

dt <- fread("../person_period.csv")

## diagnostics to check for proportional hazards
## check draft flag
draftflag.fit <- survfit(Surv(duration, event) ~ 1, subset = (draftflag == 1), data=dt)
no.draft.fit <- survfit(Surv(duration, event) ~ 1, subset = (draftflag == 0), data=dt)
plot(draftflag.fit$time, draftflag.fit$surv, type = 'l', ylim=c(0,1), col='blue')
lines(no.draft.fit$time, no.draft.fit$surv, type = 'l', col='red')
## check hgflag
hg.fit <- survfit(Surv(duration, event) ~ 1, subset = (hgflag == 1), data=dt)
no.hg.fit <- survfit(Surv(duration, event) ~ 1, subset = (hgflag == 0), data=dt)
plot(hg.fit$time, hg.fit$surv, type = 'l', ylim=c(0,1), col='blue')
lines(no.hg.fit$time, no.hg.fit$surv, type = 'l', col='red')
## already checked position more or less with KM
## check mls2flag (should make this a function)
fit <- survfit(Surv(duration, event) ~ 1, subset = (mls2flag == 1), data=dt)
no.fit <- survfit(Surv(duration, event) ~ 1, subset = (mls2flag == 0), data=dt)
plot(fit$time, fit$surv, type = 'l', ylim=c(0,1), col='blue')
lines(no.fit$time, no.fit$surv, type = 'l', col='red') ## seems no effect
## check pick value
fit <- survfit(Surv(duration, event) ~ 1, subset = (pick > 0 & pick < 20), data=dt)
no.fit <- survfit(Surv(duration, event) ~ 1, subset = (pick >= 20), data=dt)
plot(fit$time, fit$surv, type = 'l', ylim=c(0,1), col='blue')
lines(no.fit$time, no.fit$surv, type = 'l', col='red')
## check entry age
fit <- survfit(Surv(duration, event) ~ 1, subset = (entry.age >18), data=dt)
no.fit <- survfit(Surv(duration, event) ~ 1, subset = (entry.age <= 18), data=dt)
plot(fit$time, fit$surv, type = 'l', ylim=c(0,1), col='blue')
lines(no.fit$time, no.fit$surv, type = 'l', col='red')
## already saw that first.yr doesn't really make sense, from KM

