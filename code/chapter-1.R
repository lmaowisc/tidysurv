# Tidy Survival Analysis - Chapter 1
# Run from the course project folder. Includes exercises and optional exports.

small <- data.frame(
  id = 1:5,
  time = c(5, 3, 8, 2, 6),
  status = c(1, 0, 1, 0, 1)
)
small

library(survival)
Surv(small$time, small$status)

mortality <- read.table("data/gbc_mort.txt", header = TRUE)
gbc <- read.table("data/gbc.txt", header = TRUE)
head(mortality[, c("id", "time", "status", "hormone", "age")])

c(
  mortality_rows = nrow(mortality),
  event_history_rows = nrow(gbc),
  patients = length(unique(gbc$id)),
  observed_deaths = sum(mortality$status == 1)
)

gbc[gbc$id == 3, c("id", "time", "status")]

ordered_records <- gbc[order(gbc$id, gbc$time, gbc$status == 0), ]
rfs <- ordered_records[!duplicated(ordered_records$id), ]
rfs$event <- as.integer(rfs$status > 0)
head(rfs[, c("id", "time", "status", "event")])

c(patients = nrow(rfs), events = sum(rfs$event))
anyDuplicated(rfs$id)

rfs$hormone <- factor(rfs$hormone, levels = c(1, 2),
                      labels = c("No", "Yes"))
rfs$meno <- factor(rfs$meno, levels = c(1, 2),
                   labels = c("Pre", "Post"))
rfs$grade <- factor(rfs$grade, levels = c(1, 2, 3),
                    labels = c("I", "II", "III"))
table(rfs$hormone, rfs$event)

sum(mortality$status == 1)

km_fit <- survfit(Surv(time, event) ~ hormone, data = rfs)
km_fit

summary(km_fit, times = c(12, 36, 60))

plot(km_fit,
     xlab = "Months since study entry",
     ylab = "Relapse-free survival probability",
     ylim = c(0, 1),
     col = c("#172d49", "#b0772b"), lty = c(1, 2),
     lwd = 2, conf.int = TRUE, mark.time = TRUE)
legend("bottomleft", legend = c("No hormone therapy", "Hormone therapy"),
       col = c("#172d49", "#b0772b"), lty = c(1, 2), lwd = 2,
       bty = "n")

logrank_fit <- survdiff(Surv(time, event) ~ hormone, data = rfs)
logrank_fit

pchisq(logrank_fit$chisq,
       df = length(logrank_fit$n) - 1,
       lower.tail = FALSE)

survdiff(Surv(time, event) ~ hormone + strata(meno), data = rfs)

cox_fit <- coxph(
  Surv(time, event) ~ hormone + meno + age + grade + size + prog + estrg,
  data = rfs, x = TRUE
)
summary(cox_fit)

exp(10 * coef(cox_fit)["size"])

round(cbind(
  hazard_ratio = exp(coef(cox_fit)),
  exp(confint(cox_fit))
), 3)

new_patient <- data.frame(
  hormone = factor("No", levels = levels(rfs$hormone)),
  meno = factor("Pre", levels = levels(rfs$meno)),
  age = 45,
  grade = factor("II", levels = levels(rfs$grade)),
  size = 20,
  prog = 100,
  estrg = 100
)
new_patient

predicted_survival <- survfit(cox_fit, newdata = new_patient)
summary(predicted_survival, times = c(12, 36, 60))

plot(predicted_survival,
     xlab = "Months since study entry",
     ylab = "Predicted relapse-free survival probability",
     ylim = c(0, 1), col = "#172d49", lwd = 2,
     conf.int = TRUE)

ph_test <- cox.zph(cox_fit)
ph_test

old_par <- par(mfrow = c(1, 2))
plot(ph_test, var = 1, se = TRUE, col = "#172d49")
plot(ph_test, var = 3, se = TRUE, col = "#172d49")
par(old_par)

fit_without_age <- update(cox_fit, . ~ . - age)
m <- residuals(fit_without_age, type = "martingale")
plot(rfs$age, m,
     xlab = "Age at diagnosis (years)",
     ylab = "Martingale residual", pch = 16,
     col = grDevices::adjustcolor("#172d49", alpha.f = 0.35))
lines(lowess(rfs$age, m), col = "#925415", lwd = 2)
abline(h = 0, lty = 3, col = "grey50")

revised_fit <- coxph(
  Surv(time, event) ~ hormone + meno + splines::ns(age, df = 3) +
    size + prog + estrg + strata(grade),
  data = rfs, x = TRUE
)
cox.zph(revised_fit)
