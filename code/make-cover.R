# Reproduce the cover figure from the supplied German breast cancer data.
library(survival)
gbc <- read.table("data/gbc.txt", header = TRUE)
gbc <- gbc[order(gbc$id, gbc$time), ]
first_event <- gbc[!duplicated(gbc$id), ]
fit <- survfit(Surv(time, status > 0) ~ hormone, data = first_event)
svg("images/cover-survival.svg", width = 5.2, height = 4.1, bg = "transparent", pointsize = 13)
par(mar = c(4.1, 4.1, 1, .8), fg = "#89919a", col.axis = "#596573", col.lab = "#596573", family = "sans")
plot(fit, col = c("#172d49", "#b0772b"), lty = c(1, 2), lwd = 2.2,
     conf.int = FALSE, mark.time = FALSE, xlim = c(0, 84), ylim = c(0, 1),
     xlab = "Months of follow-up", ylab = "Relapse-free survival", axes = FALSE)
axis(1, at = seq(0, 84, 12), tck = -.015)
axis(2, at = seq(0, 1, .25), las = 1, tck = -.015)
legend("bottomleft", c("No hormone therapy", "Hormone therapy"),
       col = c("#172d49", "#b0772b"), lty = c(1, 2), lwd = 2.2,
       bty = "n", cex = .83, text.col = "#425168")
invisible(dev.off())
