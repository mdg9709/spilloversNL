# --- Effect of German govt spending shock on Italian exports

# Data period: 1980q1-2018q4
# 95% and 68% confidence intervals
# h = 4, 8 and 12

# OLS with left-hand side in growth rates and 4 lags of x(t-1)

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers IT and DE v4 1.R')

# Load packages
library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)
library(lmtest)
library(sandwich)

rmIT.l <- data.frame(shift(data2$rmIT, n = 1:12, type = "lead"))
names(rmIT.l) = c("rmIT.l1", "rmIT.l2", "rmIT.l3", "rmIT.l4", "rmIT.l5", "rmIT.l6",
                  "rmIT.l7", "rmIT.l8", "rmIT.l9", "rmIT.l10", "rmIT.l11", "rmIT.l12")
rxIT.l <- data.frame(shift(data2$rxIT, n = 1:12, type = "lead"))
names(rxIT.l) = c("rxIT.l1", "rxIT.l2", "rxIT.l3", "rxIT.l4", "rxIT.l5", "rxIT.l6",
                  "rxIT.l7", "rxIT.l8", "rxIT.l9", "rxIT.l10", "rxIT.l11", "rxIT.l12")
l.rmIT <- data.frame(shift(data2$rmIT, n = 1:4, type = "lag"))
names(l.rmIT) = c("l1.rmIT", "l2.rmIT", "l3.rmIT", "l4.rmIT")
l.rxIT <- data.frame(shift(data2$rxIT, n = 1:4, type = "lag"))
names(l.rxIT) = c("l1.rxIT", "l2.rxIT", "l3.rxIT", "l4.rxIT")
rmDE.l <- data.frame(shift(data2$rmDE, n = 1:12, type = "lead"))
names(rmDE.l) = c("rmDE.l1", "rmDE.l2", "rmDE.l3", "rmDE.l4", "rmDE.l5", "rmDE.l6",
                  "rmDE.l7", "rmDE.l8", "rmDE.l9", "rmDE.l10", "rmDE.l11", "rmDE.l12")
rxDE.l <- data.frame(shift(data2$rxDE, n = 1:12, type = "lead"))
names(rxDE.l) = c("rxDE.l1", "rxDE.l2", "rxDE.l3", "rxDE.l4", "rxDE.l5", "rxDE.l6",
                  "rxDE.l7", "rxDE.l8", "rxDE.l9", "rxDE.l10", "rxDE.l11", "rxDE.l12")
l.rmDE <- data.frame(shift(data2$rmDE, n = 1:4, type = "lag"))
names(l.rmDE) = c("l1.rmDE", "l2.rmDE", "l3.rmDE", "l4.rmDE")
l.rxDE <- data.frame(shift(data2$rxDE, n = 1:4, type = "lag"))
names(l.rxDE) = c("l1.rxDE", "l2.rxDE", "l3.rxDE", "l4.rxDE")

data3$shockDE2 <- (data3$shockDE / unlist(l.rxDE[1])) / sd((data3$shockDE / unlist(l.rxDE[1])), na.rm = TRUE)
data3$shockDE3 <- data3$shockDE2 / 100
data3$shockIT2 <- (data3$shockIT / unlist(l.rxIT[1])) / sd((data3$shockIT / unlist(l.rxIT[1])), na.rm = TRUE)
data3$shockIT3 <- data3$shockIT2 / 100

data4 <- cbind(data3, l.rmIT, l.rxIT, rmIT.l, rxIT.l, l.rmDE, l.rxDE, rmDE.l, rxDE.l)
data5 <- subset(data4, select = -c(30:32, 35:37, 152:203))
h <- 12


# -- OLS regressions

# -- Equation 5
lhsDEIT50 <- (data5$rxIT - data5$l1.rxIT) / data5$rxIT
lhsDEIT5 <- lapply(1:h, function(x) (data5[, 165+x] - data5$l1.rxIT) / data5$l1.rxIT)
lhsDEIT5 <- data.frame(lhsDEIT5)
names(lhsDEIT5) = paste("lhsDEIT5", 1:h, sep = "")
data6 <- cbind(data5, lhsDEIT50, lhsDEIT5)
DEIT5 <- lapply(1:13, function(x) lm(data6[, 209+x] ~ shockDE2 + l1.debtIT + l1.intIT + l1.lrtrIT + l1.lrgIT + l1.lryITc + l2.debtIT + l2.intIT + l2.lrtrIT + l2.lrgIT + l2.lryITc + l3.debtIT + l3.intIT + l3.lrtrIT + l3.lrgIT + l3.lryITc + l4.debtIT + l4.intIT + l4.lrtrIT + l4.lrgIT + l4.lryITc + shockNL2 + shockIT2 + shockES2 + shockFR2, data = data6))
summariesDEIT5 <- lapply(DEIT5, summary)
DEIT5conf95 <- lapply(DEIT5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
DEIT5conf68 <- lapply(DEIT5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
DEIT5up95 <- lapply(1:13, function(x) DEIT5conf95[[x]][2,2])
DEIT5low95 <- lapply(1:13, function(x) DEIT5conf95[[x]][2,1])
DEIT5up68 <- lapply(1:13, function(x) DEIT5conf68[[x]][2,2])
DEIT5low68 <- lapply(1:13, function(x) DEIT5conf68[[x]][2,1])
betaDEITt <- lapply(summariesDEIT5, function(x) x$coefficients[2,1])
names(betaDEITt) <- paste("betaDEITt", 0:h, sep = "")

# -- Equation 6
lhsDEIT60 <- (data6$rgDE - data6$l1.rgDE) / data6$l1.rxIT
lhsDEIT6 <- lapply(1:h, function(x) (data6[, 84+x] - data6$l1.rgDE) / data6$l1.rxIT)
lhsDEIT6 <- data.frame(lhsDEIT6)
names(lhsDEIT6) = paste("lhsDEIT6", 1:h, sep = "")
data6 <- cbind(data6, lhsDEIT60, lhsDEIT6)
DEIT6 <- lapply(1:13, function(x) lm(data6[, 222+x] ~ shockDE3 + l1.debtDE + l1.intDE + l1.lrtrDE + l1.lrgDE + l1.lryDEc + l2.debtDE + l2.intDE + l2.lrtrDE + l2.lrgDE + l2.lryDEc + l3.debtDE + l3.intDE + l3.lrtrDE + l3.lrgDE + l3.lryDEc + l4.debtDE + l4.intDE + l4.lrtrDE + l4.lrgDE + l4.lryDEc + shockNL3 + shockFR3 + shockES3 + shockIT3, data = data6))
summariesDEIT6 <- lapply(DEIT6, summary)
DEIT6conf95 <- lapply(DEIT6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
DEIT6conf68 <- lapply(DEIT6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
DEIT6up95 <- lapply(1:13, function(x) DEIT6conf95[[x]][2,2])
DEIT6low95 <- lapply(1:13, function(x) DEIT6conf95[[x]][2,1])
DEIT6up68 <- lapply(1:13, function(x) DEIT6conf68[[x]][2,2])
DEIT6low68 <- lapply(1:13, function(x) DEIT6conf68[[x]][2,1])
gammaDEITt <- lapply(summariesDEIT6, function(x) x$coefficients[2,1])
names(gammaDEITt) <- paste("gammaDEITt", 0:h, sep = "")

# -- Cumulative multiplier
mDEITtc <- cumsum(betaDEITt) / cumsum(as.numeric(gammaDEITt)); as.numeric(mDEITtc)


# --- Effect of Italian govt spending shock on German exports

# -- Equation 5
lhsITDE50 <- (data6$rxDE - data6$l1.rxDE) / data6$l1.rxDE
lhsITDE5 <- lapply(1:h, function(x) (data6[, 197+x] - data6$l1.rxDE) / data6$l1.rxDE)
lhsITDE5 <- data.frame(lhsITDE5)
names(lhsITDE5) = paste("lhsITDE5", 1:h, sep = "")
data6 <- cbind(data6, lhsITDE50, lhsITDE5)
ITDE5 <- lapply(1:13, function(x) lm(data6[, 235+x] ~ shockIT2 + l1.debtDE + l1.intDE + l1.lrtrDE + l1.lrgDE + l1.lryDEc + l2.debtDE + l2.intDE + l2.lrtrDE + l2.lrgDE + l2.lryDEc + l3.debtDE + l3.intDE + l3.lrtrDE + l3.lrgDE + l3.lryDEc + l4.debtDE + l4.intDE + l4.lrtrDE + l4.lrgDE + l4.lryDEc + shockNL2 + shockDE2 + shockFR2 + shockES2, data = data6))
summariesITDE5 <- lapply(ITDE5, summary)
ITDE5conf95 <- lapply(ITDE5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ITDE5conf68 <- lapply(ITDE5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ITDE5up95 <- lapply(1:13, function(x) ITDE5conf95[[x]][2,2])
ITDE5low95 <- lapply(1:13, function(x) ITDE5conf95[[x]][2,1])
ITDE5up68 <- lapply(1:13, function(x) ITDE5conf68[[x]][2,2])
ITDE5low68 <- lapply(1:13, function(x) ITDE5conf68[[x]][2,1])
betaITDEt <- lapply(summariesITDE5, function(x) x$coefficients[2,1])
names(betaITDEt) <- paste("betaITDEt", 0:h, sep = "")

# -- Equation 6
lhsITDE60 <- (data6$rgIT - data6$l1.rgIT) / data6$l1.rxDE
lhsITDE6 <- lapply(1:h, function(x) (data6[, 96+x] - data6$l1.rgIT) / data6$l1.rxDE)
lhsITDE6 <- data.frame(lhsITDE6)
names(lhsITDE6) = paste("lhsITDE6", 1:h, sep = "")
data6 <- cbind(data6, lhsITDE60, lhsITDE6)
ITDE6 <- lapply(1:13, function(x) lm(data6[, 248+x] ~ shockIT3 + l1.debtIT + l1.intIT + l1.lrtrIT + l1.lrgIT + l1.lryITc + l2.debtIT + l2.intIT + l2.lrtrIT + l2.lrgIT + l2.lryITc + l3.debtIT + l3.intIT + l3.lrtrIT + l3.lrgIT + l3.lryITc + l4.debtIT + l4.intIT + l4.lrtrIT + l4.lrgIT + l4.lryITc + shockDE3 + shockNL3 + shockFR3 + shockES3, data = data6))
summariesITDE6 <- lapply(ITDE6, summary)
ITDE6conf95 <- lapply(ITDE6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ITDE6conf68 <- lapply(ITDE6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ITDE6up95 <- lapply(1:13, function(x) ITDE6conf95[[x]][2,2])
ITDE6low95 <- lapply(1:13, function(x) ITDE6conf95[[x]][2,1])
ITDE6up68 <- lapply(1:13, function(x) ITDE6conf68[[x]][2,2])
ITDE6low68 <- lapply(1:13, function(x) ITDE6conf68[[x]][2,1])
gammaITDEt <- lapply(summariesITDE6, function(x) x$coefficients[2,1])
names(gammaITDEt) <- paste("gammaITDEt", 0:h, sep = "")

# -- Cumulative multiplier
mITDEtc <- cumsum(betaITDEt) / cumsum(as.numeric(gammaITDEt)); as.numeric(mITDEtc)
