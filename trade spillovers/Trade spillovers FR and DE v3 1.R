# --- Effect of German govt spending shock on French exports

# Data period: 1980q1-2018q4
# 95% and 68% confidence intervals
# h = 4, 8 and 12

# OLS with left-hand side in growth rates and 4 lags of x(t-1)

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers FR and DE v4 1.R')

# Load packages
library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)
library(lmtest)
library(sandwich)

rmFR.l <- data.frame(shift(data2$rmFR, n = 1:12, type = "lead"))
names(rmFR.l) = c("rmFR.l1", "rmFR.l2", "rmFR.l3", "rmFR.l4", "rmFR.l5", "rmFR.l6",
                  "rmFR.l7", "rmFR.l8", "rmFR.l9", "rmFR.l10", "rmFR.l11", "rmFR.l12")
rxFR.l <- data.frame(shift(data2$rxFR, n = 1:12, type = "lead"))
names(rxFR.l) = c("rxFR.l1", "rxFR.l2", "rxFR.l3", "rxFR.l4", "rxFR.l5", "rxFR.l6",
                  "rxFR.l7", "rxFR.l8", "rxFR.l9", "rxFR.l10", "rxFR.l11", "rxFR.l12")
l.rmFR <- data.frame(shift(data2$rmFR, n = 1:4, type = "lag"))
names(l.rmFR) = c("l1.rmFR", "l2.rmFR", "l3.rmFR", "l4.rmFR")
l.rxFR <- data.frame(shift(data2$rxFR, n = 1:4, type = "lag"))
names(l.rxFR) = c("l1.rxFR", "l2.rxFR", "l3.rxFR", "l4.rxFR")
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

data3$shockFR2 <- (data3$shockFR / unlist(l.rxFR[1])) / sd((data3$shockFR / unlist(l.rxFR[1])), na.rm = TRUE)
data3$shockFR3 <- data3$shockFR2 / 100
data3$shockDE2 <- (data3$shockDE / unlist(l.rxDE[1])) / sd((data3$shockDE / unlist(l.rxDE[1])), na.rm = TRUE)
data3$shockDE3 <- data3$shockDE2 / 100

shockFR5 <- data.frame(data3$shockFR2 * 100); names(shockFR5) <- "shockFR5"
shockNL5 <- data.frame(data3$shockNL2 * 100); names(shockNL5) <- "shockNL5"
shockDE5 <- data.frame(data3$shockDE2 * 100); names(shockDE5) <- "shockDE5"
shockES5 <- data.frame(data3$shockES2 * 100); names(shockES5) <- "shockES5"
shockIT5 <- data.frame(data3$shockIT2 * 100); names(shockIT5) <- "shockIT5"

data4 <- cbind(data3, shockFR5, shockNL5, shockDE5, shockIT5, shockES5, 
               l.rmDE, l.rxDE, rmDE.l, rxDE.l, l.rmFR, l.rxFR, rmFR.l, rxFR.l)
data5 <- subset(data4, select = -c(30:32, 35:37, 153:204))
h <- 12


# -- OLS regressions

# -- Equation 5
lhsDEFR50 <- (data5$rxFR - data5$l1.rxFR) / data5$l1.rxFR
lhsDEFR5 <- lapply(1:h, function(x) (data5[, 171+x] - data5$l1.rxFR) / data5$l1.rxFR)
lhsDEFR5 <- data.frame(lhsDEFR5)
names(lhsDEFR5) = paste("lhsDEFR5", 1:h, sep = "")
data6 <- cbind(data5, lhsDEFR50, lhsDEFR5)
DEFR5 <- lapply(1:13, function(x) lm(data6[, 215+x] ~ shockDE2 + l1.debtFR + l1.intFR + l1.lrtrFR + l1.lrgFR + l1.lryFRc + l2.debtFR + l2.intFR + l2.lrtrFR + l2.lrgFR + l2.lryFRc + l3.debtFR + l3.intFR + l3.lrtrFR + l3.lrgFR + l3.lryFRc + l4.debtFR + l4.intFR + l4.lrtrFR + l4.lrgFR + l4.lryFRc + shockNL2 + shockFR2 + shockES2 + shockIT2, data = data6))
summariesDEFR5 <- lapply(DEFR5, summary)
DEFR5conf95 <- lapply(DEFR5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
DEFR5conf68 <- lapply(DEFR5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
DEFR5up95 <- lapply(1:13, function(x) DEFR5conf95[[x]][2,2])
DEFR5low95 <- lapply(1:13, function(x) DEFR5conf95[[x]][2,1])
DEFR5up68 <- lapply(1:13, function(x) DEFR5conf68[[x]][2,2])
DEFR5low68 <- lapply(1:13, function(x) DEFR5conf68[[x]][2,1])
betaDEFRt <- lapply(summariesDEFR5, function(x) x$coefficients[2,1])
names(betaDEFRt) <- paste("betaDEFRt", 0:h, sep = "")

# -- Equation 6
lhsDEFR60 <- (data6$rgDE - data6$l1.rgDE) / data6$l1.rxFR
lhsDEFR6 <- lapply(1:h, function(x) (data6[, 84+x] - data6$l1.rgDE) / data6$l1.rxFR)
lhsDEFR6 <- data.frame(lhsDEFR6)
names(lhsDEFR6) = paste("lhsDEFR6", 1:h, sep = "")
data6 <- cbind(data6, lhsDEFR60, lhsDEFR6)
DEFR6 <- lapply(1:13, function(x) lm(data6[, 228+x] ~ shockDE3 + l1.debtDE + l1.intDE + l1.lrtrDE + l1.lrgDE + l1.lryDEc + l2.debtDE + l2.intDE + l2.lrtrDE + l2.lrgDE + l2.lryDEc + l3.debtDE + l3.intDE + l3.lrtrDE + l3.lrgDE + l3.lryDEc + l4.debtDE + l4.intDE + l4.lrtrDE + l4.lrgDE + l4.lryDEc + shockNL3 + shockFR3 + shockES3 + shockIT3, data = data6))
summariesDEFR6 <- lapply(DEFR6, summary)
DEFR6conf95 <- lapply(DEFR6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
DEFR6conf68 <- lapply(DEFR6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
DEFR6up95 <- lapply(1:13, function(x) DEFR6conf95[[x]][2,2])
DEFR6low95 <- lapply(1:13, function(x) DEFR6conf95[[x]][2,1])
DEFR6up68 <- lapply(1:13, function(x) DEFR6conf68[[x]][2,2])
DEFR6low68 <- lapply(1:13, function(x) DEFR6conf68[[x]][2,1])
gammaDEFRt <- lapply(summariesDEFR6, function(x) x$coefficients[2,1])
names(gammaDEFRt) <- paste("gammaDEFRt", 0:h, sep = "")

# -- Cumulative multiplier
mDEFRtc <- cumsum(betaDEFRt) / cumsum(as.numeric(gammaDEFRt)); as.numeric(mDEFRtc)


# --- Effect of French govt spending shock on German exports

# -- Equation 5
lhsFRDE50 <- (data6$rxDE - data6$l1.rxDE) / data6$l1.rxDE
lhsFRDE5 <- lapply(1:h, function(x) (data6[, 203+x] - data6$l1.rxDE) / data6$l1.rxDE)
lhsFRDE5 <- data.frame(lhsFRDE5)
names(lhsFRDE5) = paste("lhsFRDE5", 1:h, sep = "")
data6 <- cbind(data6, lhsFRDE50, lhsFRDE5)
FRDE5 <- lapply(1:13, function(x) lm(data6[, 241+x] ~ shockFR5 + l1.debtDE + l1.intDE + l1.lrtrDE + l1.lrgDE + l1.lryDEc + l2.debtDE + l2.intDE + l2.lrtrDE + l2.lrgDE + l2.lryDEc + l3.debtDE + l3.intDE + l3.lrtrDE + l3.lrgDE + l3.lryDEc + l4.debtDE + l4.intDE + l4.lrtrDE + l4.lrgDE + l4.lryDEc + shockDE5 + shockES5 + shockIT5 + shockNL5, data = data6))
summariesFRDE5 <- lapply(FRDE5, summary)
FRDE5conf95 <- lapply(FRDE5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
FRDE5conf68 <- lapply(FRDE5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
FRDE5up95 <- lapply(1:13, function(x) FRDE5conf95[[x]][2,2])
FRDE5low95 <- lapply(1:13, function(x) FRDE5conf95[[x]][2,1])
FRDE5up68 <- lapply(1:13, function(x) FRDE5conf68[[x]][2,2])
FRDE5low68 <- lapply(1:13, function(x) FRDE5conf68[[x]][2,1])
betaFRDEt <- lapply(summariesFRDE5, function(x) x$coefficients[2,1])
names(betaFRDEt) <- paste("betaFRDEt", 0:h, sep = "")

# -- Equation 6
lhsFRDE60 <- (data6$rgFR - data6$l1.rgFR) / data6$l1.rxDE
lhsFRDE6 <- lapply(1:h, function(x) (data6[, 96+x] - data6$l1.rgFR) / data6$l1.rxDE)
lhsFRDE6 <- data.frame(lhsFRDE6)
names(lhsFRDE6) = paste("lhsFRDE6", 1:h, sep = "")
data6 <- cbind(data6, lhsFRDE60, lhsFRDE6)
FRDE6 <- lapply(1:13, function(x) lm(data6[, 254+x] ~ shockFR3 + l1.debtFR + l1.intFR + l1.lrtrFR + l1.lrgFR + l1.lryFRc + l2.debtFR + l2.intFR + l2.lrtrFR + l2.lrgFR + l2.lryFRc + l3.debtFR + l3.intFR + l3.lrtrFR + l3.lrgFR + l3.lryFRc + l4.debtFR + l4.intFR + l4.lrtrFR + l4.lrgFR + l4.lryFRc + shockDE3 + shockNL3 + shockES3 + shockIT3, data = data6))
summariesFRDE6 <- lapply(FRDE6, summary)
FRDE6conf95 <- lapply(FRDE6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
FRDE6conf68 <- lapply(FRDE6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
FRDE6up95 <- lapply(1:13, function(x) FRDE6conf95[[x]][2,2])
FRDE6low95 <- lapply(1:13, function(x) FRDE6conf95[[x]][2,1])
FRDE6up68 <- lapply(1:13, function(x) FRDE6conf68[[x]][2,2])
FRDE6low68 <- lapply(1:13, function(x) FRDE6conf68[[x]][2,1])
gammaFRDEt <- lapply(summariesFRDE6, function(x) x$coefficients[2,1])
names(gammaFRDEt) <- paste("gammaFRDEt", 0:h, sep = "")

# -- Cumulative multiplier
mFRDEtc <- cumsum(betaFRDEt) / cumsum(as.numeric(gammaFRDEt)); as.numeric(mFRDEtc)
