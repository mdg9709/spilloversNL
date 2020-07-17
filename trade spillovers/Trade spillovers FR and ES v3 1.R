# --- Effect of Spanish govt spending shock on French exports

# Data period: 1980q1-2018q4
# 95% and 68% confidence intervals
# h = 4, 8 and 12

# OLS with left-hand side in growth rates and 4 lags of x(t-1)

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers FR and ES v4 1.R')

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
rmES.l <- data.frame(shift(data2$rmES, n = 1:12, type = "lead"))
names(rmES.l) = c("rmES.l1", "rmES.l2", "rmES.l3", "rmES.l4", "rmES.l5", "rmES.l6",
                  "rmES.l7", "rmES.l8", "rmES.l9", "rmES.l10", "rmES.l11", "rmES.l12")
rxES.l <- data.frame(shift(data2$rxES, n = 1:12, type = "lead"))
names(rxES.l) = c("rxES.l1", "rxES.l2", "rxES.l3", "rxES.l4", "rxES.l5", "rxES.l6",
                  "rxES.l7", "rxES.l8", "rxES.l9", "rxES.l10", "rxES.l11", "rxES.l12")
l.rmES <- data.frame(shift(data2$rmES, n = 1:4, type = "lag"))
names(l.rmES) = c("l1.rmES", "l2.rmES", "l3.rmES", "l4.rmES")
l.rxES <- data.frame(shift(data2$rxES, n = 1:4, type = "lag"))
names(l.rxES) = c("l1.rxES", "l2.rxES", "l3.rxES", "l4.rxES")

data3$shockFR2 <- (data3$shockFR / unlist(l.rxFR[1])) / sd((data3$shockFR / unlist(l.rxFR[1])), na.rm = TRUE)
data3$shockFR3 <- data3$shockFR2 / 100
data3$shockES2 <- (data3$shockES / unlist(l.rxES[1])) / sd((data3$shockES / unlist(l.rxES[1])), na.rm = TRUE)
data3$shockES3 <- data3$shockES2 / 100

data4 <- cbind(data3, l.rmFR, l.rxFR, rmFR.l, rxFR.l, l.rmES, l.rxES, rmES.l, rxES.l)
data5 <- subset(data4, select = -c(30:32, 35:37, 153:204))
h <- 12


# -- OLS regressions

# -- Equation 5
lhsESFR50 <- (data5$rxFR - data5$l1.rxFR) / data5$l1.rxFR
lhsESFR5 <- lapply(1:h, function(x) (data5[, 166+x] - data5$l1.rxFR) / data5$l1.rxFR)
lhsESFR5 <- data.frame(lhsESFR5)
names(lhsESFR5) = paste("lhsESFR5", 1:h, sep = "")
data6 <- cbind(data5, lhsESFR50, lhsESFR5)
ESFR5 <- lapply(1:13, function(x) lm(data6[, 210+x] ~ shockES2 + l1.debtFR + l1.intFR + l1.lrtrFR + l1.lrgFR + l1.lryFRc + l2.debtFR + l2.intFR + l2.lrtrFR + l2.lrgFR + l2.lryFRc + l3.debtFR + l3.intFR + l3.lrtrFR + l3.lrgFR + l3.lryFRc + l4.debtFR + l4.intFR + l4.lrtrFR + l4.lrgFR + l4.lryFRc + shockDE2 + shockFR2 + shockNL2 + shockIT2, data = data6))
summariesESFR5 <- lapply(ESFR5, summary)
ESFR5conf95 <- lapply(ESFR5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ESFR5conf68 <- lapply(ESFR5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ESFR5up95 <- lapply(1:13, function(x) ESFR5conf95[[x]][2,2])
ESFR5low95 <- lapply(1:13, function(x) ESFR5conf95[[x]][2,1])
ESFR5up68 <- lapply(1:13, function(x) ESFR5conf68[[x]][2,2])
ESFR5low68 <- lapply(1:13, function(x) ESFR5conf68[[x]][2,1])
betaESFRt <- lapply(summariesESFR5, function(x) x$coefficients[2,1])
names(betaESFRt) <- paste("betaESFRt", 0:h, sep = "")

# -- Equation 6
lhsESFR60 <- (data6$rgES - data6$l1.rgES) / data6$l1.rxFR
lhsESFR6 <- lapply(1:h, function(x) (data6[, 84+x] - data6$l1.rgES) / data6$l1.rxFR)
lhsESFR6 <- data.frame(lhsESFR6)
names(lhsESFR6) = paste("lhsESFR6", 1:h, sep = "")
data6 <- cbind(data6, lhsESFR60, lhsESFR6)
ESFR6 <- lapply(1:13, function(x) lm(data6[, 223+x] ~ shockES3 + l1.debtES + l1.intES + l1.lrtrES + l1.lrgES + l1.lryESc + l2.debtES + l2.intES + l2.lrtrES + l2.lrgES + l2.lryESc + l3.debtES + l3.intES + l3.lrtrES + l3.lrgES + l3.lryESc + l4.debtES + l4.intES + l4.lrtrES + l4.lrgES + l4.lryESc + shockDE3 + shockFR3 + shockNL3 + shockIT3, data = data6))
summariesESFR6 <- lapply(ESFR6, summary)
ESFR6conf95 <- lapply(ESFR6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ESFR6conf68 <- lapply(ESFR6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ESFR6up95 <- lapply(1:13, function(x) ESFR6conf95[[x]][2,2])
ESFR6low95 <- lapply(1:13, function(x) ESFR6conf95[[x]][2,1])
ESFR6up68 <- lapply(1:13, function(x) ESFR6conf68[[x]][2,2])
ESFR6low68 <- lapply(1:13, function(x) ESFR6conf68[[x]][2,1])
gammaESFRt <- lapply(summariesESFR6, function(x) x$coefficients[2,1])
names(gammaESFRt) <- paste("gammaESFRt", 0:h, sep = "")

# -- Cumulative multiplier
mESFRtc <- cumsum(betaESFRt) / cumsum(as.numeric(gammaESFRt)); as.numeric(mESFRtc)


# --- Effect of French govt spending shock on Spanish exports

# -- Equation 5
lhsFRES50 <- (data6$rxES - data6$l1.rxES) / data6$l1.rxES
lhsFRES5 <- lapply(1:h, function(x) (data6[, 198+x] - data6$l1.rxES) / data6$l1.rxES)
lhsFRES5 <- data.frame(lhsFRES5)
names(lhsFRES5) = paste("lhsFRES5", 1:h, sep = "")
data6 <- cbind(data6, lhsFRES50, lhsFRES5)
FRES5 <- lapply(1:13, function(x) lm(data6[, 236+x] ~ shockFR2 + l1.debtES + l1.intES + l1.lrtrES + l1.lrgES + l1.lryESc + l2.debtES + l2.intES + l2.lrtrES + l2.lrgES + l2.lryESc + l3.debtES + l3.intES + l3.lrtrES + l3.lrgES + l3.lryESc + l4.debtES + l4.intES + l4.lrtrES + l4.lrgES + l4.lryESc + shockDE2 + shockNL2 + shockIT2 + shockES2, data = data6))
summariesFRES5 <- lapply(FRES5, summary)
FRES5conf95 <- lapply(FRES5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
FRES5conf68 <- lapply(FRES5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
FRES5up95 <- lapply(1:13, function(x) FRES5conf95[[x]][2,2])
FRES5low95 <- lapply(1:13, function(x) FRES5conf95[[x]][2,1])
FRES5up68 <- lapply(1:13, function(x) FRES5conf68[[x]][2,2])
FRES5low68 <- lapply(1:13, function(x) FRES5conf68[[x]][2,1])
betaFRESt <- lapply(summariesFRES5, function(x) x$coefficients[2,1])
names(betaFRESt) <- paste("betaFRESt", 0:h, sep = "")

# -- Equation 6
lhsFRES60 <- (data6$rgFR - data6$l1.rgFR) / data6$l1.rxES
lhsFRES6 <- lapply(1:h, function(x) (data6[, 96+x] - data6$l1.rgFR) / data6$l1.rxES)
lhsFRES6 <- data.frame(lhsFRES6)
names(lhsFRES6) = paste("lhsFRES6", 1:h, sep = "")
data6 <- cbind(data6, lhsFRES60, lhsFRES6)
FRES6 <- lapply(1:13, function(x) lm(data6[, 249+x] ~ shockFR3 + l1.debtFR + l1.intFR + l1.lrtrFR + l1.lrgFR + l1.lryFRc + l2.debtFR + l2.intFR + l2.lrtrFR + l2.lrgFR + l2.lryFRc + l3.debtFR + l3.intFR + l3.lrtrFR + l3.lrgFR + l3.lryFRc + l4.debtFR + l4.intFR + l4.lrtrFR + l4.lrgFR + l4.lryFRc + shockDE3 + shockNL3 + shockES3 + shockIT3, data = data6))
summariesFRES6 <- lapply(FRES6, summary)
FRES6conf95 <- lapply(FRES6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
FRES6conf68 <- lapply(FRES6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
FRES6up95 <- lapply(1:13, function(x) FRES6conf95[[x]][2,2])
FRES6low95 <- lapply(1:13, function(x) FRES6conf95[[x]][2,1])
FRES6up68 <- lapply(1:13, function(x) FRES6conf68[[x]][2,2])
FRES6low68 <- lapply(1:13, function(x) FRES6conf68[[x]][2,1])
gammaFRESt <- lapply(summariesFRES6, function(x) x$coefficients[2,1])
names(gammaFRESt) <- paste("gammaFRESt", 0:h, sep = "")

# -- Cumulative multiplier
mFREStc <- cumsum(betaFRESt) / cumsum(as.numeric(gammaFRESt)); as.numeric(mFREStc)
