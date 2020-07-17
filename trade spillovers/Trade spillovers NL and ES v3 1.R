# --- Effect of Dutch govt spending shock on Spanish exports

# Data period: 1980q1-2018q4 and 1999q1-2018q4
# 95% and 68% confidence intervals
# h = 4, 8 and 12

# OLS with left-hand side in growth rates and 4 lags of x(t-1)

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers NL and ES v4 1.R')

# Load packages
library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)
library(lmtest)
library(sandwich)

rmNL.l <- data.frame(shift(data2$rmNL, n = 1:12, type = "lead"))
names(rmNL.l) = c("rmNL.l1", "rmNL.l2", "rmNL.l3", "rmNL.l4", "rmNL.l5", "rmNL.l6",
                  "rmNL.l7", "rmNL.l8", "rmNL.l9", "rmNL.l10", "rmNL.l11", "rmNL.l12")
rxNL.l <- data.frame(shift(data2$rxNL, n = 1:12, type = "lead"))
names(rxNL.l) = c("rxNL.l1", "rxNL.l2", "rxNL.l3", "rxNL.l4", "rxNL.l5", "rxNL.l6",
                  "rxNL.l7", "rxNL.l8", "rxNL.l9", "rxNL.l10", "rxNL.l11", "rxNL.l12")
l.rmNL <- data.frame(shift(data2$rmNL, n = 1:4, type = "lag"))
names(l.rmNL) = c("l1.rmNL", "l2.rmNL", "l3.rmNL", "l4.rmNL")
l.rxNL <- data.frame(shift(data2$rxNL, n = 1:4, type = "lag"))
names(l.rxNL) = c("l1.rxNL", "l2.rxNL", "l3.rxNL", "l4.rxNL")
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

data3$shockNL2 <- (data3$shockNL / unlist(l.rxNL[1])) / sd((data3$shockNL / unlist(l.rxNL[1])), na.rm = TRUE)
data3$shockNL3 <- data3$shockNL2 / 100
data3$shockES2 <- (data3$shockES / unlist(l.rxES[1])) / sd((data3$shockES / unlist(l.rxES[1])), na.rm = TRUE)
data3$shockES3 <- data3$shockES2 / 100

shockFR5 <- data.frame(data3$shockFR2 * 100); names(shockFR5) <- "shockFR5"
shockNL5 <- data.frame(data3$shockNL2 * 100); names(shockNL5) <- "shockNL5"
shockDE5 <- data.frame(data3$shockDE2 * 100); names(shockDE5) <- "shockDE5"
shockES5 <- data.frame(data3$shockES2 * 100); names(shockES5) <- "shockES5"
shockIT5 <- data.frame(data3$shockIT2 * 100); names(shockIT5) <- "shockIT5"

data4 <- cbind(data3, shockFR5, shockNL5, shockDE5, shockIT5, shockES5, 
               l.rmNL, l.rxNL, rmNL.l, rxNL.l, l.rmES, l.rxES, rmES.l, rxES.l)
data5 <- subset(data4, select = -c(30:32, 35:37, 152:203))
h <- 12


# -- OLS regressions

# -- Equation 5
lhsNLES50 <- (data5$rxES - data5$l1.rxES) / data5$rxES
lhsNLES5 <- lapply(1:h, function(x) (data5[, 202+x] - data5$l1.rxES) / data5$l1.rxES)
lhsNLES5 <- data.frame(lhsNLES5)
names(lhsNLES5) = paste("lhsNLES5", 1:h, sep = "")
data6 <- cbind(data5, lhsNLES50, lhsNLES5)
NLES5 <- lapply(1:13, function(x) lm(data6[, 214+x] ~ shockNL2 + l1.debtES + l1.intES + l1.lrtrES + l1.lrgES + l1.lryESc + l2.debtES + l2.intES + l2.lrtrES + l2.lrgES + l2.lryESc + l3.debtES + l3.intES + l3.lrtrES + l3.lrgES + l3.lryESc + l4.debtES + l4.intES + l4.lrtrES + l4.lrgES + l4.lryESc + shockDE2 + shockFR2 + shockIT2 + shockES2, data = data6))
summariesNLES5 <- lapply(NLES5, summary)
NLES5conf95 <- lapply(NLES5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
NLES5conf68 <- lapply(NLES5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
NLES5up95 <- lapply(1:13, function(x) NLES5conf95[[x]][2,2])
NLES5low95 <- lapply(1:13, function(x) NLES5conf95[[x]][2,1])
NLES5up68 <- lapply(1:13, function(x) NLES5conf68[[x]][2,2])
NLES5low68 <- lapply(1:13, function(x) NLES5conf68[[x]][2,1])
betaNLESt <- lapply(summariesNLES5, function(x) x$coefficients[2,1])
names(betaNLESt) <- paste("betaNLESt", 0:h, sep = "")

# -- Equation 6
lhsNLES60 <- (data6$rgNL - data6$l1.rgNL) / data6$l1.rxES
lhsNLES6 <- lapply(1:h, function(x) (data6[, 96+x] - data6$l1.rgNL) / data6$l1.rxES)
lhsNLES6 <- data.frame(lhsNLES6)
names(lhsNLES6) = paste("lhsNLES6", 1:h, sep = "")
data6 <- cbind(data6, lhsNLES60, lhsNLES6)
NLES6 <- lapply(1:13, function(x) lm(data6[, 227+x] ~ shockNL3 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc + shockDE3 + shockFR3 + shockES3 + shockIT3, data = data6))
summariesNLES6 <- lapply(NLES6, summary)
NLES6conf95 <- lapply(NLES6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
NLES6conf68 <- lapply(NLES6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
NLES6up95 <- lapply(1:13, function(x) NLES6conf95[[x]][2,2])
NLES6low95 <- lapply(1:13, function(x) NLES6conf95[[x]][2,1])
NLES6up68 <- lapply(1:13, function(x) NLES6conf68[[x]][2,2])
NLES6low68 <- lapply(1:13, function(x) NLES6conf68[[x]][2,1])
gammaNLESt <- lapply(summariesNLES6, function(x) x$coefficients[2,1])
names(gammaNLESt) <- paste("gammaNLESt", 0:h, sep = "")

# -- Cumulative multiplier
mNLEStc <- cumsum(betaNLESt) / cumsum(as.numeric(gammaNLESt)); as.numeric(mNLEStc)


# --- Effect of Spanish govt spending shock on Dutch exports

# -- Equation 5
lhsESNL50 <- (data6$rxNL - data6$l1.rxNL) / data6$l1.rxNL
lhsESNL5 <- lapply(1:h, function(x) (data6[, 170+x] - data6$l1.rxNL) / data6$l1.rxNL)
lhsESNL5 <- data.frame(lhsESNL5)
names(lhsESNL5) = paste("lhsESNL5", 1:h, sep = "")
data6 <- cbind(data6, lhsESNL50, lhsESNL5)
ESNL5 <- lapply(1:13, function(x) lm(data6[, 240+x] ~ shockES5 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc + shockDE5 + shockFR5 + shockIT5 + shockNL5, data = data6))
summariesESNL5 <- lapply(ESNL5, summary)
ESNL5conf95 <- lapply(ESNL5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ESNL5conf68 <- lapply(ESNL5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ESNL5up95 <- lapply(1:13, function(x) ESNL5conf95[[x]][2,2])
ESNL5low95 <- lapply(1:13, function(x) ESNL5conf95[[x]][2,1])
ESNL5up68 <- lapply(1:13, function(x) ESNL5conf68[[x]][2,2])
ESNL5low68 <- lapply(1:13, function(x) ESNL5conf68[[x]][2,1])
betaESNLt <- lapply(summariesESNL5, function(x) x$coefficients[2,1])
names(betaESNLt) <- paste("betaESNLt", 0:h, sep = "")

# -- Equation 6
lhsESNL60 <- (data6$rgES - data6$l1.rgES) / data6$l1.rxNL
lhsESNL6 <- lapply(1:h, function(x) (data6[, 84+x] - data6$l1.rgES) / data6$l1.rxNL)
lhsESNL6 <- data.frame(lhsESNL6)
names(lhsESNL6) = paste("lhsESNL6", 1:h, sep = "")
data6 <- cbind(data6, lhsESNL60, lhsESNL6)
ESNL6 <- lapply(1:13, function(x) lm(data6[, 253+x] ~ shockES3 + l1.debtES + l1.intES + l1.lrtrES + l1.lrgES + l1.lryESc + l2.debtES + l2.intES + l2.lrtrES + l2.lrgES + l2.lryESc + l3.debtES + l3.intES + l3.lrtrES + l3.lrgES + l3.lryESc + l4.debtES + l4.intES + l4.lrtrES + l4.lrgES + l4.lryESc + shockDE3 + shockNL3 + shockFR3 + shockIT3, data = data6))
summariesESNL6 <- lapply(ESNL6, summary)
ESNL6conf95 <- lapply(ESNL6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ESNL6conf68 <- lapply(ESNL6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ESNL6up95 <- lapply(1:13, function(x) ESNL6conf95[[x]][2,2])
ESNL6low95 <- lapply(1:13, function(x) ESNL6conf95[[x]][2,1])
ESNL6up68 <- lapply(1:13, function(x) ESNL6conf68[[x]][2,2])
ESNL6low68 <- lapply(1:13, function(x) ESNL6conf68[[x]][2,1])
gammaESNLt <- lapply(summariesESNL6, function(x) x$coefficients[2,1])
names(gammaESNLt) <- paste("gammaESNLt", 0:h, sep = "")

# -- Cumulative multiplier
mESNLtc <- cumsum(betaESNLt) / cumsum(as.numeric(gammaESNLt)); as.numeric(mESNLtc)
