# --- Effect of Dutch govt spending shock on French exports

# Data period: 1980q1-2018q4 and 1999q1-2018q4
# 95% and 68% confidence intervals
# h = 4, 8 and 12

# OLS with left-hand side in growth rates and 4 lags of x(t-1)

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers NL and FR v4 1.R')

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

data3$shockNL2 <- (data3$shockNL / unlist(l.rxNL[1])) / sd((data3$shockNL / unlist(l.rxNL[1])), na.rm = TRUE)
data3$shockNL3 <- data3$shockNL2 / 100
data3$shockFR2 <- (data3$shockFR / unlist(l.rxFR[1])) / sd((data3$shockFR / unlist(l.rxFR[1])), na.rm = TRUE)
data3$shockFR3 <- data3$shockFR2 / 100

shockFR5 <- data.frame(data3$shockFR2 * 100); names(shockFR5) <- "shockFR5"
shockNL5 <- data.frame(data3$shockNL2 * 100); names(shockNL5) <- "shockNL5"
shockDE5 <- data.frame(data3$shockDE2 * 100); names(shockDE5) <- "shockDE5"
shockES5 <- data.frame(data3$shockES2 * 100); names(shockES5) <- "shockES5"
shockIT5 <- data.frame(data3$shockIT2 * 100); names(shockIT5) <- "shockIT5"

data4 <- cbind(data3, shockFR5, shockNL5, shockDE5, shockIT5, shockES5, 
               l.rmNL, l.rxNL, rmNL.l, rxNL.l, l.rmFR, l.rxFR, rmFR.l, rxFR.l)
data5 <- subset(data4, select = -c(30:32, 35:37, 152:203))
h <- 12


# -- OLS regressions

# -- Equation 5
lhsNLFR50 <- (data5$rxFR - data5$l1.rxFR) / data5$l1.rxFR
lhsNLFR5 <- lapply(1:h, function(x) (data5[, 202+x] - data5$l1.rxFR) / data5$l1.rxFR)
lhsNLFR5 <- data.frame(lhsNLFR5)
names(lhsNLFR5) = paste("lhsNLFR5", 1:h, sep = "")
data6 <- cbind(data5, lhsNLFR50, lhsNLFR5)
NLFR5 <- lapply(1:13, function(x) lm(data6[, 214+x] ~ shockNL2 + l1.debtFR + l1.intFR + l1.lrtrFR + l1.lrgFR + l1.lryFRc + l2.debtFR + l2.intFR + l2.lrtrFR + l2.lrgFR + l2.lryFRc + l3.debtFR + l3.intFR + l3.lrtrFR + l3.lrgFR + l3.lryFRc + l4.debtFR + l4.intFR + l4.lrtrFR + l4.lrgFR + l4.lryFRc + shockDE2 + shockFR2 + shockES2 + shockIT2, data = data6))
summariesNLFR5 <- lapply(NLFR5, summary)
NLFR5conf95 <- lapply(NLFR5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
NLFR5conf68 <- lapply(NLFR5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
NLFR5up95 <- lapply(1:13, function(x) NLFR5conf95[[x]][2,2])
NLFR5low95 <- lapply(1:13, function(x) NLFR5conf95[[x]][2,1])
NLFR5up68 <- lapply(1:13, function(x) NLFR5conf68[[x]][2,2])
NLFR5low68 <- lapply(1:13, function(x) NLFR5conf68[[x]][2,1])
betaNLFRt <- lapply(summariesNLFR5, function(x) x$coefficients[2,1])
names(betaNLFRt) <- paste("betaNLFRt", 0:h, sep = "")

# -- Equation 6
lhsNLFR60 <- (data6$rgNL - data6$l1.rgNL) / data6$l1.rxFR
lhsNLFR6 <- lapply(1:h, function(x) (data6[, 96+x] - data6$l1.rgNL) / data6$l1.rxFR)
lhsNLFR6 <- data.frame(lhsNLFR6)
names(lhsNLFR6) = paste("lhsNLFR6", 1:h, sep = "")
data6 <- cbind(data6, lhsNLFR60, lhsNLFR6)
NLFR6 <- lapply(1:13, function(x) lm(data6[, 227+x] ~ shockNL3 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc + shockDE3 + shockFR3 + shockES3 + shockIT3, data = data6))
summariesNLFR6 <- lapply(NLFR6, summary)
NLFR6conf95 <- lapply(NLFR6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
NLFR6conf68 <- lapply(NLFR6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
NLFR6up95 <- lapply(1:13, function(x) NLFR6conf95[[x]][2,2])
NLFR6low95 <- lapply(1:13, function(x) NLFR6conf95[[x]][2,1])
NLFR6up68 <- lapply(1:13, function(x) NLFR6conf68[[x]][2,2])
NLFR6low68 <- lapply(1:13, function(x) NLFR6conf68[[x]][2,1])
gammaNLFRt <- lapply(summariesNLFR6, function(x) x$coefficients[2,1])
names(gammaNLFRt) <- paste("gammaNLFRt", 0:h, sep = "")

# -- Cumulative multiplier
mNLFRtc <- cumsum(betaNLFRt) / cumsum(as.numeric(gammaNLFRt)); as.numeric(mNLFRtc)


# --- Effect of French govt spending shock on Dutch exports

# -- Equation 5
lhsFRNL50 <- (data6$rxNL - data6$l1.rxNL) / data6$l1.rxNL
lhsFRNL5 <- lapply(1:h, function(x) (data6[, 170+x] - data6$l1.rxNL) / data6$l1.rxNL)
lhsFRNL5 <- data.frame(lhsFRNL5)
names(lhsFRNL5) = paste("lhsFRNL5", 1:h, sep = "")
data6 <- cbind(data6, lhsFRNL50, lhsFRNL5)
FRNL5 <- lapply(1:13, function(x) lm(data6[, 240+x] ~ shockFR5 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc + shockDE5 + shockES5 + shockIT5 + shockNL5, data = data6))
summariesFRNL5 <- lapply(FRNL5, summary)
FRNL5conf95 <- lapply(FRNL5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
FRNL5conf68 <- lapply(FRNL5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
FRNL5up95 <- lapply(1:13, function(x) FRNL5conf95[[x]][2,2])
FRNL5low95 <- lapply(1:13, function(x) FRNL5conf95[[x]][2,1])
FRNL5up68 <- lapply(1:13, function(x) FRNL5conf68[[x]][2,2])
FRNL5low68 <- lapply(1:13, function(x) FRNL5conf68[[x]][2,1])
betaFRNLt <- lapply(summariesFRNL5, function(x) x$coefficients[2,1])
names(betaFRNLt) <- paste("betaFRNLt", 0:h, sep = "")

# -- Equation 6
lhsFRNL60 <- (data6$rgFR - data6$l1.rgFR) / data6$l1.rxNL
lhsFRNL6 <- lapply(1:h, function(x) (data6[, 84+x] - data6$l1.rgFR) / data6$l1.rxNL)
lhsFRNL6 <- data.frame(lhsFRNL6)
names(lhsFRNL6) = paste("lhsFRNL6", 1:h, sep = "")
data6 <- cbind(data6, lhsFRNL60, lhsFRNL6)
FRNL6 <- lapply(1:13, function(x) lm(data6[, 253+x] ~ shockFR3 + l1.debtFR + l1.intFR + l1.lrtrFR + l1.lrgFR + l1.lryFRc + l2.debtFR + l2.intFR + l2.lrtrFR + l2.lrgFR + l2.lryFRc + l3.debtFR + l3.intFR + l3.lrtrFR + l3.lrgFR + l3.lryFRc + l4.debtFR + l4.intFR + l4.lrtrFR + l4.lrgFR + l4.lryFRc + shockDE3 + shockNL3 + shockES3 + shockIT3, data = data6))
summariesFRNL6 <- lapply(FRNL6, summary)
FRNL6conf95 <- lapply(FRNL6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
FRNL6conf68 <- lapply(FRNL6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
FRNL6up95 <- lapply(1:13, function(x) FRNL6conf95[[x]][2,2])
FRNL6low95 <- lapply(1:13, function(x) FRNL6conf95[[x]][2,1])
FRNL6up68 <- lapply(1:13, function(x) FRNL6conf68[[x]][2,2])
FRNL6low68 <- lapply(1:13, function(x) FRNL6conf68[[x]][2,1])
gammaFRNLt <- lapply(summariesFRNL6, function(x) x$coefficients[2,1])
names(gammaFRNLt) <- paste("gammaFRNLt", 0:h, sep = "")

# -- Cumulative multiplier
mFRNLtc <- cumsum(betaFRNLt) / cumsum(as.numeric(gammaFRNLt)); as.numeric(mFRNLtc)
