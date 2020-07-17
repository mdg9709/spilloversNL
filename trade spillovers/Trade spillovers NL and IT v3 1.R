# --- Effect of Dutch govt spending shock on Italian exports

# Data period: 1980q1-2018q4 and 1999q1-2018q4
# 95% and 68% confidence intervals
# h = 4, 8 and 12

# OLS with left-hand side in growth rates and 4 lags of x(t-1)

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers NL and IT v4 1.R')

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

data3$shockNL2 <- (data3$shockNL / unlist(l.rxNL[1])) / sd((data3$shockNL / unlist(l.rxNL[1])), na.rm = TRUE)
data3$shockNL3 <- data3$shockNL2 / 100
data3$shockIT2 <- (data3$shockIT / unlist(l.rxIT[1])) / sd((data3$shockIT / unlist(l.rxIT[1])), na.rm = TRUE)
data3$shockIT3 <- data3$shockIT2 / 100

data4 <- cbind(data3, l.rmNL, l.rxNL, rmNL.l, rxNL.l, l.rmIT, l.rxIT, rmIT.l, rxIT.l)
data5 <- subset(data4, select = -c(30:32, 35:37, 152:203))
h <- 12

# -- OLS regressions

# -- Equation 5
lhsNLIT50 <- (data5$rxIT - data5$l1.rxIT) / data5$rxIT
lhsNLIT5 <- lapply(1:h, function(x) (data5[, 197+x] - data5$l1.rxIT) / data5$l1.rxIT)
lhsNLIT5 <- data.frame(lhsNLIT5)
names(lhsNLIT5) = paste("lhsNLIT5", 1:h, sep = "")
data6 <- cbind(data5, lhsNLIT50, lhsNLIT5)
NLIT5 <- lapply(1:13, function(x) lm(data6[, 209+x] ~ shockNL2 + l1.debtIT + l1.intIT + l1.lrtrIT + l1.lrgIT + l1.lryITc + l2.debtIT + l2.intIT + l2.lrtrIT + l2.lrgIT + l2.lryITc + l3.debtIT + l3.intIT + l3.lrtrIT + l3.lrgIT + l3.lryITc + l4.debtIT + l4.intIT + l4.lrtrIT + l4.lrgIT + l4.lryITc + shockDE2 + shockIT2 + shockES2 + shockFR2, data = data6))
summariesNLIT5 <- lapply(NLIT5, summary)
NLIT5conf95 <- lapply(NLIT5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
NLIT5conf68 <- lapply(NLIT5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
NLIT5up95 <- lapply(1:13, function(x) NLIT5conf95[[x]][2,2])
NLIT5low95 <- lapply(1:13, function(x) NLIT5conf95[[x]][2,1])
NLIT5up68 <- lapply(1:13, function(x) NLIT5conf68[[x]][2,2])
NLIT5low68 <- lapply(1:13, function(x) NLIT5conf68[[x]][2,1])
betaNLITt <- lapply(summariesNLIT5, function(x) x$coefficients[2,1])
names(betaNLITt) <- paste("betaNLITt", 0:h, sep = "")

# -- Equation 6
lhsNLIT60 <- (data6$rgNL - data6$l1.rgNL) / data6$l1.rxIT
lhsNLIT6 <- lapply(1:h, function(x) (data6[, 96+x] - data6$l1.rgNL) / data6$l1.rxIT)
lhsNLIT6 <- data.frame(lhsNLIT6)
names(lhsNLIT6) = paste("lhsNLIT6", 1:h, sep = "")
data6 <- cbind(data6, lhsNLIT60, lhsNLIT6)
NLIT6 <- lapply(1:13, function(x) lm(data6[, 222+x] ~ shockNL3 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc + shockDE3 + shockFR3 + shockES3 + shockIT3, data = data6))
summariesNLIT6 <- lapply(NLIT6, summary)
NLIT6conf95 <- lapply(NLIT6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
NLIT6conf68 <- lapply(NLIT6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
NLIT6up95 <- lapply(1:13, function(x) NLIT6conf95[[x]][2,2])
NLIT6low95 <- lapply(1:13, function(x) NLIT6conf95[[x]][2,1])
NLIT6up68 <- lapply(1:13, function(x) NLIT6conf68[[x]][2,2])
NLIT6low68 <- lapply(1:13, function(x) NLIT6conf68[[x]][2,1])
gammaNLITt <- lapply(summariesNLIT6, function(x) x$coefficients[2,1])
names(gammaNLITt) <- paste("gammaNLITt", 0:h, sep = "")

# -- Cumulative multiplier
mNLITtc <- cumsum(betaNLITt) / cumsum(as.numeric(gammaNLITt)); as.numeric(mNLITtc)


# --- Effect of Italian govt spending shock on Dutch exports

# -- Equation 5
lhsITNL50 <- (data6$rxNL - data6$l1.rxNL) / data6$l1.rxNL
lhsITNL5 <- lapply(1:h, function(x) (data6[, 165+x] - data6$l1.rxNL) / data6$l1.rxNL)
lhsITNL5 <- data.frame(lhsITNL5)
names(lhsITNL5) = paste("lhsITNL5", 1:h, sep = "")
data6 <- cbind(data6, lhsITNL50, lhsITNL5)
ITNL5 <- lapply(1:13, function(x) lm(data6[, 235+x] ~ shockIT2 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc + shockNL2 + shockDE2 + shockFR2 + shockES2, data = data6))
summariesITNL5 <- lapply(ITNL5, summary)
ITNL5conf95 <- lapply(ITNL5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ITNL5conf68 <- lapply(ITNL5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ITNL5up95 <- lapply(1:13, function(x) ITNL5conf95[[x]][2,2])
ITNL5low95 <- lapply(1:13, function(x) ITNL5conf95[[x]][2,1])
ITNL5up68 <- lapply(1:13, function(x) ITNL5conf68[[x]][2,2])
ITNL5low68 <- lapply(1:13, function(x) ITNL5conf68[[x]][2,1])
betaITNLt <- lapply(summariesITNL5, function(x) x$coefficients[2,1])
names(betaITNLt) <- paste("betaITNLt", 0:h, sep = "")

# -- Equation 6
lhsITNL60 <- (data6$rgIT - data6$l1.rgIT) / data6$l1.rxNL
lhsITNL6 <- lapply(1:h, function(x) (data6[, 84+x] - data6$l1.rgIT) / data6$l1.rxNL)
lhsITNL6 <- data.frame(lhsITNL6)
names(lhsITNL6) = paste("lhsITNL6", 1:h, sep = "")
data6 <- cbind(data6, lhsITNL60, lhsITNL6)
ITNL6 <- lapply(1:13, function(x) lm(data6[, 248+x] ~ shockIT3 + l1.debtIT + l1.intIT + l1.lrtrIT + l1.lrgIT + l1.lryITc + l2.debtIT + l2.intIT + l2.lrtrIT + l2.lrgIT + l2.lryITc + l3.debtIT + l3.intIT + l3.lrtrIT + l3.lrgIT + l3.lryITc + l4.debtIT + l4.intIT + l4.lrtrIT + l4.lrgIT + l4.lryITc + shockDE3 + shockNL3 + shockFR3 + shockES3, data = data6))
summariesITNL6 <- lapply(ITNL6, summary)
ITNL6conf95 <- lapply(ITNL6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ITNL6conf68 <- lapply(ITNL6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ITNL6up95 <- lapply(1:13, function(x) ITNL6conf95[[x]][2,2])
ITNL6low95 <- lapply(1:13, function(x) ITNL6conf95[[x]][2,1])
ITNL6up68 <- lapply(1:13, function(x) ITNL6conf68[[x]][2,2])
ITNL6low68 <- lapply(1:13, function(x) ITNL6conf68[[x]][2,1])
gammaITNLt <- lapply(summariesITNL6, function(x) x$coefficients[2,1])
names(gammaITNLt) <- paste("gammaITNLt", 0:h, sep = "")

# -- Cumulative multiplier
mITNLtc <- cumsum(betaITNLt) / cumsum(as.numeric(gammaITNLt)); as.numeric(mITNLtc)
