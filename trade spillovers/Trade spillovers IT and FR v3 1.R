# --- Effect of Italian govt spending shock on French exports

# Data period: 1980q1-2018q4
# 95% and 68% confidence intervals
# h = 4, 8 and 12

# OLS with left-hand side in growth rates and 4 lags of x(t-1)

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers FR and IT v4 1.R')

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

data3$shockFR2 <- (data3$shockFR / unlist(l.rxFR[1])) / sd((data3$shockFR / unlist(l.rxFR[1])), na.rm = TRUE)
data3$shockFR3 <- data3$shockFR2 / 100
data3$shockIT2 <- (data3$shockIT / unlist(l.rxIT[1])) / sd((data3$shockIT / unlist(l.rxIT[1])), na.rm = TRUE)
data3$shockIT3 <- data3$shockIT2 / 100

shockFR5 <- data.frame(data3$shockFR2 * 100); names(shockFR5) <- "shockFR5"
shockNL5 <- data.frame(data3$shockNL2 * 100); names(shockNL5) <- "shockNL5"
shockDE5 <- data.frame(data3$shockDE2 * 100); names(shockDE5) <- "shockDE5"
shockES5 <- data.frame(data3$shockES2 * 100); names(shockES5) <- "shockES5"
shockIT5 <- data.frame(data3$shockIT2 * 100); names(shockIT5) <- "shockIT5"

data4 <- cbind(data3, shockFR5, shockNL5, shockDE5, shockIT5, shockES5, 
               l.rmIT, l.rxIT, rmIT.l, rxIT.l, l.rmFR, l.rxFR, rmFR.l, rxFR.l)
data5 <- subset(data4, select = -c(30:32, 35:37, 153:204))
h <- 12


# -- OLS regressions

# -- Equation 5
lhsFRIT50 <- (data5$rxIT - data5$l1.rxIT) / data5$rxIT
lhsFRIT5 <- lapply(1:h, function(x) (data5[, 171+x] - data5$l1.rxIT) / data5$l1.rxIT)
lhsFRIT5 <- data.frame(lhsFRIT5)
names(lhsFRIT5) = paste("lhsFRIT5", 1:h, sep = "")
data6 <- cbind(data5, lhsFRIT50, lhsFRIT5)
FRIT5 <- lapply(1:13, function(x) lm(data6[, 215+x] ~ shockFR2 + l1.debtIT + l1.intIT + l1.lrtrIT + l1.lrgIT + l1.lryITc + l2.debtIT + l2.intIT + l2.lrtrIT + l2.lrgIT + l2.lryITc + l3.debtIT + l3.intIT + l3.lrtrIT + l3.lrgIT + l3.lryITc + l4.debtIT + l4.intIT + l4.lrtrIT + l4.lrgIT + l4.lryITc + shockDE2 + shockIT2 + shockES2 + shockNL2, data = data6))
summariesFRIT5 <- lapply(FRIT5, summary)
FRIT5conf95 <- lapply(FRIT5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
FRIT5conf68 <- lapply(FRIT5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
FRIT5up95 <- lapply(1:13, function(x) FRIT5conf95[[x]][2,2])
FRIT5low95 <- lapply(1:13, function(x) FRIT5conf95[[x]][2,1])
FRIT5up68 <- lapply(1:13, function(x) FRIT5conf68[[x]][2,2])
FRIT5low68 <- lapply(1:13, function(x) FRIT5conf68[[x]][2,1])
betaFRITt <- lapply(summariesFRIT5, function(x) x$coefficients[2,1])
names(betaFRITt) <- paste("betaFRITt", 0:h, sep = "")

# -- Equation 6
lhsFRIT60 <- (data6$rgFR - data6$l1.rgFR) / data6$l1.rxIT
lhsFRIT6 <- lapply(1:h, function(x) (data6[, 96+x] - data6$l1.rgFR) / data6$l1.rxIT)
lhsFRIT6 <- data.frame(lhsFRIT6)
names(lhsFRIT6) = paste("lhsFRIT6", 1:h, sep = "")
data6 <- cbind(data6, lhsFRIT60, lhsFRIT6)
FRIT6 <- lapply(1:13, function(x) lm(data6[, 228+x] ~ shockFR3 + l1.debtFR + l1.intFR + l1.lrtrFR + l1.lrgFR + l1.lryFRc + l2.debtFR + l2.intFR + l2.lrtrFR + l2.lrgFR + l2.lryFRc + l3.debtFR + l3.intFR + l3.lrtrFR + l3.lrgFR + l3.lryFRc + l4.debtFR + l4.intFR + l4.lrtrFR + l4.lrgFR + l4.lryFRc + shockDE3 + shockNL3 + shockES3 + shockIT3, data = data6))
summariesFRIT6 <- lapply(FRIT6, summary)
FRIT6conf95 <- lapply(FRIT6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
FRIT6conf68 <- lapply(FRIT6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
FRIT6up95 <- lapply(1:13, function(x) FRIT6conf95[[x]][2,2])
FRIT6low95 <- lapply(1:13, function(x) FRIT6conf95[[x]][2,1])
FRIT6up68 <- lapply(1:13, function(x) FRIT6conf68[[x]][2,2])
FRIT6low68 <- lapply(1:13, function(x) FRIT6conf68[[x]][2,1])
gammaFRITt <- lapply(summariesFRIT6, function(x) x$coefficients[2,1])
names(gammaFRITt) <- paste("gammaFRITt", 0:h, sep = "")

# -- Cumulative multiplier
mFRITtc <- cumsum(betaFRITt) / cumsum(as.numeric(gammaFRITt)); as.numeric(mFRITtc)


# --- Effect of Italian govt spending shock on French exports

# -- Equation 5
lhsITFR50 <- (data6$rxFR - data6$l1.rxFR) / data6$l1.rxFR
lhsITFR5 <- lapply(1:h, function(x) (data6[, 203+x] - data6$l1.rxFR) / data6$l1.rxFR)
lhsITFR5 <- data.frame(lhsITFR5)
names(lhsITFR5) = paste("lhsITFR5", 1:h, sep = "")
data6 <- cbind(data6, lhsITFR50, lhsITFR5)
ITFR5 <- lapply(1:13, function(x) lm(data6[, 241+x] ~ shockIT2 + l1.debtFR + l1.intFR + l1.lrtrFR + l1.lrgFR + l1.lryFRc + l2.debtFR + l2.intFR + l2.lrtrFR + l2.lrgFR + l2.lryFRc + l3.debtFR + l3.intFR + l3.lrtrFR + l3.lrgFR + l3.lryFRc + l4.debtFR + l4.intFR + l4.lrtrFR + l4.lrgFR + l4.lryFRc + shockFR2 + shockDE2 + shockNL2 + shockES2, data = data6))
summariesITFR5 <- lapply(ITFR5, summary)
ITFR5conf95 <- lapply(ITFR5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ITFR5conf68 <- lapply(ITFR5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ITFR5up95 <- lapply(1:13, function(x) ITFR5conf95[[x]][2,2])
ITFR5low95 <- lapply(1:13, function(x) ITFR5conf95[[x]][2,1])
ITFR5up68 <- lapply(1:13, function(x) ITFR5conf68[[x]][2,2])
ITFR5low68 <- lapply(1:13, function(x) ITFR5conf68[[x]][2,1])
betaITFRt <- lapply(summariesITFR5, function(x) x$coefficients[2,1])
names(betaITFRt) <- paste("betaITFRt", 0:h, sep = "")

# -- Equation 6
lhsITFR60 <- (data6$rgIT - data6$l1.rgIT) / data6$l1.rxFR
lhsITFR6 <- lapply(1:h, function(x) (data6[, 84+x] - data6$l1.rgIT) / data6$l1.rxFR)
lhsITFR6 <- data.frame(lhsITFR6)
names(lhsITFR6) = paste("lhsITFR6", 1:h, sep = "")
data6 <- cbind(data6, lhsITFR60, lhsITFR6)
ITFR6 <- lapply(1:13, function(x) lm(data6[, 254+x] ~ shockIT3 + l1.debtIT + l1.intIT + l1.lrtrIT + l1.lrgIT + l1.lryITc + l2.debtIT + l2.intIT + l2.lrtrIT + l2.lrgIT + l2.lryITc + l3.debtIT + l3.intIT + l3.lrtrIT + l3.lrgIT + l3.lryITc + l4.debtIT + l4.intIT + l4.lrtrIT + l4.lrgIT + l4.lryITc + shockDE3 + shockNL3 + shockFR3 + shockES3, data = data6))
summariesITFR6 <- lapply(ITFR6, summary)
ITFR6conf95 <- lapply(ITFR6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ITFR6conf68 <- lapply(ITFR6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ITFR6up95 <- lapply(1:13, function(x) ITFR6conf95[[x]][2,2])
ITFR6low95 <- lapply(1:13, function(x) ITFR6conf95[[x]][2,1])
ITFR6up68 <- lapply(1:13, function(x) ITFR6conf68[[x]][2,2])
ITFR6low68 <- lapply(1:13, function(x) ITFR6conf68[[x]][2,1])
gammaITFRt <- lapply(summariesITFR6, function(x) x$coefficients[2,1])
names(gammaITFRt) <- paste("gammaITFRt", 0:h, sep = "")

# -- Cumulative multiplier
mITFRtc <- cumsum(betaITFRt) / cumsum(as.numeric(gammaITFRt)); as.numeric(mITFRtc)
