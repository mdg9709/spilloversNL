# --- Effect of Spanish govt spending shock on German exports

# Data period: 1980q1-2018q4
# 95% and 68% confidence intervals
# h = 4, 8 and 12

# OLS with left-hand side in growth rates and 4 lags of x(t-1)

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers ES and DE v4 1.R')

# Load packages
library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)
library(lmtest)
library(sandwich)

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
data3$shockES2 <- (data3$shockES / unlist(l.rxES[1])) / sd((data3$shockES / unlist(l.rxES[1])), na.rm = TRUE)
data3$shockES3 <- data3$shockES2 / 100

shockFR5 <- data.frame(data3$shockFR2 * 100); names(shockFR5) <- "shockFR5"
shockNL5 <- data.frame(data3$shockNL2 * 100); names(shockNL5) <- "shockNL5"
shockDE5 <- data.frame(data3$shockDE2 * 100); names(shockDE5) <- "shockDE5"
shockES5 <- data.frame(data3$shockES2 * 100); names(shockES5) <- "shockES5"
shockIT5 <- data.frame(data3$shockIT2 * 100); names(shockIT5) <- "shockIT5"

data4 <- cbind(data3, shockFR5, shockNL5, shockDE5, shockIT5, shockES5, 
               l.rmES, l.rxES, rmES.l, rxES.l, l.rmDE, l.rxDE, rmDE.l, rxDE.l)
data5 <- subset(data4, select = -c(30:32, 35:37, 153:204))
h <- 12

# -- OLS regressions

# -- Equation 5
lhsESDE50 <- (data5$rxDE - data5$l1.rxDE) / data5$l1.rxDE
lhsESDE5 <- lapply(1:h, function(x) (data5[, 203+x] - data5$l1.rxDE) / data5$l1.rxDE)
lhsESDE5 <- data.frame(lhsESDE5)
names(lhsESDE5) = paste("lhsESDE5", 1:h, sep = "")
data6 <- cbind(data5, lhsESDE50, lhsESDE5)
ESDE5 <- lapply(1:13, function(x) lm(data6[, 215+x] ~ shockES2 + l1.debtDE + l1.intDE + l1.lrtrDE + l1.lrgDE + l1.lryDEc + l2.debtDE + l2.intDE + l2.lrtrDE + l2.lrgDE + l2.lryDEc + l3.debtDE + l3.intDE + l3.lrtrDE + l3.lrgDE + l3.lryDEc + l4.debtDE + l4.intDE + l4.lrtrDE + l4.lrgDE + l4.lryDEc + shockDE2 + shockFR2 + shockNL2 + shockIT2, data = data6))
summariesESDE5 <- lapply(ESDE5, summary)
ESDE5conf95 <- lapply(ESDE5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ESDE5conf68 <- lapply(ESDE5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ESDE5up95 <- lapply(1:13, function(x) ESDE5conf95[[x]][2,2])
ESDE5low95 <- lapply(1:13, function(x) ESDE5conf95[[x]][2,1])
ESDE5up68 <- lapply(1:13, function(x) ESDE5conf68[[x]][2,2])
ESDE5low68 <- lapply(1:13, function(x) ESDE5conf68[[x]][2,1])
betaESDEt <- lapply(summariesESDE5, function(x) x$coefficients[2,1])
names(betaESDEt) <- paste("betaESDEt", 0:h, sep = "")

# -- Equation 6
lhsESDE60 <- (data6$rgES - data6$l1.rgES) / data6$l1.rxDE
lhsESDE6 <- lapply(1:h, function(x) (data6[, 96+x] - data6$l1.rgES) / data6$l1.rxDE)
lhsESDE6 <- data.frame(lhsESDE6)
names(lhsESDE6) = paste("lhsESDE6", 1:h, sep = "")
data6 <- cbind(data6, lhsESDE60, lhsESDE6)
ESDE6 <- lapply(1:13, function(x) lm(data6[, 228+x] ~ shockES3 + l1.debtES + l1.intES + l1.lrtrES + l1.lrgES + l1.lryESc + l2.debtES + l2.intES + l2.lrtrES + l2.lrgES + l2.lryESc + l3.debtES + l3.intES + l3.lrtrES + l3.lrgES + l3.lryESc + l4.debtES + l4.intES + l4.lrtrES + l4.lrgES + l4.lryESc + shockDE3 + shockFR3 + shockNL3 + shockIT3, data = data6))
summariesESDE6 <- lapply(ESDE6, summary)
ESDE6conf95 <- lapply(ESDE6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ESDE6conf68 <- lapply(ESDE6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ESDE6up95 <- lapply(1:13, function(x) ESDE6conf95[[x]][2,2])
ESDE6low95 <- lapply(1:13, function(x) ESDE6conf95[[x]][2,1])
ESDE6up68 <- lapply(1:13, function(x) ESDE6conf68[[x]][2,2])
ESDE6low68 <- lapply(1:13, function(x) ESDE6conf68[[x]][2,1])
gammaESDEt <- lapply(summariesESDE6, function(x) x$coefficients[2,1])
names(gammaESDEt) <- paste("gammaESDEt", 0:h, sep = "")

# -- Cumulative multiplier
mESDEtc <- cumsum(betaESDEt) / cumsum(as.numeric(gammaESDEt)); as.numeric(mESDEtc)


# --- Effect of German govt spending shock on Spanish exports

# -- Equation 5
lhsDEES50 <- (data6$rxES - data6$l1.rxES) / data6$l1.rxES
lhsDEES5 <- lapply(1:h, function(x) (data6[, 171+x] - data6$l1.rxES) / data6$l1.rxES)
lhsDEES5 <- data.frame(lhsDEES5)
names(lhsDEES5) = paste("lhsDEES5", 1:h, sep = "")
data6 <- cbind(data6, lhsDEES50, lhsDEES5)
DEES5 <- lapply(1:13, function(x) lm(data6[, 241+x] ~ shockDE5 + l1.debtES + l1.intES + l1.lrtrES + l1.lrgES + l1.lryESc + l2.debtES + l2.intES + l2.lrtrES + l2.lrgES + l2.lryESc + l3.debtES + l3.intES + l3.lrtrES + l3.lrgES + l3.lryESc + l4.debtES + l4.intES + l4.lrtrES + l4.lrgES + l4.lryESc + shockFR5 + shockES5 + shockIT5 + shockNL5, data = data6))
summariesDEES5 <- lapply(DEES5, summary)
DEES5conf95 <- lapply(DEES5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
DEES5conf68 <- lapply(DEES5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
DEES5up95 <- lapply(1:13, function(x) DEES5conf95[[x]][2,2])
DEES5low95 <- lapply(1:13, function(x) DEES5conf95[[x]][2,1])
DEES5up68 <- lapply(1:13, function(x) DEES5conf68[[x]][2,2])
DEES5low68 <- lapply(1:13, function(x) DEES5conf68[[x]][2,1])
betaDEESt <- lapply(summariesDEES5, function(x) x$coefficients[2,1])
names(betaDEESt) <- paste("betaDEESt", 0:h, sep = "")

# -- Equation 6
lhsDEES60 <- (data6$rgDE - data6$l1.rgDE) / data6$l1.rxES
lhsDEES6 <- lapply(1:h, function(x) (data6[, 84+x] - data6$l1.rgDE) / data6$l1.rxES)
lhsDEES6 <- data.frame(lhsDEES6)
names(lhsDEES6) = paste("lhsDEES6", 1:h, sep = "")
data6 <- cbind(data6, lhsDEES60, lhsDEES6)
DEES6 <- lapply(1:13, function(x) lm(data6[, 254+x] ~ shockDE3 + l1.debtDE + l1.intDE + l1.lrtrDE + l1.lrgDE + l1.lryDEc + l2.debtDE + l2.intDE + l2.lrtrDE + l2.lrgDE + l2.lryDEc + l3.debtDE + l3.intDE + l3.lrtrDE + l3.lrgDE + l3.lryDEc + l4.debtDE + l4.intDE + l4.lrtrDE + l4.lrgDE + l4.lryDEc + shockNL3 + shockFR3 + shockES3 + shockIT3, data = data6))
summariesDEES6 <- lapply(DEES6, summary)
DEES6conf95 <- lapply(DEES6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
DEES6conf68 <- lapply(DEES6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
DEES6up95 <- lapply(1:13, function(x) DEES6conf95[[x]][2,2])
DEES6low95 <- lapply(1:13, function(x) DEES6conf95[[x]][2,1])
DEES6up68 <- lapply(1:13, function(x) DEES6conf68[[x]][2,2])
DEES6low68 <- lapply(1:13, function(x) DEES6conf68[[x]][2,1])
gammaDEESt <- lapply(summariesDEES6, function(x) x$coefficients[2,1])
names(gammaDEESt) <- paste("gammaDEESt", 0:h, sep = "")

# -- Cumulative multiplier
mDEEStc <- cumsum(betaDEESt) / cumsum(as.numeric(gammaDEESt)); as.numeric(mDEEStc)
