# --- Effect of Spanish govt spending shock on Italian exports

# Data period: 1980q1-2018q4
# 95% and 68% confidence intervals
# h = 4, 8 and 12

# OLS with left-hand side in growth rates and 4 lags of x(t-1)

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers IT and ES v4 1.R')

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

data3$shockIT2 <- (data3$shockIT / unlist(l.rxIT[1])) / sd((data3$shockIT / unlist(l.rxIT[1])), na.rm = TRUE)
data3$shockIT3 <- data3$shockIT2 / 100
data3$shockES2 <- (data3$shockES / unlist(l.rxES[1])) / sd((data3$shockES / unlist(l.rxES[1])), na.rm = TRUE)
data3$shockES3 <- data3$shockES2 / 100

shockFR5 <- data.frame(data3$shockFR2 * 100); names(shockFR5) <- "shockFR5"
shockNL5 <- data.frame(data3$shockNL2 * 100); names(shockNL5) <- "shockNL5"
shockDE5 <- data.frame(data3$shockDE2 * 100); names(shockDE5) <- "shockDE5"
shockES5 <- data.frame(data3$shockES2 * 100); names(shockES5) <- "shockES5"
shockIT5 <- data.frame(data3$shockIT2 * 100); names(shockIT5) <- "shockIT5"

data4 <- cbind(data3, shockFR5, shockNL5, shockDE5, shockIT5, shockES5, 
               l.rmES, l.rxES, rmES.l, rxES.l, l.rmIT, l.rxIT, rmIT.l, rxIT.l)
data5 <- subset(data4, select = -c(30:32, 35:37, 153:204))
h <- 12

# -- OLS regressions

# -- Equation 5
lhsESIT50 <- (data5$rxIT - data5$l1.rxIT) / data5$rxIT
lhsESIT5 <- lapply(1:h, function(x) (data5[, 171+x] - data5$l1.rxIT) / data5$l1.rxIT)
lhsESIT5 <- data.frame(lhsESIT5)
names(lhsESIT5) = paste("lhsESIT5", 1:h, sep = "")
data6 <- cbind(data5, lhsESIT50, lhsESIT5)
ESIT5 <- lapply(1:13, function(x) lm(data6[, 215+x] ~ shockES2 + l1.debtIT + l1.intIT + l1.lrtrIT + l1.lrgIT + l1.lryITc + l2.debtIT + l2.intIT + l2.lrtrIT + l2.lrgIT + l2.lryITc + l3.debtIT + l3.intIT + l3.lrtrIT + l3.lrgIT + l3.lryITc + l4.debtIT + l4.intIT + l4.lrtrIT + l4.lrgIT + l4.lryITc + shockDE2 + shockIT2 + shockNL2 + shockFR2, data = data6))
summariesESIT5 <- lapply(ESIT5, summary)
ESIT5conf95 <- lapply(ESIT5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ESIT5conf68 <- lapply(ESIT5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ESIT5up95 <- lapply(1:13, function(x) ESIT5conf95[[x]][2,2])
ESIT5low95 <- lapply(1:13, function(x) ESIT5conf95[[x]][2,1])
ESIT5up68 <- lapply(1:13, function(x) ESIT5conf68[[x]][2,2])
ESIT5low68 <- lapply(1:13, function(x) ESIT5conf68[[x]][2,1])
betaESITt <- lapply(summariesESIT5, function(x) x$coefficients[2,1])
names(betaESITt) <- paste("betaESITt", 0:h, sep = "")

# -- Equation 6
lhsESIT60 <- (data6$rgES - data6$l1.rgES) / data6$l1.rxIT
lhsESIT6 <- lapply(1:h, function(x) (data6[, 96+x] - data6$l1.rgES) / data6$l1.rxIT)
lhsESIT6 <- data.frame(lhsESIT6)
names(lhsESIT6) = paste("lhsESIT6", 1:h, sep = "")
data6 <- cbind(data6, lhsESIT60, lhsESIT6)
ESIT6 <- lapply(1:13, function(x) lm(data6[, 228+x] ~ shockES3 + l1.debtES + l1.intES + l1.lrtrES + l1.lrgES + l1.lryESc + l2.debtES + l2.intES + l2.lrtrES + l2.lrgES + l2.lryESc + l3.debtES + l3.intES + l3.lrtrES + l3.lrgES + l3.lryESc + l4.debtES + l4.intES + l4.lrtrES + l4.lrgES + l4.lryESc + shockDE3 + shockFR3 + shockNL3 + shockIT3, data = data6))
summariesESIT6 <- lapply(ESIT6, summary)
ESIT6conf95 <- lapply(ESIT6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ESIT6conf68 <- lapply(ESIT6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ESIT6up95 <- lapply(1:13, function(x) ESIT6conf95[[x]][2,2])
ESIT6low95 <- lapply(1:13, function(x) ESIT6conf95[[x]][2,1])
ESIT6up68 <- lapply(1:13, function(x) ESIT6conf68[[x]][2,2])
ESIT6low68 <- lapply(1:13, function(x) ESIT6conf68[[x]][2,1])
gammaESITt <- lapply(summariesESIT6, function(x) x$coefficients[2,1])
names(gammaESITt) <- paste("gammaESITt", 0:h, sep = "")

# -- Cumulative multiplier
mESITtc <- cumsum(betaESITt) / cumsum(as.numeric(gammaESITt)); as.numeric(mESITtc)


# --- Effect of Italian govt spending shock on Spanish exports

# -- Equation 5
lhsITES50 <- (data6$rxES - data6$l1.rxES) / data6$l1.rxES
lhsITES5 <- lapply(1:h, function(x) (data6[, 203+x] - data6$l1.rxES) / data6$l1.rxES)
lhsITES5 <- data.frame(lhsITES5)
names(lhsITES5) = paste("lhsITES5", 1:h, sep = "")
data6 <- cbind(data6, lhsITES50, lhsITES5)
ITES5 <- lapply(1:13, function(x) lm(data6[, 241+x] ~ shockIT2 + l1.debtES + l1.intES + l1.lrtrES + l1.lrgES + l1.lryESc + l2.debtES + l2.intES + l2.lrtrES + l2.lrgES + l2.lryESc + l3.debtES + l3.intES + l3.lrtrES + l3.lrgES + l3.lryESc + l4.debtES + l4.intES + l4.lrtrES + l4.lrgES + l4.lryESc + shockNL2 + shockDE2 + shockFR2 + shockES2, data = data6))
summariesITES5 <- lapply(ITES5, summary)
ITES5conf95 <- lapply(ITES5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ITES5conf68 <- lapply(ITES5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ITES5up95 <- lapply(1:13, function(x) ITES5conf95[[x]][2,2])
ITES5low95 <- lapply(1:13, function(x) ITES5conf95[[x]][2,1])
ITES5up68 <- lapply(1:13, function(x) ITES5conf68[[x]][2,2])
ITES5low68 <- lapply(1:13, function(x) ITES5conf68[[x]][2,1])
betaITESt <- lapply(summariesITES5, function(x) x$coefficients[2,1])
names(betaITESt) <- paste("betaITESt", 0:h, sep = "")

# -- Equation 6
lhsITES60 <- (data6$rgIT - data6$l1.rgIT) / data6$l1.rxES
lhsITES6 <- lapply(1:h, function(x) (data6[, 84+x] - data6$l1.rgIT) / data6$l1.rxES)
lhsITES6 <- data.frame(lhsITES6)
names(lhsITES6) = paste("lhsITES6", 1:h, sep = "")
data6 <- cbind(data6, lhsITES60, lhsITES6)
ITES6 <- lapply(1:13, function(x) lm(data6[, 254+x] ~ shockIT3 + l1.debtIT + l1.intIT + l1.lrtrIT + l1.lrgIT + l1.lryITc + l2.debtIT + l2.intIT + l2.lrtrIT + l2.lrgIT + l2.lryITc + l3.debtIT + l3.intIT + l3.lrtrIT + l3.lrgIT + l3.lryITc + l4.debtIT + l4.intIT + l4.lrtrIT + l4.lrgIT + l4.lryITc + shockDE3 + shockNL3 + shockFR3 + shockES3, data = data6))
summariesITES6 <- lapply(ITES6, summary)
ITES6conf95 <- lapply(ITES6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ITES6conf68 <- lapply(ITES6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ITES6up95 <- lapply(1:13, function(x) ITES6conf95[[x]][2,2])
ITES6low95 <- lapply(1:13, function(x) ITES6conf95[[x]][2,1])
ITES6up68 <- lapply(1:13, function(x) ITES6conf68[[x]][2,2])
ITES6low68 <- lapply(1:13, function(x) ITES6conf68[[x]][2,1])
gammaITESt <- lapply(summariesITES6, function(x) x$coefficients[2,1])
names(gammaITESt) <- paste("gammaITESt", 0:h, sep = "")

# -- Cumulative multiplier
mITEStc <- cumsum(betaITESt) / cumsum(as.numeric(gammaITESt)); as.numeric(mITEStc)
