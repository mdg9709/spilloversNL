# --- Effect of Dutch govt spending shock on German exports

# Data period:1980q1-2018q4 and 1999q1-2018q4
# 95% and 68% confidence intervals
# h = 4, 8 and 12

# OLS with left-hand side in growth rates, 4 lags of x(t-1), and shock2 * (100)^3

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers NL and DE v4 1.R')

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

data3$shockNL2 <- (data3$shockNL / unlist(l.rxNL[1])) / sd((data3$shockNL / unlist(l.rxNL[1])), na.rm = TRUE)
data3$shockNL3 <- data3$shockNL2 / 100
data3$shockDE2 <- (data3$shockDE / unlist(l.rxDE[1])) / sd((data3$shockDE / unlist(l.rxDE[1])), na.rm = TRUE)
data3$shockDE3 <- data3$shockDE2 / 100

shockFR5 <- data.frame(data3$shockFR2 * 100); names(shockFR5) <- "shockFR5"
shockNL5 <- data.frame(data3$shockNL2 * 100); names(shockNL5) <- "shockNL5"
shockDE5 <- data.frame(data3$shockDE2 * 100); names(shockDE5) <- "shockDE5"
shockES5 <- data.frame(data3$shockES2 * 100); names(shockES5) <- "shockES5"
shockIT5 <- data.frame(data3$shockIT2 * 100); names(shockIT5) <- "shockIT5"

data4 <- cbind(data3, shockFR5, shockNL5, shockDE5, shockIT5, shockES5, 
               l.rmNL, l.rxNL, rmNL.l, rxNL.l, l.rmDE, l.rxDE, rmDE.l, rxDE.l)
data5 <- subset(data4, select = -c(30:32, 35:37, 152:203))
h <- 12


# -- OLS regressions

# -- Equation 5
lhsNLDE50 <- (data5$rxDE - data5$l1.rxDE) / data5$l1.rxDE
lhsNLDE5 <- lapply(1:h, function(x) (data5[, 202+x] - data5$l1.rxDE) / data5$l1.rxDE)
lhsNLDE5 <- data.frame(lhsNLDE5)
names(lhsNLDE5) = paste("lhsNLDE5", 1:h, sep = "")
data6 <- cbind(data5, lhsNLDE50, lhsNLDE5)
NLDE5 <- lapply(1:13, function(x) lm(data6[, 214+x] ~ shockNL2 + l1.debtDE + l1.intDE + l1.lrtrDE + l1.lrgDE + l1.lryDEc + l2.debtDE + l2.intDE + l2.lrtrDE + l2.lrgDE + l2.lryDEc + l3.debtDE + l3.intDE + l3.lrtrDE + l3.lrgDE + l3.lryDEc + l4.debtDE + l4.intDE + l4.lrtrDE + l4.lrgDE + l4.lryDEc + shockDE2 + shockFR2 + shockES2 + shockIT2, data = data6))
summariDENLDE5 <- lapply(NLDE5, summary)
NLDE5conf95 <- lapply(NLDE5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
NLDE5conf68 <- lapply(NLDE5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
NLDE5up95 <- lapply(1:13, function(x) NLDE5conf95[[x]][2,2])
NLDE5low95 <- lapply(1:13, function(x) NLDE5conf95[[x]][2,1])
NLDE5up68 <- lapply(1:13, function(x) NLDE5conf68[[x]][2,2])
NLDE5low68 <- lapply(1:13, function(x) NLDE5conf68[[x]][2,1])
betaNLDEt <- lapply(summariDENLDE5, function(x) x$coefficients[2,1])
names(betaNLDEt) <- paste("betaNLDEt", 0:h, sep = "")

# -- Equation 6
lhsNLDE60 <- (data6$rgNL - data6$l1.rgNL) / data6$l1.rxDE
lhsNLDE6 <- lapply(1:h, function(x) (data6[, 96+x] - data6$l1.rgNL) / data6$l1.rxDE)
lhsNLDE6 <- data.frame(lhsNLDE6)
names(lhsNLDE6) = paste("lhsNLDE6", 1:h, sep = "")
data6 <- cbind(data6, lhsNLDE60, lhsNLDE6)
NLDE6 <- lapply(1:13, function(x) lm(data6[, 227+x] ~ shockNL3 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc + shockDE3 + shockFR3 + shockES3 + shockIT3, data = data6))
summariesNLDE6 <- lapply(NLDE6, summary)
NLDE6conf95 <- lapply(NLDE6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
NLDE6conf68 <- lapply(NLDE6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
NLDE6up95 <- lapply(1:13, function(x) NLDE6conf95[[x]][2,2])
NLDE6low95 <- lapply(1:13, function(x) NLDE6conf95[[x]][2,1])
NLDE6up68 <- lapply(1:13, function(x) NLDE6conf68[[x]][2,2])
NLDE6low68 <- lapply(1:13, function(x) NLDE6conf68[[x]][2,1])
gammaNLDEt <- lapply(summariesNLDE6, function(x) x$coefficients[2,1])
names(gammaNLDEt) <- paste("gammaNLDEt", 0:h, sep = "")

# -- Cumulative multiplier
mNLDEtc <- cumsum(betaNLDEt) / cumsum(as.numeric(gammaNLDEt)); as.numeric(mNLDEtc)


# --- Effect of German govt spending shock on Dutch exports

# -- Equation 5
lhsDENL50 <- (data6$rxNL - data6$l1.rxNL) / data6$l1.rxNL
lhsDENL5 <- lapply(1:h, function(x) (data6[, 170+x] - data6$l1.rxNL) / data6$l1.rxNL)
lhsDENL5 <- data.frame(lhsDENL5)
names(lhsDENL5) = paste("lhsDENL5", 1:h, sep = "")
data6 <- cbind(data6, lhsDENL50, lhsDENL5)
DENL5 <- lapply(1:13, function(x) lm(data6[, 240+x] ~ shockDE2 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc + shockFR2 + shockES2 + shockIT2 + shockNL2, data = data6))
summariDEDENL5 <- lapply(DENL5, summary)
DENL5conf95 <- lapply(DENL5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
DENL5conf68 <- lapply(DENL5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
DENL5up95 <- lapply(1:13, function(x) DENL5conf95[[x]][2,2])
DENL5low95 <- lapply(1:13, function(x) DENL5conf95[[x]][2,1])
DENL5up68 <- lapply(1:13, function(x) DENL5conf68[[x]][2,2])
DENL5low68 <- lapply(1:13, function(x) DENL5conf68[[x]][2,1])
betaDENLt <- lapply(summariDEDENL5, function(x) x$coefficients[2,1])
names(betaDENLt) <- paste("betaDENLt", 0:h, sep = "")

# -- Equation 6
lhsDENL60 <- (data6$rgDE - data6$l1.rgDE) / data6$l1.rxNL
lhsDENL6 <- lapply(1:h, function(x) (data6[, 84+x] - data6$l1.rgDE) / data6$l1.rxNL)
lhsDENL6 <- data.frame(lhsDENL6)
names(lhsDENL6) = paste("lhsDENL6", 1:h, sep = "")
data6 <- cbind(data6, lhsDENL60, lhsDENL6)
DENL6 <- lapply(1:13, function(x) lm(data6[, 253+x] ~ shockDE3 + l1.debtDE + l1.intDE + l1.lrtrDE + l1.lrgDE + l1.lryDEc + l2.debtDE + l2.intDE + l2.lrtrDE + l2.lrgDE + l2.lryDEc + l3.debtDE + l3.intDE + l3.lrtrDE + l3.lrgDE + l3.lryDEc + l4.debtDE + l4.intDE + l4.lrtrDE + l4.lrgDE + l4.lryDEc + shockNL3 + shockFR3 + shockES3 + shockIT3, data = data6))
summariesDENL6 <- lapply(DENL6, summary)
DENL6conf95 <- lapply(DENL6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
DENL6conf68 <- lapply(DENL6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
DENL6up95 <- lapply(1:13, function(x) DENL6conf95[[x]][2,2])
DENL6low95 <- lapply(1:13, function(x) DENL6conf95[[x]][2,1])
DENL6up68 <- lapply(1:13, function(x) DENL6conf68[[x]][2,2])
DENL6low68 <- lapply(1:13, function(x) DENL6conf68[[x]][2,1])
gammaDENLt <- lapply(summariesDENL6, function(x) x$coefficients[2,1])
names(gammaDENLt) <- paste("gammaDENLt", 0:h, sep = "")

# -- Cumulative multiplier
mDENLtc <- cumsum(betaDENLt) / cumsum(as.numeric(gammaDENLt)); as.numeric(mDENLtc)
