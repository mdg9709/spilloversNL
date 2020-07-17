# --- Spillovers via trade channel

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Destination and origin spillovers NL.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers NL and IT v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers NL and FR v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers NL and DE v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers NL and ES v4 1.R')


# --- Effect of domestic shock on own imports
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
data3$shockNL2 <- (data3$shockNL / unlist(l.rmNL[1])) / sd((data3$shockNL / unlist(l.rmNL[1])), na.rm = TRUE)
data3$shockNL3 <- data3$shockNL2 / 100

data4 <- cbind(data3, l.rmNL, l.rxNL, rmNL.l, rxNL.l)
data5 <- subset(data4, select = -c(30:32, 35:37, 152:203))
h <- 12

# -- Equation 5
lhsNL50 <- (data5$rmNL - data5$l1.rmNL) / data5$l1.rmNL
lhsNL5 <- lapply(1:h, function(x) (data5[, 153+x] - data5$l1.rmNL) / data5$l1.rmNL)
lhsNL5 <- data.frame(lhsNL5)
names(lhsNL5) = paste("lhsNL5", 1:h, sep = "")
data5 <- cbind(data5, lhsNL50, lhsNL5)
NL5 <- lapply(1:13, function(x) lm(data5[, 177+x] ~ shockNL2 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc, data = data5))
summariesNL5 <- lapply(NL5, summary)
NL5conf95 <- lapply(NL5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
NL5conf68 <- lapply(NL5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
NL5up95 <- lapply(1:13, function(x) NL5conf95[[x]][2,2])
NL5low95 <- lapply(1:13, function(x) NL5conf95[[x]][2,1])
NL5up68 <- lapply(1:13, function(x) NL5conf68[[x]][2,2])
NL5low68 <- lapply(1:13, function(x) NL5conf68[[x]][2,1])
betaNLm <- lapply(summariesNL5, function(x) x$coefficients[2,1])
names(betaNLm) <- paste("betaNLm", 0:h, sep = "")

# -- Equation 6
lhsNL60 <- (data5$rgNL - data5$l1.rgNL) / data5$l1.rmNL
lhsNL6 <- lapply(1:h, function(x) (data5[, 96+x] - data5$l1.rgNL) / data5$l1.rmNL)
lhsNL6 <- data.frame(lhsNL6)
names(lhsNL6) = paste("lhsNL6", 1:h, sep = "")
data5 <- cbind(data5, lhsNL60, lhsNL6)
NL6 <- lapply(1:13, function(x) lm(data5[, 190+x] ~ shockNL3 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc, data = data5))
summariesNL6 <- lapply(NL6, summary)
NL6conf95 <- lapply(NL6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
NL6conf68 <- lapply(NL6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
NL6up95 <- lapply(1:13, function(x) NL6conf95[[x]][2,2])
NL6low95 <- lapply(1:13, function(x) NL6conf95[[x]][2,1])
NL6up68 <- lapply(1:13, function(x) NL6conf68[[x]][2,2])
NLlow68 <- lapply(1:13, function(x) NL6conf68[[x]][2,1])
gammaNLm <- lapply(summariesNL6, function(x) x$coefficients[2,1])
names(gammaNLm) <- paste("gammaNLm", 0:h, sep = "")

# -- Domestic cumulative multipliers NL
mNLc <- lapply(1:13, function(x) cumsum(betaNLm)[x] / cumsum(gammaNLm)[x])
mNLc1 <- as.numeric(mNLc[1]); print(mNLc1)
mNLc4 <- as.numeric(mNLc[5]); print(mNLc4)
mNLc8 <- as.numeric(mNLc[9]); print(mNLc8)
mNLc12 <- as.numeric(mNLc[13]); print(mNLc12)

# -- Generate IRF graph (as in Fig. 4 of Alloza et al.)
v1 <- data.frame(cbind(betaNLm = unlist(betaNLm), NL5up95 = unlist(NL5up95), NL5low95 = unlist(NL5low95), 
                       NL5up68 = unlist(NL5up68), NL5low68 = unlist(NL5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfNLm <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfNLm <- irfNLm + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfNLm1 <- irfNLm + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.015, 0.025, 0.01)) +
  ggtitle("NL - IMP change (GOV shock in NL)")
irfNLm2 <- irfNLm1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                           panel.background = element_rect(fill = "white"), 
                           panel.border = element_rect(linetype = "solid", fill = NA))
irfNLm2


# --- Spillovers by destination via exports

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Trade spillovers NL and DE v3 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Trade spillovers NL and ES v3 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Trade spillovers NL and IT v3 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Trade spillovers NL and FR v3 1.R')

library(lmtest)
library(purrr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)

mNLdt <- 0
gamma1NL <- 0
gamma2NL <- 0
gammaNLd <- 0
NLspillDt <- 0
error95dt <- 0
up95dt <- 0
low95dt <- 0
error68dt <- 0
up68dt <- 0
low68dt <- 0

for (i in 1:13) {
  mNLdt[i] = sum(mDENLtc[i], mFRNLtc[i], mESNLtc[i], mITNLtc[i])
  gammaNLd[i] = unlist(gammaNL[i])
  gamma1NL[i] = sum(gammaDENL[[i]], gammaESNL[[i]], gammaFRNL[[i]], gammaITNL[[i]]) 
  gamma2NL[i] = sum(gammaNLDE[[i]], gammaNLES[[i]], gammaNLFR[[i]], gammaNLIT[[i]])
  NLspillDt[i] = mNLdt[i] * (gammaNLd[i] / gamma1NL[i])
}

mNLdt
NLspillDt

for (i in 1:13) {
  error95dt[i] = qnorm(0.975) * sd(NLspillDt) / sqrt(12)
  up95dt[i] = mean(NLspillDt[i]) + error95dt[i]
  low95dt[i] = mean(NLspillDt[i]) - error95dt[i]
  error68dt[i] = qnorm(0.84) * sd(NLspillDt) / sqrt(12)
  up68dt[i] = mean(NLspillDt[i]) + error68dt[i]
  low68dt[i] = mean(NLspillDt[i]) - error68dt[i]
}

# -- Cumulative multipliers
mNLdtc1 <- cumsum(mNLdt)[1]; print(mNLdtc1)
mNLdtc4 <- cumsum(mNLdt)[5]; print(mNLdtc4)
mNLdtc8 <- cumsum(mNLdt)[9]; print(mNLdtc8)
mNLdtc12 <- cumsum(mNLdt)[13]; print(mNLdtc12)


# --- Spillovers by origin via exports
mNLot <- 0
wNL <- 0
NLspillOt <- 0
error95ot <- 0
up95ot <- 0
low95ot <- 0
error68ot <- 0
up68ot <- 0
low68ot <- 0

for (i in 1:13) {
  mNLot[i] = sum(mNLDEtc[i], mNLFRtc[i], mNLEStc[i], mNLITtc[i])
  wNL[i] = sum(NL$Y, na.rm = TRUE) / sum(DE$Y, FR$Y, ES$Y, IT$Y, na.rm = TRUE)
  NLspillOt[i] = mNLot[i] * wNL[i]
}

mNLot
NLspillOt

for (i in 1:13) {
  error95ot[i] = qnorm(0.975) * sd(NLspillOt) / sqrt(12)
  up95ot[i] = mean(NLspillOt[i]) + error95ot[i]
  low95ot[i] = mean(NLspillOt[i]) - error95ot[i]
  error68ot[i] = qnorm(0.84) * sd(NLspillOt) / sqrt(12)
  up68ot[i] = mean(NLspillOt[i]) + error68ot[i]
  low68ot[i] = mean(NLspillOt[i]) - error68ot[i]
}

# -- Cumulative multipliers
mNLotc1 <- cumsum(mNLot)[1]; print(mNLotc1)
mNLotc4 <- cumsum(mNLot)[5]; print(mNLotc4)
mNLotc8 <- cumsum(mNLot)[9]; print(mNLotc8)
mNLotc12 <- cumsum(mNLot)[13]; print(mNLotc12)
