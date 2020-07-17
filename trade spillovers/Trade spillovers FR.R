# --- Spillovers via trade channel

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/destination and origin spillovers FR.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers FR and IT v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers NL and FR v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers FR and DE v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers FR and ES v4 1.R')


# --- Effect of domestic shock on own imports
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
data3$shockFR2 <- (data3$shockFR / unlist(l.rmFR[1])) / sd((data3$shockFR / unlist(l.rmFR[1])), na.rm = TRUE)
data3$shockFR3 <- data3$shockFR2 / 100

data4 <- cbind(data3, l.rmFR, l.rxFR, rmFR.l, rxFR.l)
data4 <- subset(data4, select = -c(30:32, 35:37, 153:204))
h <- 12

# -- Equation 5
lhsFR50 <- (data4$rmFR - data4$l1.rmFR) / data4$l1.rmFR
lhsFR5 <- lapply(1:h, function(x) (data4[, 154+x] - data4$l1.rmFR) / data4$l1.rmFR)
lhsFR5 <- data.frame(lhsFR5)
names(lhsFR5) = paste("lhsFR5", 1:h, sep = "")
data4 <- cbind(data4, lhsFR50, lhsFR5)
FR5 <- lapply(1:13, function(x) lm(data4[, 178+x] ~ shockFR2 + l1.debtFR + l1.intFR + l1.lrtrFR + l1.lrgFR + l1.lryFRc + l2.debtFR + l2.intFR + l2.lrtrFR + l2.lrgFR + l2.lryFRc + l3.debtFR + l3.intFR + l3.lrtrFR + l3.lrgFR + l3.lryFRc + l4.debtFR + l4.intFR + l4.lrtrFR + l4.lrgFR + l4.lryFRc, data = data4))
summariesFR5 <- lapply(FR5, summary)
FR5conf95 <- lapply(FR5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
FR5conf68 <- lapply(FR5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
FR5up95 <- lapply(1:13, function(x) FR5conf95[[x]][2,2])
FR5low95 <- lapply(1:13, function(x) FR5conf95[[x]][2,1])
FR5up68 <- lapply(1:13, function(x) FR5conf68[[x]][2,2])
FR5low68 <- lapply(1:13, function(x) FR5conf68[[x]][2,1])
betaFR <- lapply(summariesFR5, function(x) x$coefficients[2,1])
names(betaFR) <- paste("betaFR", 0:h, sep = "")

# -- Equation 6
lhsFR60 <- (data4$rgFR - data4$l1.rgFR) / data4$l1.rmFR
lhsFR6 <- lapply(1:h, function(x) (data4[, 96+x] - data4$l1.rgFR) / data4$l1.rmFR)
lhsFR6 <- data.frame(lhsFR6)
names(lhsFR6) = paste("lhsFR6", 1:h, sep = "")
data4 <- cbind(data4, lhsFR60, lhsFR6)
FR6 <- lapply(1:13, function(x) lm(data4[, 191+x] ~ shockFR3 + l1.debtFR + l1.intFR + l1.lrtrFR + l1.lrgFR + l1.lryFRc + l2.debtFR + l2.intFR + l2.lrtrFR + l2.lrgFR + l2.lryFRc + l3.debtFR + l3.intFR + l3.lrtrFR + l3.lrgFR + l3.lryFRc + l4.debtFR + l4.intFR + l4.lrtrFR + l4.lrgFR + l4.lryFRc, data = data4))
summariesFR6 <- lapply(FR6, summary)
FR6conf95 <- lapply(FR6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
FR6conf68 <- lapply(FR6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
FR6up95 <- lapply(1:13, function(x) FR6conf95[[x]][2,2])
FR6low95 <- lapply(1:13, function(x) FR6conf95[[x]][2,1])
FR6up68 <- lapply(1:13, function(x) FR6conf68[[x]][2,2])
FRlow68 <- lapply(1:13, function(x) FR6conf68[[x]][2,1])
gammaFR <- lapply(summariesFR6, function(x) x$coefficients[2,1])
names(gammaFR) <- paste("gammaFR", 0:h, sep = "")

# -- domestic cumulative multipliers FR
mFRc <- lapply(1:13, function(x) cumsum(betaFR)[x] / cumsum(gammaFR)[x])
mFRc1 <- as.numeric(mFRc[1]); print(mFRc1)
mFRc4 <- as.numeric(mFRc[5]); print(mFRc4)
mFRc8 <- as.numeric(mFRc[9]); print(mFRc8)
mFRc12 <- as.numeric(mFRc[13]); print(mFRc12)

# -- Generate IRF graph (as in Fig. 4 of Alloza et al.)
v1 <- data.frame(cbind(betaFR = unlist(betaFR), FR5up95 = unlist(FR5up95), FR5low95 = unlist(FR5low95), 
                       FR5up68 = unlist(FR5up68), FR5low68 = unlist(FR5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfFRm <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfFRm <- irfFRm + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfFRm1 <- irfFRm + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-1, 1.5, 0.5)) +
  ggtitle("FR - IMP change (GOV shock in FR)")
irfFRm2 <- irfFRm1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                           panel.background = element_rect(fill = "white"), 
                           panel.border = element_rect(linetype = "solid", fill = NA))
irfFRm2


# --- Spillovers by destination via exports

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Trade spillovers FR and DE v3 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Trade spillovers FR and ES v3 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Trade spillovers NL and FR v3 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Trade spillovers IT and FR v3 1.R')

library(lmtest)
library(purrr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)

mFRdt <- 0
gamma1FR <- 0
gamma2FR <- 0
gammaFRd <- 0
FRspillDt <- 0
error95dt <- 0
up95dt <- 0
low95dt <- 0
error68dt <- 0
up68dt <- 0
low68dt <- 0

for (i in 1:13) {
  mFRdt[i] = sum(mDEFRtc[i], mNLFRtc[i], mESFRtc[i], mITFRtc[i])
  gamma1FR[i] = sum(gammaDEFR[[i]], gammaESFR[[i]], gammaNLFR[[i]], gammaITFR[[i]]) 
  gamma2FR[i] = sum(gammaFRDE[[i]], gammaESFR[[i]], gammaFRNL[[i]], gammaFRIT[[i]])
  FRspillDt[i] = mFRdt[i] * (gamma1FR[i] / gamma2FR[i])
}

mFRdt
FRspillDt

for (i in 1:13) {
  error95dt[i] = qnorm(0.975) * sd(FRspillDt) / sqrt(12)
  up95dt[i] = mean(FRspillDt[i]) + error95dt[i]
  low95dt[i] = mean(FRspillDt[i]) - error95dt[i]
  error68dt[i] = qnorm(0.84) * sd(FRspillDt) / sqrt(12)
  up68dt[i] = mean(FRspillDt[i]) + error68dt[i]
  low68dt[i] = mean(FRspillDt[i]) - error68dt[i]
}

# -- Cumulative multipliers
mFRdtc1 <- cumsum(mFRdt)[1]; print(mFRdtc1)
mFRdtc4 <- cumsum(mFRdt)[5]; print(mFRdtc4)
mFRdtc8 <- cumsum(mFRdt)[9]; print(mFRdtc8)
mFRdtc12 <- cumsum(mFRdt)[13]; print(mFRdtc12)


# --- Spillovers by origin via exports
mFRot <- 0
wFR <- 0
FRspillOt <- 0
error95ot <- 0
up95ot <- 0
low95ot <- 0
error68ot <- 0
up68ot <- 0
low68ot <- 0

for (i in 1:13) {
  mFRot[i] = sum(mFRDEtc[i], mFRNLtc[i], mFREStc[i], mFRITtc[i])
  wFR[i] = sum(FR$Y) / sum(DE$Y, NL$Y, ES$Y, IT$Y, na.rm = TRUE)
  FRspillOt[i] = mFRot[i] * wFR[i]
}

mFRot
FRspillOt

for (i in 1:13) {
  error95ot[i] = qnorm(0.975) * sd(FRspillOt) / sqrt(12)
  up95ot[i] = mean(FRspillOt[i]) + error95ot[i]
  low95ot[i] = mean(FRspillOt[i]) - error95ot[i]
  error68ot[i] = qnorm(0.84) * sd(FRspillOt) / sqrt(12)
  up68ot[i] = mean(FRspillOt[i]) + error68ot[i]
  low68ot[i] = mean(FRspillOt[i]) - error68ot[i]
}

# -- Cumulative multipliers
mFRotc1 <- cumsum(mFRot)[1]; print(mFRotc1)
mFRotc4 <- cumsum(mFRot)[5]; print(mFRotc4)
mFRotc8 <- cumsum(mFRot)[9]; print(mFRotc8)
mFRotc12 <- cumsum(mFRot)[13]; print(mFRotc12)
