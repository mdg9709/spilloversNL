# --- Spillovers via trade channel

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Destination and origin spillovers DE.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers IT and DE v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers FR and DE v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers NL and DE v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers ES and DE v4 1.R')


# --- Effect of domestic shock on own imports
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
data3$shockDE2 <- (data3$shockDE / unlist(l.rmDE[1])) / sd((data3$shockDE / unlist(l.rmDE[1])), na.rm = TRUE)
data3$shockDE3 <- data3$shockDE2 / 100

data4 <- cbind(data3, l.rmDE, l.rxDE, rmDE.l, rxDE.l)
data4 <- subset(data4, select = -c(30:32, 35:37, 153:204))
h <- 12

# -- Equation 5
lhsDE50 <- (data4$rmDE - data4$l1.rmDE) / data4$l1.rmDE
lhsDE5 <- lapply(1:h, function(x) (data4[, 154+x] - data4$l1.rmDE) / data4$l1.rmDE)
lhsDE5 <- data.frame(lhsDE5)
names(lhsDE5) = paste("lhsDE5", 1:h, sep = "")
data4 <- cbind(data4, lhsDE50, lhsDE5)
DE5 <- lapply(1:13, function(x) lm(data4[, 178+x] ~ shockDE2 + l1.debtDE + l1.intDE + l1.lrtrDE + l1.lrgDE + l1.lryDEc + l2.debtDE + l2.intDE + l2.lrtrDE + l2.lrgDE + l2.lryDEc + l3.debtDE + l3.intDE + l3.lrtrDE + l3.lrgDE + l3.lryDEc + l4.debtDE + l4.intDE + l4.lrtrDE + l4.lrgDE + l4.lryDEc, data = data4))
summariesDE5 <- lapply(DE5, summary)
DE5conf95 <- lapply(DE5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
DE5conf68 <- lapply(DE5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
DE5up95 <- lapply(1:13, function(x) DE5conf95[[x]][2,2])
DE5low95 <- lapply(1:13, function(x) DE5conf95[[x]][2,1])
DE5up68 <- lapply(1:13, function(x) DE5conf68[[x]][2,2])
DE5low68 <- lapply(1:13, function(x) DE5conf68[[x]][2,1])
betaDE <- lapply(summariesDE5, function(x) x$coefficients[2,1])
names(betaDE) <- paste("betaDE", 0:h, sep = "")

# -- Equation 6
lhsDE60 <- (data4$rgDE - data4$l1.rgDE) / data4$l1.rmDE
lhsDE6 <- lapply(1:h, function(x) (data4[, 84+x] - data4$l1.rgDE) / data4$l1.rmDE)
lhsDE6 <- data.frame(lhsDE6)
names(lhsDE6) = paste("lhsDE6", 1:h, sep = "")
data4 <- cbind(data4, lhsDE60, lhsDE6)
DE6 <- lapply(1:13, function(x) lm(data4[, 191+x] ~ shockDE3 + l1.debtDE + l1.intDE + l1.lrtrDE + l1.lrgDE + l1.lryDEc + l2.debtDE + l2.intDE + l2.lrtrDE + l2.lrgDE + l2.lryDEc + l3.debtDE + l3.intDE + l3.lrtrDE + l3.lrgDE + l3.lryDEc + l4.debtDE + l4.intDE + l4.lrtrDE + l4.lrgDE + l4.lryDEc, data = data4))
summariesDE6 <- lapply(DE6, summary)
DE6conf95 <- lapply(DE6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
DE6conf68 <- lapply(DE6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
DE6up95 <- lapply(1:13, function(x) DE6conf95[[x]][2,2])
DE6low95 <- lapply(1:13, function(x) DE6conf95[[x]][2,1])
DE6up68 <- lapply(1:13, function(x) DE6conf68[[x]][2,2])
DElow68 <- lapply(1:13, function(x) DE6conf68[[x]][2,1])
gammaDE <- lapply(summariesDE6, function(x) x$coefficients[2,1])
names(gammaDE) <- paste("gammaDE", 0:h, sep = "")

# -- Domestic cumulative multipliers DE
mDEc <- lapply(1:13, function(x) cumsum(betaDE)[x] / cumsum(gammaDE)[x])
mDEc1 <- as.numeric(mDEc[1]); print(mDEc1)
mDEc4 <- as.numeric(mDEc[5]); print(mDEc4)
mDEc8 <- as.numeric(mDEc[9]); print(mDEc8)
mDEc12 <- as.numeric(mDEc[13]); print(mDEc12)

# -- Generate IRF graph (as in Fig. 4 of Alloza et al.)
v1 <- data.frame(cbind(betaDE = unlist(betaDE), DE5up95 = unlist(DE5up95), DE5low95 = unlist(DE5low95), 
                       DE5up68 = unlist(DE5up68), DE5low68 = unlist(DE5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfDEm <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfDEm <- irfDEm + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfDEm1 <- irfDEm + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.01, 0.025, 0.005)) +
  ggtitle("DE - IMP change (GOV shock in DE)")
irfDEm2 <- irfDEm1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                           panel.background = element_rect(fill = "white"), 
                           panel.border = element_rect(linetype = "solid", fill = NA))
irfDEm2


# --- Spillovers by destination via exports

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Trade spillovers NL and DE v3 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Trade spillovers ES and DE v3 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Trade spillovers IT and DE v3 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Trade spillovers FR and DE v3 1.R')

library(lmtest)
library(purrr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)

mDEdt <- 0
gamma1DE <- 0
gamma2DE <- 0
DEspillDt <- 0
error95dt <- 0
up95dt <- 0
low95dt <- 0
error68dt <- 0
up68dt <- 0
low68dt <- 0

for (i in 1:13) {
  mDEdt[i] = sum(mNLDEtc[i], mFRDEtc[i], mESDEtc[i], mITDEtc[i])
  gamma1DE[i] = sum(gammaNLDE[[i]], gammaESDE[[i]], gammaFRDE[[i]], gammaITDE[[i]]) 
  gamma2DE[i] = sum(gammaDENL[[i]], gammaDEES[[i]], gammaDEFR[[i]], gammaDEIT[[i]])
  DEspillDt[i] = mDEdt[i] * (gamma1DE[i] / gamma2DE[i])
}

mDEdt
DEspillDt

for (i in 1:13) {
  error95dt[i] = qnorm(0.975) * sd(DEspillDt) / sqrt(12)
  up95dt[i] = mean(DEspillDt[i]) + error95dt[i]
  low95dt[i] = mean(DEspillDt[i]) - error95dt[i]
  error68dt[i] = qnorm(0.84) * sd(DEspillDt) / sqrt(12)
  up68dt[i] = mean(DEspillDt[i]) + error68dt[i]
  low68dt[i] = mean(DEspillDt[i]) - error68dt[i]
}

# -- Cumulative multipliers
mDEdtc1 <- cumsum(mDEdt)[1]; print(mDEdtc1)
mDEdtc4 <- cumsum(mDEdt)[5]; print(mDEdtc4)
mDEdtc8 <- cumsum(mDEdt)[9]; print(mDEdtc8)
mDEdtc12 <- cumsum(mDEdt)[13]; print(mDEdtc12)


# --- Spillovers by origin via exports
mDEot <- 0
wDE <- 0
DEspillOt <- 0
error95ot <- 0
up95ot <- 0
low95ot <- 0
error68ot <- 0
up68ot <- 0
low68ot <- 0

for (i in 1:13) {
  mDEot[i] = sum(mDENLtc[i], mDEFRtc[i], mDEEStc[i], mDEITtc[i])
  wDE[i] = sum(DE$Y) / sum(NL$Y, FR$Y, ES$Y, IT$Y, na.rm = TRUE)
  DEspillOt[i] = mDEot[i] * wDE[i]
}

mDEot
DEspillOt

for (i in 1:13) {
  error95ot[i] = qnorm(0.975) * sd(DEspillOt) / sqrt(12)
  up95ot[i] = mean(DEspillOt[i]) + error95ot[i]
  low95ot[i] = mean(DEspillOt[i]) - error95ot[i]
  error68ot[i] = qnorm(0.84) * sd(DEspillOt) / sqrt(12)
  up68ot[i] = mean(DEspillOt[i]) + error68ot[i]
  low68ot[i] = mean(DEspillOt[i]) - error68ot[i]
}

# -- Cumulative multipliers
mDEotc1 <- cumsum(mDEot)[1]; print(mDEotc1)
mDEotc4 <- cumsum(mDEot)[5]; print(mDEotc4)
mDEotc8 <- cumsum(mDEot)[9]; print(mDEotc8)
mDEotc12 <- cumsum(mDEot)[13]; print(mDEotc12)
