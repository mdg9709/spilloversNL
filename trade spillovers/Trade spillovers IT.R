# --- Spillovers via trade channel

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Destination and origin spillovers IT.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers NL and IT v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers FR and IT v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers IT and DE v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers IT and ES v4 1.R')


# --- Effect of domestic shock on own imports
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
data3$shockIT2 <- (data3$shockIT / unlist(l.rmIT[1])) / sd((data3$shockIT / unlist(l.rmIT[1])), na.rm = TRUE)
data3$shockIT3 <- data3$shockIT2 / 100

data4 <- cbind(data3, l.rmIT, l.rxIT, rmIT.l, rxIT.l)
data5 <- subset(data4, select = -c(30:32, 35:37, 153:204))
h <- 12

# -- Equation 5
lhsIT50 <- (data5$rmIT - data5$l1.rmIT) / data5$rmIT
lhsIT5 <- lapply(1:h, function(x) (data5[, 154+x] - data5$l1.rmIT) / data5$l1.rmIT)
lhsIT5 <- data.frame(lhsIT5)
names(lhsIT5) = paste("lhsIT5", 1:h, sep = "")
data6 <- cbind(data5, lhsIT50, lhsIT5)
IT5 <- lapply(1:13, function(x) lm(data6[, 178+x] ~ shockIT3 + l1.debtIT + l1.intIT + l1.lrtrIT + l1.lrgIT + l1.lryITc + l2.debtIT + l2.intIT + l2.lrtrIT + l2.lrgIT + l2.lryITc + l3.debtIT + l3.intIT + l3.lrtrIT + l3.lrgIT + l3.lryITc + l4.debtIT + l4.intIT + l4.lrtrIT + l4.lrgIT + l4.lryITc, data = data6))
summariesIT5 <- lapply(IT5, summary)
IT5conf95 <- lapply(IT5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
IT5conf68 <- lapply(IT5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
IT5up95 <- lapply(1:13, function(x) IT5conf95[[x]][2,2])
IT5low95 <- lapply(1:13, function(x) IT5conf95[[x]][2,1])
IT5up68 <- lapply(1:13, function(x) IT5conf68[[x]][2,2])
IT5low68 <- lapply(1:13, function(x) IT5conf68[[x]][2,1])
betaITt <- lapply(summariesIT5, function(x) x$coefficients[2,1])
names(betaITt) <- paste("betaITt", 0:h, sep = "")

# -- Equation 6
lhsIT60 <- (data6$rgIT - data6$l1.rgIT) / data6$l1.rmIT
lhsIT6 <- lapply(1:h, function(x) (data6[, 84+x] - data6$l1.rgIT) / data6$l1.rmIT)
lhsIT6 <- data.frame(lhsIT6)
names(lhsIT6) = paste("lhsIT6", 1:h, sep = "")
data6 <- cbind(data6, lhsIT60, lhsIT6)
IT6 <- lapply(1:13, function(x) lm(data6[, 191+x] ~ shockIT3 + l1.debtIT + l1.intIT + l1.lrtrIT + l1.lrgIT + l1.lryITc + l2.debtIT + l2.intIT + l2.lrtrIT + l2.lrgIT + l2.lryITc + l3.debtIT + l3.intIT + l3.lrtrIT + l3.lrgIT + l3.lryITc + l4.debtIT + l4.intIT + l4.lrtrIT + l4.lrgIT + l4.lryITc, data = data6))
summariesIT6 <- lapply(IT6, summary)
IT6conf95 <- lapply(IT6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
IT6conf68 <- lapply(IT6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
IT6up95 <- lapply(1:13, function(x) IT6conf95[[x]][2,2])
IT6low95 <- lapply(1:13, function(x) IT6conf95[[x]][2,1])
IT6up68 <- lapply(1:13, function(x) IT6conf68[[x]][2,2])
IT6low68 <- lapply(1:13, function(x) IT6conf68[[x]][2,1])
gammaITt <- lapply(summariesIT6, function(x) x$coefficients[2,1])
names(gammaITt) <- paste("gammaITt", 0:h, sep = "")

# -- Domestic cumulative multipliers IT
mITtc <- cumsum(betaITt) / cumsum(as.numeric(gammaITt)); as.numeric(mITtc)
mITc1 <- as.numeric(mITtc[1]); print(mITc1)
mITc4 <- as.numeric(mITtc[5]); print(mITc4)
mITc8 <- as.numeric(mITtc[9]); print(mITc8)
mITc12 <- as.numeric(mITtc[13]); print(mITc12)

# -- Generate IRF graph (as in Fig. 4 of Alloza et al.)
v1 <- data.frame(cbind(betaITm = unlist(betaITm), IT5up95 = unlist(IT5up95), IT5low95 = unlist(IT5low95), 
                       IT5up68 = unlist(IT5up68), IT5low68 = unlist(IT5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfITm <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfITm <- irfITm + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfITm1 <- irfITm + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.8, 0.2, 0.2)) +
  ggtitle("IT - IMP change (GOV shock in IT)")
irfITm2 <- irfITm1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                           panel.background = element_rect(fill = "white"), 
                           panel.border = element_rect(linetype = "solid", fill = NA))
irfITm2


# --- Spillovers by destination via exports

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Trade spillovers IT and DE v3 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Trade spillovers IT and ES v3 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Trade spillovers NL and IT v3 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Trade spillovers IT and FR v3 1.R')

library(lmtest)
library(purrr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)

mITdt <- 0
gamma1IT <- 0
gamma2IT <- 0
ITspillDt <- 0
error95dt <- 0
up95dt <- 0
low95dt <- 0
error68dt <- 0
up68dt <- 0
low68dt <- 0

for (i in 1:13) {
  mITdt[i] = sum(mDEITtc[i], mFRITtc[i], mESITtc[i], mNLITtc[i])
  gamma1IT[i] = sum(gammaDEIT[[i]], gammaESIT[[i]], gammaFRIT[[i]], gammaNLIT[[i]]) 
  gamma2IT[i] = sum(gammaITDE[[i]], gammaITES[[i]], gammaITFR[[i]], gammaITNL[[i]])
  ITspillDt[i] = mITdt[i] * (gamma1IT[i] / gamma2IT[i])
}

mITdt
ITspillDt

for (i in 1:13) {
  error95dt[i] = qnorm(0.975) * sd(ITspillDt) / sqrt(12)
  up95dt[i] = mean(ITspillDt[i]) + error95dt[i]
  low95dt[i] = mean(ITspillDt[i]) - error95dt[i]
  error68dt[i] = qnorm(0.84) * sd(ITspillDt) / sqrt(12)
  up68dt[i] = mean(ITspillDt[i]) + error68dt[i]
  low68dt[i] = mean(ITspillDt[i]) - error68dt[i]
}

# -- Cumulative multipliers
mITdtc1 <- cumsum(mITdt)[1]; print(mITdtc1)
mITdtc4 <- cumsum(mITdt)[5]; print(mITdtc4)
mITdtc8 <- cumsum(mITdt)[9]; print(mITdtc8)
mITdtc12 <- cumsum(mITdt)[13]; print(mITdtc12)


# --- Spillovers by origin via exports
mITot <- 0
wIT <- 0
ITspillOt <- 0
error95ot <- 0
up95ot <- 0
low95ot <- 0
error68ot <- 0
up68ot <- 0
low68ot <- 0

for (i in 1:13) {
  mITot[i] = sum(mITDEtc[i], mITFRtc[i], mITEStc[i], mITNLtc[i])
  wIT[i] = sum(IT$Y) / sum(DE$Y, FR$Y, ES$Y, NL$Y, na.rm = TRUE)
  ITspillOt[i] = mITot[i] * wIT[i]
}

mITot
ITspillOt

for (i in 1:13) {
  error95ot[i] = qnorm(0.975) * sd(ITspillOt) / sqrt(12)
  up95ot[i] = mean(ITspillOt[i]) + error95ot[i]
  low95ot[i] = mean(ITspillOt[i]) - error95ot[i]
  error68ot[i] = qnorm(0.84) * sd(ITspillOt) / sqrt(12)
  up68ot[i] = mean(ITspillOt[i]) + error68ot[i]
  low68ot[i] = mean(ITspillOt[i]) - error68ot[i]
}

# -- Cumulative multipliers
mITotc1 <- cumsum(mITot)[1]; print(mITotc1)
mITotc4 <- cumsum(mITot)[5]; print(mITotc4)
mITotc8 <- cumsum(mITot)[9]; print(mITotc8)
mITotc12 <- cumsum(mITot)[13]; print(mITotc12)
