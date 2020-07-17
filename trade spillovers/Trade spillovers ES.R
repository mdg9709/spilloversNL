# --- Spillovers via trade channel

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Destination and origin spillovers ES.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers IT and ES v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers FR and ES v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers ES and DE v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers NL and ES v4 1.R')


# --- Effect of domestic shock on own imports
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
data3$shockES2 <- (data3$shockES / unlist(l.rmES[1])) / sd((data3$shockES / unlist(l.rmES[1])), na.rm = TRUE)
data3$shockES3 <- data3$shockES2 / 100

data4 <- cbind(data3, l.rmES, l.rxES, rmES.l, rxES.l)
data4 <- subset(data4, select = -c(30:32, 35:37, 152:203))
h <- 12

# -- Equation 5
lhsES50 <- (data4$rmES - data4$l1.rmES) / data4$l1.rmES
lhsES5 <- lapply(1:h, function(x) (data4[, 153+x] - data4$l1.rmES) / data4$l1.rmES)
lhsES5 <- data.frame(lhsES5)
names(lhsES5) = paste("lhsES5", 1:h, sep = "")
data4 <- cbind(data4, lhsES50, lhsES5)
ES5 <- lapply(1:13, function(x) lm(data4[, 177+x] ~ shockES2 + l1.debtES + l1.intES + l1.lrtrES + l1.lrgES + l1.lryESc + l2.debtES + l2.intES + l2.lrtrES + l2.lrgES + l2.lryESc + l3.debtES + l3.intES + l3.lrtrES + l3.lrgES + l3.lryESc + l4.debtES + l4.intES + l4.lrtrES + l4.lrgES + l4.lryESc, data = data4))
summariesES5 <- lapply(ES5, summary)
ES5conf95 <- lapply(ES5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ES5conf68 <- lapply(ES5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ES5up95 <- lapply(1:13, function(x) ES5conf95[[x]][2,2])
ES5low95 <- lapply(1:13, function(x) ES5conf95[[x]][2,1])
ES5up68 <- lapply(1:13, function(x) ES5conf68[[x]][2,2])
ES5low68 <- lapply(1:13, function(x) ES5conf68[[x]][2,1])
betaES <- lapply(summariesES5, function(x) x$coefficients[2,1])
names(betaES) <- paste("betaES", 0:h, sep = "")

# -- Equation 6
lhsES60 <- (data4$rgES - data4$l1.rgES) / data4$l1.rmES
lhsES6 <- lapply(1:h, function(x) (data4[, 84+x] - data4$l1.rgES) / data4$l1.rmES)
lhsES6 <- data.frame(lhsES6)
names(lhsES6) = paste("lhsES6", 1:h, sep = "")
data4 <- cbind(data4, lhsES60, lhsES6)
ES6 <- lapply(1:13, function(x) lm(data4[, 190+x] ~ shockES3 + l1.debtES + l1.intES + l1.lrtrES + l1.lrgES + l1.lryESc + l2.debtES + l2.intES + l2.lrtrES + l2.lrgES + l2.lryESc + l3.debtES + l3.intES + l3.lrtrES + l3.lrgES + l3.lryESc + l4.debtES + l4.intES + l4.lrtrES + l4.lrgES + l4.lryESc, data = data4))
summariesES6 <- lapply(ES6, summary)
ES6conf95 <- lapply(ES6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ES6conf68 <- lapply(ES6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ES6up95 <- lapply(1:13, function(x) ES6conf95[[x]][2,2])
ES6low95 <- lapply(1:13, function(x) ES6conf95[[x]][2,1])
ES6up68 <- lapply(1:13, function(x) ES6conf68[[x]][2,2])
ESlow68 <- lapply(1:13, function(x) ES6conf68[[x]][2,1])
gammaES <- lapply(summariesES6, function(x) x$coefficients[2,1])
names(gammaES) <- paste("gammaES", 0:h, sep = "")

# -- Domestic cumulative multipliers ES
mESc <- lapply(1:13, function(x) cumsum(betaES)[x] / cumsum(gammaES)[x])
mESc1 <- as.numeric(mESc[1]); print(mESc1)
mESc4 <- as.numeric(mESc[5]); print(mESc4)
mESc8 <- as.numeric(mESc[9]); print(mESc8)
mESc12 <- as.numeric(mESc[13]); print(mESc12)

# -- Generate IRF graph (as in Fig. 4 of Alloza et al.)
v1 <- data.frame(cbind(betaES = unlist(betaES), ES5up95 = unlist(ES5up95), ES5low95 = unlist(ES5low95), 
                       ES5up68 = unlist(ES5up68), ES5low68 = unlist(ES5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfESm <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfESm <- irfESm + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfESm1 <- irfESm + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.01, 0.015, 0.005)) +
  ggtitle("ES - IMP change (GOV shock in ES)")
irfESm2 <- irfESm1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                           panel.background = element_rect(fill = "white"), 
                           panel.border = element_rect(linetype = "solid", fill = NA))
irfESm2


# --- Spillovers by destination via exports

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Trade spillovers ES and DE v3 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Trade spillovers NL and ES v3 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Trade spillovers IT and ES v3 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Trade spillovers FR and ES v3 1.R')

library(lmtest)
library(purrr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)

mESdt <- 0
gamma1ES <- 0
gamma2ES <- 0
ESspillDt <- 0
error95dt <- 0
up95dt <- 0
low95dt <- 0
error68dt <- 0
up68dt <- 0
low68dt <- 0

for (i in 1:13) {
  mESdt[i] = sum(mDEEStc[i], mFREStc[i], mNLEStc[i], mITEStc[i])
  gamma1ES[i] = sum(gammaDEES[[i]], gammaNLES[[i]], gammaFRES[[i]], gammaITES[[i]]) 
  gamma2ES[i] = sum(gammaESDE[[i]], gammaESNL[[i]], gammaESFR[[i]], gammaESIT[[i]])
  ESspillDt[i] = mESdt[i] * (gamma1ES[i] / gamma2ES[i])
}

mESdt
ESspillDt

for (i in 1:13) {
  error95dt[i] = qnorm(0.975) * sd(ESspillDt) / sqrt(12)
  up95dt[i] = mean(ESspillDt[i]) + error95dt[i]
  low95dt[i] = mean(ESspillDt[i]) - error95dt[i]
  error68dt[i] = qnorm(0.84) * sd(ESspillDt) / sqrt(12)
  up68dt[i] = mean(ESspillDt[i]) + error68dt[i]
  low68dt[i] = mean(ESspillDt[i]) - error68dt[i]
}

# -- Cumulative multipliers
mESdtc1 <- cumsum(mESdt)[1]; print(mESdtc1)
mESdtc4 <- cumsum(mESdt)[5]; print(mESdtc4)
mESdtc8 <- cumsum(mESdt)[9]; print(mESdtc8)
mESdtc12 <- cumsum(mESdt)[13]; print(mESdtc12)


# --- Spillovers by origin via exports
mESot <- 0
wES <- 0
ESspillOt <- 0
error95ot <- 0
up95ot <- 0
low95ot <- 0
error68ot <- 0
up68ot <- 0
low68ot <- 0

for (i in 1:13) {
  mESot[i] = sum(mESDEtc[i], mESFRtc[i], mESNLtc[i], mESITtc[i])
  wES[i] = sum(ES$Y) / sum(DE$Y, FR$Y, NL$Y, IT$Y, na.rm = TRUE)
  ESspillOt[i] = mESot[i] * wES[i]
}

mESot
ESspillOt

for (i in 1:13) {
  error95ot[i] = qnorm(0.975) * sd(ESspillOt) / sqrt(12)
  up95ot[i] = mean(ESspillOt[i]) + error95ot[i]
  low95ot[i] = mean(ESspillOt[i]) - error95ot[i]
  error68ot[i] = qnorm(0.84) * sd(ESspillOt) / sqrt(12)
  up68ot[i] = mean(ESspillOt[i]) + error68ot[i]
  low68ot[i] = mean(ESspillOt[i]) - error68ot[i]
}

# -- Cumulative multipliers
mESotc1 <- cumsum(mESot)[1]; print(mESotc1)
mESotc4 <- cumsum(mESot)[5]; print(mESotc4)
mESotc8 <- cumsum(mESot)[9]; print(mESotc8)
mESotc12 <- cumsum(mESot)[13]; print(mESotc12)
