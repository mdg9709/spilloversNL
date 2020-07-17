# --- Spillovers by destination FR

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers NL and FR v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers FR and DE v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers FR and IT v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers FR and ES v4 1.R')

library(lmtest)
library(purrr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)

mFRd <- 0
gamma1FR <- 0
gamma2FR <- 0
FRspillD <- 0
error95d <- 0
up95d <- 0
low95d <- 0
error68d <- 0
up68d <- 0
low68d <- 0

for (i in 1:13) {
  mFRd[i] = sum(as.numeric(mITFRc[i], mESFRc[i], mDEFRc[i], mNLFRc[i]))
  gamma1FR[i] = sum(as.numeric(gammaITFR[[i]], gammaDEFR[[i]], gammaESFR[[i]], gammaNLFR[[i]])) 
  gamma2FR[i] = sum(as.numeric(gammaFRIT[[i]], gammaFRDE[[i]], gammaFRES[[i]], gammaFRNL[[i]]))
  FRspillD[i] = mFRd[i] * (gamma1FR[i] / gamma2FR[i])
}

mFRd
FRspillD

for (i in 1:13) {
  error95d[i] = qnorm(0.975) * sd(FRspillD) / sqrt(12)
  up95d[i] = mean(FRspillD[i]) + error95d[i]
  low95d[i] = mean(FRspillD[i]) - error95d[i]
  error68d[i] = qnorm(0.84) * sd(FRspillD) / sqrt(12)
  up68d[i] = mean(FRspillD[i]) + error68d[i]
  low68d[i] = mean(FRspillD[i]) - error68d[i]
}

# -- Cumulative multipliers
mFRdc1 <- cumsum(mFRd)[1]; print(mFRdc1)
mFRdc4 <- cumsum(mFRd)[5]; print(mFRdc4)
mFRdc8 <- cumsum(mFRd)[9]; print(mFRdc8)
mFRdc12 <- cumsum(mFRd)[13]; print(mFRdc12)

# -- Generate IRF graph (as in Fig. 2 of Alloza et al.)
v1 <- data.frame(cbind(FRspillD, up95d, low95d, up68d, low68d))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfFRd <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfFRd <- irfFRd + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfFRd1 <- irfFRd + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-1.5, 1.5, 0.5)) +
  ggtitle("FR - GDP change (GOV shock in rest)")
irfFRd2 <- irfFRd1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                           panel.background = element_rect(fill = "white"), 
                           panel.border = element_rect(linetype = "solid", fill = NA))
irfFRd2


# --- Spillovers by origin FR
mFRo <- 0
wFR <- 0
FRspillO <- 0
error95o <- 0
up95o <- 0
low95o <- 0
error68o <- 0
up68o <- 0
low68o <- 0

for (i in 1:13) {
  mFRo[i] = sum(as.numeric(mFRITc[i], mFRESc[i], mFRDEc[i], mFRNLc[i]))
}

mFRo

for (i in 1:13) {
  wFR[i] = sum(FR$Y) / sum(IT$Y, ES$Y, DE$Y, NL$Y, na.rm = TRUE)
  FRspillO[i] = mFRo[i] * wFR[i]
}

FRspillO

for (i in 1:13) {
  error95o[i] = qnorm(0.975) * sd(FRspillO) / sqrt(12)
  up95o[i] = mean(FRspillO[i]) + error95o[i]
  low95o[i] = mean(FRspillO[i]) - error95o[i]
  error68o[i] = qnorm(0.84) * sd(FRspillO) / sqrt(12)
  up68o[i] = mean(FRspillO[i]) + error68o[i]
  low68o[i] = mean(FRspillO[i]) - error68o[i] 
}

# -- Cumulative multipliers
mFRoc1 <- cumsum(mFRo)[1]; print(mFRoc1)
mFRoc4 <- cumsum(mFRo)[5]; print(mFRoc4)
mFRoc8 <- cumsum(mFRo)[9]; print(mFRoc8)
mFRoc12 <- cumsum(mFRo)[13]; print(mFRoc12)

# -- Generate IRF graph (as in Fig. 3 of Alloza et al.)
v1 <- data.frame(cbind(FRspillO, up95o, low95o, up68o, low68o))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfFRo <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfFRo <- irfFRo + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfFRo1 <- irfFRo + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.1, 0.3, 0.1)) +
  ggtitle("Rest - GDP change (GOV shock in FR)")
irfFRo2 <- irfFRo1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                           panel.background = element_rect(fill = "white"), 
                           panel.border = element_rect(linetype = "solid", fill = NA))
irfFRo2


# --- Domestic govt spending effects FR
ryFR.l <- data.frame(shift(data2$ryFR, n = 1:12, type = "lead"))
names(ryFR.l) = c("ryFR.l1", "ryFR.l2", "ryFR.l3", "ryFR.l4", "ryFR.l5", "ryFR.l6",
                  "ryFR.l7", "ryFR.l8", "ryFR.l9", "ryFR.l10", "ryFR.l11", "ryFR.l12")
rgFR.l <- data.frame(shift(data2$rgFR, n = 1:12, type = "lead"))
names(rgFR.l) = c("rgFR.l1", "rgFR.l2", "rgFR.l3", "rgFR.l4", "rgFR.l5", "rgFR.l6",
                  "rgFR.l7", "rgFR.l8", "rgFR.l9", "rgFR.l10", "rgFR.l11", "rgFR.l12")
l.ryFRc <- data.frame(shift(data2$ryFR, n = 1:4, type = "lag"))
names(l.ryFRc) = c("l1.ryFRc", "l2.ryFRc", "l3.ryFRc", "l4.ryFRc")
l.debtFR <- data.frame(shift(data2$debtFR, n = 1:4, type = "lag"))
names(l.debtFR) = c("l1.debtFR", "l2.debtFR", "l3.debtFR", "l4.debtFR")
l.intFR <- data.frame(shift(data2$intFR, n = 1:4, type = "lag"))
names(l.intFR) = c("l1.intFR", "l2.intFR", "l3.intFR", "l4.intFR")
l.lrtrFR <- data.frame(shift(data2$lrtrFR, n = 1:4, type = "lag"))
names(l.lrtrFR) = c("l1.lrtrFR", "l2.lrtrFR", "l3.lrtrFR", "l4.lrtrFR")
l.lrgFR <- data.frame(shift(data2$lrgFR, n = 1:4, type = "lag"))
names(l.lrgFR) = c("l1.lrgFR", "l2.lrgFR", "l3.lrgFR", "l4.lrgFR")
l.lryFRc <- data.frame(shift(data2$lryFRc, n = 1:4, type = "lag"))
names(l.lryFRc) = c("l1.lryFRc", "l2.lryFRc", "l3.lryFRc", "l4.lryFRc")
data3 <- cbind(quarter = data2$quarter, ryFR = data2$ryFR, rgFR = data2$rgFR, debtFR = data2$debtFR, 
               intFR = data2$intFR, lrtrFR = data2$lrtrFR, lrgFR = data2$lrgFR, lryFR = data2$lryFR, 
               lryFRc = data2$lryFRc, shockFR2 = data2$shockFR2, shockFR3 = data2$shockFR3, l1.ryFR = data2$l1.ryFR, 
               l1.rgFR = data2$l1.rgFR, l.debtFR, l.intFR, l.lrtrFR, l.lrgFR, l.lryFRc, ryFR.l, rgFR.l)
h <- 12

# -- Equation 5
lhsFR50 <- (data3$ryFR - data3$l1.ryFR) / data3$l1.ryFR
lhsFR5 <- lapply(1:h, function(x) (data3[, 33+x] - data3$l1.ryFR) / data3$l1.ryFR)
lhsFR5 <- data.frame(lhsFR5)
names(lhsFR5) = paste("lhsFR5", 1:h, sep = "")
data3 <- cbind(data3, lhsFR50, lhsFR5)
FR5 <- lapply(1:13, function(x) lm(data3[, 57+x] ~ shockFR3 + l1.debtFR + l1.intFR + l1.lrtrFR + l1.lrgFR + l1.lryFRc + l2.debtFR + l2.intFR + l2.lrtrFR + l2.lrgFR + l2.lryFRc + l3.debtFR + l3.intFR + l3.lrtrFR + l3.lrgFR + l3.lryFRc + l4.debtFR + l4.intFR + l4.lrtrFR + l4.lrgFR + l4.lryFRc, data = data3))
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
lhsFR60 <- (data3$rgFR - data3$l1.rgFR) / data3$l1.ryFR
lhsFR6 <- lapply(1:h, function(x) (data3[, 45+x] - data3$l1.rgFR) / data3$l1.ryFR)
lhsFR6 <- data.frame(lhsFR6)
names(lhsFR6) = paste("lhsFR6", 1:h, sep = "")
data3 <- cbind(data3, lhsFR60, lhsFR6)
FR6 <- lapply(1:13, function(x) lm(data3[, 70+x] ~ shockFR3 + l1.debtFR + l1.intFR + l1.lrtrFR + l1.lrgFR + l1.lryFRc + l2.debtFR + l2.intFR + l2.lrtrFR + l2.lrgFR + l2.lryFRc + l3.debtFR + l3.intFR + l3.lrtrFR + l3.lrgFR + l3.lryFRc + l4.debtFR + l4.intFR + l4.lrtrFR + l4.lrgFR + l4.lryFRc, data = data3))
summariesFR6 <- lapply(FR6, summary)
FR6conf95 <- lapply(FR6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
FR6conf68 <- lapply(FR6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
FR6up95 <- lapply(1:13, function(x) FR6conf95[[x]][2,2])
FR6low95 <- lapply(1:13, function(x) FR6conf95[[x]][2,1])
FR6up68 <- lapply(1:13, function(x) FR6conf68[[x]][2,2])
FRlow68 <- lapply(1:13, function(x) FR6conf68[[x]][2,1])
gammaFR <- lapply(summariesFR6, function(x) x$coefficients[2,1])
names(gammaFR) <- paste("gammaFR", 0:h, sep = "")

# -- Domestic cumulative multiplier FR
mFRc <- lapply(1:13, function(x) cumsum(as.numeric(betaFR))[x] / cumsum(as.numeric(gammaFR))[x])
mFRc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaFR) / as.numeric(gammaFR))[x])
mFRc <- unlist(mFRc)

error95 <- 0
up95 <- 0
low95 <- 0
error68 <- 0
up68 <- 0
low68 <- 0

for (i in 1:13) {
  error95[i] = qnorm(0.975) * sd(mFRc) / sqrt(12)
  up95[i] = mean(mFRc[i]) + error95[i]
  low95[i] = mean(mFRc[i]) - error95[i]
  error68[i] = qnorm(0.84) * sd(mFRc) / sqrt(12)
  up68[i] = mean(mFRc[i]) + error68[i]
  low68[i] = mean(mFRc[i]) - error68[i]
}

# -- Generate IRF graph on domestic multipliers FR
v1 <- data.frame(cbind(mFRc = unlist(mFRc), up95, low95, up68, low68))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfFR <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfFR <- irfFR + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfFR1 <- irfFR + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-2, 6, 2)) +
  ggtitle("FR - multiplier (GOV shock in FR)")
irfFR2 <- irfFR1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                         panel.background = element_rect(fill = "white"), 
                         panel.borFRr = element_rect(linetype = "solid", fill = NA))
irfFR2
