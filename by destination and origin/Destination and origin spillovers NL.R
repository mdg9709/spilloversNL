# --- Domestic govt spending effects NL

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers NL and IT v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers NL and FR v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers NL and DE v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers NL and ES v4 1.R')

library(lmtest)
library(purrr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)

ryNL.l <- data.frame(shift(data2$ryNL, n = 1:12, type = "lead"))
names(ryNL.l) = c("ryNL.l1", "ryNL.l2", "ryNL.l3", "ryNL.l4", "ryNL.l5", "ryNL.l6",
                  "ryNL.l7", "ryNL.l8", "ryNL.l9", "ryNL.l10", "ryNL.l11", "ryNL.l12")
rgNL.l <- data.frame(shift(data2$rgNL, n = 1:12, type = "lead"))
names(rgNL.l) = c("rgNL.l1", "rgNL.l2", "rgNL.l3", "rgNL.l4", "rgNL.l5", "rgNL.l6",
                  "rgNL.l7", "rgNL.l8", "rgNL.l9", "rgNL.l10", "rgNL.l11", "rgNL.l12")
l.ryNLc <- data.frame(shift(data2$ryNL, n = 1:4, type = "lag"))
names(l.ryNLc) = c("l1.ryNLc", "l2.ryNLc", "l3.ryNLc", "l4.ryNLc")
l.debtNL <- data.frame(shift(data2$debtNL, n = 1:4, type = "lag"))
names(l.debtNL) = c("l1.debtNL", "l2.debtNL", "l3.debtNL", "l4.debtNL")
l.intNL <- data.frame(shift(data2$intNL, n = 1:4, type = "lag"))
names(l.intNL) = c("l1.intNL", "l2.intNL", "l3.intNL", "l4.intNL")
l.lrtrNL <- data.frame(shift(data2$lrtrNL, n = 1:4, type = "lag"))
names(l.lrtrNL) = c("l1.lrtrNL", "l2.lrtrNL", "l3.lrtrNL", "l4.lrtrNL")
l.lrgNL <- data.frame(shift(data2$lrgNL, n = 1:4, type = "lag"))
names(l.lrgNL) = c("l1.lrgNL", "l2.lrgNL", "l3.lrgNL", "l4.lrgNL")
l.lryNLc <- data.frame(shift(data2$lryNLc, n = 1:4, type = "lag"))
names(l.lryNLc) = c("l1.lryNLc", "l2.lryNLc", "l3.lryNLc", "l4.lryNLc")
data4 <- cbind(quarter = data2$quarter, ryNL = data2$ryNL, rgNL = data2$rgNL, debtNL = data2$debtNL, 
               intNL = data2$intNL, lrtrNL = data2$lrtrNL, lrgNL = data2$lrgNL, lryNL = data2$lryNL, 
               lryNLc = data2$lryNLc, shockNL3 = data2$shockNL3, l1.ryNL = data2$l1.ryNL, 
               l1.rgNL = data2$l1.rgNL, l.debtNL, l.intNL, l.lrtrNL, l.lrgNL, l.lryNLc, ryNL.l, rgNL.l)
h <- 12

# -- Equation 5
lhsNL50 <- (data4$ryNL - data4$l1.ryNL) / data4$l1.ryNL
lhsNL5 <- lapply(1:h, function(x) (data4[, 32+x] - data4$l1.ryNL) / data4$l1.ryNL)
lhsNL5 <- data.frame(lhsNL5)
names(lhsNL5) = paste("lhsNL5", 1:h, sep = "")
data4 <- cbind(data4, lhsNL50, lhsNL5)
NL5 <- lapply(1:13, function(x) lm(data4[, 56+x] ~ shockNL3 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc, data = data4))
summariesNL5 <- lapply(NL5, summary)
NL5conf95 <- lapply(NL5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
NL5conf68 <- lapply(NL5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
NL5up95 <- lapply(1:13, function(x) NL5conf95[[x]][2,2])
NL5low95 <- lapply(1:13, function(x) NL5conf95[[x]][2,1])
NL5up68 <- lapply(1:13, function(x) NL5conf68[[x]][2,2])
NL5low68 <- lapply(1:13, function(x) NL5conf68[[x]][2,1])
betaNL <- lapply(summariesNL5, function(x) x$coefficients[2,1])
names(betaNL) <- paste("betaNL", 0:h, sep = "")

# -- Equation 6
lhsNL60 <- (data4$rgNL - data4$l1.rgNL) / data4$l1.ryNL
lhsNL6 <- lapply(1:h, function(x) (data4[, 44+x] - data4$l1.rgNL) / data4$l1.ryNL)
lhsNL6 <- data.frame(lhsNL6)
names(lhsNL6) = paste("lhsNL6", 1:h, sep = "")
data4 <- cbind(data4, lhsNL60, lhsNL6)
NL6 <- lapply(1:13, function(x) lm(data4[, 69+x] ~ shockNL3 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc, data = data4))
summariesNL6 <- lapply(NL6, summary)
NL6conf95 <- lapply(NL6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
NL6conf68 <- lapply(NL6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
NL6up95 <- lapply(1:13, function(x) NL6conf95[[x]][2,2])
NL6low95 <- lapply(1:13, function(x) NL6conf95[[x]][2,1])
NL6up68 <- lapply(1:13, function(x) NL6conf68[[x]][2,2])
NLlow68 <- lapply(1:13, function(x) NL6conf68[[x]][2,1])
gammaNL <- lapply(summariesNL6, function(x) x$coefficients[2,1])
names(gammaNL) <- paste("gammaNL", 0:h, sep = "")

# -- Domestic cumulative multiplier NL
mNLc <- lapply(1:13, function(x) cumsum(as.numeric(betaNL))[x] / cumsum(as.numeric(gammaNL))[x])
mNLc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaNL) / as.numeric(gammaNL))[x])
mNLc <- unlist(mNLc)

error95 <- 0
up95 <- 0
low95 <- 0
error68 <- 0
up68 <- 0
low68 <- 0

for (i in 1:13) {
  error95[i] = qnorm(0.975) * sd(mNLc) / sqrt(12)
  up95[i] = mean(mNLc[i]) + error95[i]
  low95[i] = mean(mNLc[i]) - error95[i]
  error68[i] = qnorm(0.84) * sd(mNLc) / sqrt(12)
  up68[i] = mean(mNLc[i]) + error68[i]
  low68[i] = mean(mNLc[i]) - error68[i]
}

# -- Generate IRF graph on domestic multipliers NL
v1 <- data.frame(cbind(mNLc = unlist(mNLc), up95, low95, up68, low68))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfNL <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfNL <- irfNL + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfNL1 <- irfNL + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-2, 6, 2)) +
  ggtitle("NL - multiplier (GOV shock in NL)")
irfNL2 <- irfNL1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                         panel.background = element_rect(fill = "white"), 
                         panel.border = element_rect(linetype = "solid", fill = NA))
irfNL2


# --- Spillovers by destination NL

mNLd <- 0
gamma1NL <- 0
gamma2NL <- 0
gammaNLd <- 0
NLspillD <- 0
error95d <- 0
up95d <- 0
low95d <- 0
error68d <- 0
up68d <- 0
low68d <- 0

for (i in 1:13) {
  mNLd[i] = sum(as.numeric(mDENLc[i], mFRNLc[i], mESNLc[i], mITNLc[i]))
  gamma1NL[i] = sum(as.numeric(gammaDENL[[i]], gammaESNL[[i]], gammaFRNL[[i]], gammaITNL[[i]])) 
  gamma2NL[i] = sum(as.numeric(gammaNLDE[[i]], gammaNLES[[i]], gammaNLFR[[i]], gammaNLIT[[i]]))
  gammaNLd[i] = unlist(gammaNL[i])
}

mNLd[1] = mNLd[1] / 100
mNLd

for (i in 1:13) {
  NLspillD[i] = mNLd[i] * (gammaNLd[i] / gamma1NL[i])
}

NLspillD[6] = NLspillD[6] / 100
NLspillD

for (i in 1:13) {
  error95d[i] = qnorm(0.975) * sd(NLspillD) / sqrt(12)
  up95d[i] = mean(NLspillD[i]) + error95d[i]
  low95d[i] = mean(NLspillD[i]) - error95d[i]
  error68d[i] = qnorm(0.84) * sd(NLspillD) / sqrt(12)
  up68d[i] = mean(NLspillD[i]) + error68d[i]
  low68d[i] = mean(NLspillD[i]) - error68d[i]
}

# -- Cumulative multipliers
mNLdc1 <- cumsum(mNLd)[1]; print(mNLdc1)
mNLdc4 <- cumsum(mNLd)[5]; print(mNLdc4)
mNLdc8 <- cumsum(mNLd)[9]; print(mNLdc8)
mNLdc12 <- cumsum(mNLd)[13]; print(mNLdc12)

# -- Generate IRF graph (as in Fig. 2 of Alloza et al.)
v1 <- data.frame(cbind(NLspillD, up95d, low95d, up68d, low68d))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfNLd <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfNLd <- irfNLd + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfNLd1 <- irfNLd + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.5, 0.75, 0.25)) +
  ggtitle("NL - GDP change (GOV shock in rest)")
irfNLd2 <- irfNLd1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfNLd2


# --- Spillovers by origin NL
mNLo <- 0
wNL <- 0
NLspillO <- 0
error95o <- 0
up95o <- 0
low95o <- 0
error68o <- 0
up68o <- 0
low68o <- 0

for (i in 1:13) {
  mNLo[i] = sum(as.numeric(mNLDEc[i], mNLFRc[i], mNLESc[i], mNLITc[i]))
}

mNLo[1:3] = mNLo[1:3] / 100

for (i in 1:13) {
  wNL[i] = sum(NL$Y, na.rm = TRUE) / sum(DE$Y, FR$Y, ES$Y, IT$Y)
  NLspillO[i] = mNLo[i] * wNL[i]
}

NLspillO

for (i in 1:13) {
  error95o[i] = qnorm(0.975) * sd(NLspillO) / sqrt(12)
  up95o[i] = mean(NLspillO[i]) + error95o[i]
  low95o[i] = mean(NLspillO[i]) - error95o[i]
  error68o[i] = qnorm(0.84) * sd(NLspillO) / sqrt(12)
  up68o[i] = mean(NLspillO[i]) + error68o[i]
  low68o[i] = mean(NLspillO[i]) - error68o[i]
}

# -- Cumulative multipliers
mNLoc1 <- cumsum(mNLo)[1]; print(mNLoc1)
mNLoc4 <- cumsum(mNLo)[5]; print(mNLoc4)
mNLoc8 <- cumsum(mNLo)[9]; print(mNLoc8)
mNLoc12 <- cumsum(mNLo)[13]; print(mNLoc12)

# -- Generate IRF graph (as in Fig. 3 of Alloza et al.)
v1 <- data.frame(cbind(NLspillO, up95o, low95o, up68o, low68o))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfNLo <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfNLo <- irfNLo + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfNLo1 <- irfNLo + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.005, 0.045, 0.01)) +
  ggtitle("Rest - GDP change (GOV shock in NL)")
irfNLo2 <- irfNLo1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                           panel.background = element_rect(fill = "white"), 
                           panel.border = element_rect(linetype = "solid", fill = NA))
irfNLo2
