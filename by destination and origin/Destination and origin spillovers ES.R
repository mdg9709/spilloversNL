# --- Spillovers by destination ES

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers IT and ES v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers FR and ES v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers ES and DE v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers NL and ES v4 1.R')

library(lmtest)
library(purrr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)

mITES <- cumsum(betaITES) / cumsum(as.numeric(gammaITES) * 100)
mFRES <- cumsum(betaFRES) / cumsum(as.numeric(gammaFRES) * 100)
mDEES <- cumsum(betaDEES) / cumsum(as.numeric(gammaDEES) * 100)
mNLES <- cumsum(betaNLES) / cumsum(as.numeric(gammaNLES) * 100)

mESd <- 0
gamma1ES <- 0
gamma2ES <- 0
ESspillD <- 0
error95d <- 0
up95d <- 0
low95d <- 0
error68d <- 0
up68d <- 0
low68d <- 0

for (i in 1:13) {
  mESd[i] = sum(as.numeric(mITESc[i], mFRESc[i], mDEESc[i], mNLESc[i]))
  gamma1ES[i] = sum(gammaITES[[i]], gammaDEES[[i]], gammaFRES[[i]], gammaNLES[[i]]) 
  gamma2ES[i] = sum(gammaESIT[[i]], gammaESDE[[i]], gammaESFR[[i]], gammaESNL[[i]])
  ESspillD[i] = mESd[i] * (gamma1ES[i] / gamma2ES[i])
}

mESd
ESspillD

for (i in 1:13) {
  error95d[i] = qnorm(0.975) * sd(ESspillD) / sqrt(12)
  up95d[i] = mean(ESspillD[i]) + error95d[i]
  low95d[i] = mean(ESspillD[i]) - error95d[i]
  error68d[i] = qnorm(0.84) * sd(ESspillD) / sqrt(12)
  up68d[i] = mean(ESspillD[i]) + error68d[i]
  low68d[i] = mean(ESspillD[i]) - error68d[i]
}

# -- Cumulative multipliers
mESdc1 <- cumsum(mESd)[1]; print(mESdc1)
mESdc4 <- cumsum(mESd)[5]; print(mESdc4)
mESdc8 <- cumsum(mESd)[9]; print(mESdc8)
mESdc12 <- cumsum(mESd)[13]; print(mESdc12)

# -- Generate IRF graph (as in Fig. 2 of Alloza et al.)
v1 <- data.frame(cbind(ESspillD, up95d, low95d, up68d, low68d))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfESd <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfESd <- irfESd + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfESd1 <- irfESd + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.2, 0.1, 0.05)) +
  ggtitle("ES - GDP change (GOV shock in rest)")
irfESd2 <- irfESd1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                           panel.background = element_rect(fill = "white"), 
                           panel.border = element_rect(linetype = "solid", fill = NA))
irfESd2


# --- Spillovers by origin ES
mESIT <- cumsum(betaESIT) / cumsum(as.numeric(gammaESIT) * 100)
mESFR <- cumsum(betaESFR) / cumsum(as.numeric(gammaESFR) *100)
mESDE <- cumsum(betaESDE) / cumsum(as.numeric(gammaESDE) *100)
mESNL <- cumsum(betaESNL) / cumsum(as.numeric(gammaESNL) *100)

mESo <- 0
wES <- 0
ESspillO <- 0
error95o <- 0
up95o <- 0
low95o <- 0
error68o <- 0
up68o <- 0
low68o <- 0

for (i in 1:13) {
  mESo[i] = sum(as.numeric(mESITc[i], mESFRc[i], mESDEc[i], mESNLc[i]))
  wES[i] = sum(ES$Y) / sum(IT$Y, FR$Y, DE$Y, NL$Y, na.rm = TRUE)
  ESspillO[i] = mESo[i] * wES[i]
}

mESo

for (i in 1:13) {
  error95o[i] = qnorm(0.975) * sd(ESspillO, na.rm = TRUE) / sqrt(12)
  up95o[i] = mean(ESspillO[i]) + error95o[i]
  low95o[i] = mean(ESspillO[i]) - error95o[i]
  error68o[i] = qnorm(0.84) * sd(ESspillO, na.rm = TRUE) / sqrt(12)
  up68o[i] = mean(ESspillO[i]) + error68o[i]
  low68o[i] = mean(ESspillO[i]) - error68o[i]
}

# -- Cumulative multipliers
mESoc1 <- cumsum(mESo)[1]; print(mESoc1)
mESoc4 <- cumsum(mESo)[5]; print(mESoc4)
mESoc8 <- cumsum(mESo)[9]; print(mESoc8)
mESoc12 <- cumsum(mESo)[13]; print(mESoc12)

# -- Generate IRF graph (as in Fig. 3 of Alloza et al.)
v1 <- data.frame(cbind(ESspillO, up95o, low95o, up68o, low68o))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfESo <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfESo <- irfESo + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfESo1 <- irfESo + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.075, 0.005, 0.0025)) +
  ggtitle("Rest - GDP change (GOV shock in ES)")
irfESo2 <- irfESo1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                           panel.background = element_rect(fill = "white"), 
                           panel.border = element_rect(linetype = "solid", fill = NA))
irfESo2

# --- Domestic govt spending effects ES
ryES.l <- data.frame(shift(data2$ryES, n = 1:12, type = "lead"))
names(ryES.l) = c("ryES.l1", "ryES.l2", "ryES.l3", "ryES.l4", "ryES.l5", "ryES.l6",
                  "ryES.l7", "ryES.l8", "ryES.l9", "ryES.l10", "ryES.l11", "ryES.l12")
rgES.l <- data.frame(shift(data2$rgES, n = 1:12, type = "lead"))
names(rgES.l) = c("rgES.l1", "rgES.l2", "rgES.l3", "rgES.l4", "rgES.l5", "rgES.l6",
                  "rgES.l7", "rgES.l8", "rgES.l9", "rgES.l10", "rgES.l11", "rgES.l12")
l.ryESc <- data.frame(shift(data2$ryES, n = 1:4, type = "lag"))
names(l.ryESc) = c("l1.ryESc", "l2.ryESc", "l3.ryESc", "l4.ryESc")
l.debtES <- data.frame(shift(data2$debtES, n = 1:4, type = "lag"))
names(l.debtES) = c("l1.debtES", "l2.debtES", "l3.debtES", "l4.debtES")
l.intES <- data.frame(shift(data2$intES, n = 1:4, type = "lag"))
names(l.intES) = c("l1.intES", "l2.intES", "l3.intES", "l4.intES")
l.lrtrES <- data.frame(shift(data2$lrtrES, n = 1:4, type = "lag"))
names(l.lrtrES) = c("l1.lrtrES", "l2.lrtrES", "l3.lrtrES", "l4.lrtrES")
l.lrgES <- data.frame(shift(data2$lrgES, n = 1:4, type = "lag"))
names(l.lrgES) = c("l1.lrgES", "l2.lrgES", "l3.lrgES", "l4.lrgES")
l.lryESc <- data.frame(shift(data2$lryESc, n = 1:4, type = "lag"))
names(l.lryESc) = c("l1.lryESc", "l2.lryESc", "l3.lryESc", "l4.lryESc")
data3 <- cbind(quarter = data2$quarter, ryES = data2$ryES, rgES = data2$rgES, debtES = data2$debtES, 
               intES = data2$intES, lrtrES = data2$lrtrES, lrgES = data2$lrgES, lryES = data2$lryES, 
               lryESc = data2$lryESc, shockES3 = data2$shockES3, l1.ryES = data2$l1.ryES, l1.rgES = data2$l1.rgES, 
               l.debtES, l.intES, l.lrtrES, l.lrgES, l.lryESc, ryES.l, rgES.l)
h <- 12

# -- Equation 5
lhsES50 <- (data3$ryES - data3$l1.ryES) / data3$l1.ryES
lhsES5 <- lapply(1:h, function(x) (data3[, 32+x] - data3$l1.ryES) / data3$l1.ryES)
lhsES5 <- data.frame(lhsES5)
names(lhsES5) = paste("lhsES5", 1:h, sep = "")
data3 <- cbind(data3, lhsES50, lhsES5)
ES5 <- lapply(1:13, function(x) lm(data3[, 56+x] ~ shockES3 + l1.debtES + l1.intES + l1.lrtrES + l1.lrgES + l1.lryESc + l2.debtES + l2.intES + l2.lrtrES + l2.lrgES + l2.lryESc + l3.debtES + l3.intES + l3.lrtrES + l3.lrgES + l3.lryESc + l4.debtES + l4.intES + l4.lrtrES + l4.lrgES + l4.lryESc, data = data3))
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
lhsES60 <- (data3$rgES - data3$l1.rgES) / data3$l1.ryES
lhsES6 <- lapply(1:h, function(x) (data3[, 44+x] - data3$l1.rgES) / data3$l1.ryES)
lhsES6 <- data.frame(lhsES6)
names(lhsES6) = paste("lhsES6", 1:h, sep = "")
data3 <- cbind(data3, lhsES60, lhsES6)
ES6 <- lapply(1:13, function(x) lm(data3[, 69+x] ~ shockES3 + l1.debtES + l1.intES + l1.lrtrES + l1.lrgES + l1.lryESc + l2.debtES + l2.intES + l2.lrtrES + l2.lrgES + l2.lryESc + l3.debtES + l3.intES + l3.lrtrES + l3.lrgES + l3.lryESc + l4.debtES + l4.intES + l4.lrtrES + l4.lrgES + l4.lryESc, data = data3))
summariesES6 <- lapply(ES6, summary)
ES6conf95 <- lapply(ES6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ES6conf68 <- lapply(ES6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ES6up95 <- lapply(1:13, function(x) ES6conf95[[x]][2,2])
ES6low95 <- lapply(1:13, function(x) ES6conf95[[x]][2,1])
ES6up68 <- lapply(1:13, function(x) ES6conf68[[x]][2,2])
ESlow68 <- lapply(1:13, function(x) ES6conf68[[x]][2,1])
gammaES <- lapply(summariesES6, function(x) x$coefficients[2,1])
names(gammaES) <- paste("gammaES", 0:h, sep = "")

# -- Domestic cumulative multiplier ES
mESc <- lapply(1:13, function(x) cumsum(as.numeric(betaES))[x] / cumsum(as.numeric(gammaES))[x])
mESc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaES) / as.numeric(gammaES))[x])
