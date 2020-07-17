# --- Spillovers by destination DE

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers NL and DE v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers FR and DE v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers IT and DE v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers ES and DE v4 1.R')

library(lmtest)
library(purrr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)

mDEd <- 0
gamma1DE <- 0
gamma2DE <- 0
DEspillD <- 0
error95d <- 0
up95d <- 0
low95d <- 0
error68d <- 0
up68d <- 0
low68d <- 0

for (i in 1:13) {
  mDEd[i] = sum(as.numeric(mITDEc[i], mFRDEc[i], mESDEc[i], mNLDEc[i]))
  gamma1DE[i] = sum(as.numeric(gammaITDE[i], gammaESDE[i], gammaFRDE[i], gammaNLDE[i])) 
  gamma2DE[i] = sum(as.numeric(gammaDEIT[i], gammaDEES[i], gammaDEFR[i], gammaDENL[i]))
}

mDEd

for (i in 1:13) {
  DEspillD[i] = mDEd[i] * (gamma1DE[i] / gamma2DE[i])
}

DEspillD

for (i in 1:13) {
  error95d[i] = qnorm(0.975) * sd(DEspillD) / sqrt(12)
  up95d[i] = mean(DEspillD[i]) + error95d[i]
  low95d[i] = mean(DEspillD[i]) - error95d[i]
  error68d[i] = qnorm(0.84) * sd(DEspillD) / sqrt(12)
  up68d[i] = mean(DEspillD[i]) + error68d[i]
  low68d[i] = mean(DEspillD[i]) - error68d[i]
}

# -- Cumulative multipliers
mDEdc1 <- cumsum(mDEd)[1]; print(mDEdc1)
mDEdc4 <- cumsum(mDEd)[5]; print(mDEdc4)
mDEdc8 <- cumsum(mDEd)[9]; print(mDEdc8)
mDEdc12 <- cumsum(mDEd)[13]; print(mDEdc12)

# -- Generate IRF graph (as in Fig. 2 of Alloza et al.)
v1 <- data.frame(cbind(DEspillD, up95d, low95d, up68d, low68d))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfDEd <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfDEd <- irfDEd + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfDEd1 <- irfDEd + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.4, 0.2, 0.2)) +
  ggtitle("DE - GDP change (GOV shock in rest)")
irfDEd2 <- irfDEd1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                           panel.background = element_rect(fill = "white"), 
                           panel.border = element_rect(linetype = "solid", fill = NA))
irfDEd2


# --- Spillovers by origin DE
mDEo <- 0
wDE <- 0
DEspillO <- 0
error95o <- 0
up95o <- 0
low95o <- 0
error68o <- 0
up68o <- 0
low68o <- 0

for (i in 1:13) {
  mDEo[i] = sum(as.numeric(mDEITc[i], mDEFRc[i], mDEESc[i], mDENLc[i]))
  wDE[i] = sum(DE$Y) / sum(IT$Y, FR$Y, ES$Y, NL$Y, na.rm = TRUE)
  DEspillO[i] = mDEo[i] * wDE[i]
}

mDEo
DEspillO

for (i in 1:13) {
  error95o[i] = qnorm(0.975) * sd(DEspillO) / sqrt(12)
  up95o[i] = mean(DEspillO[i]) + error95o[i]
  low95o[i] = mean(DEspillO[i]) - error95o[i]
  error68o[i] = qnorm(0.84) * sd(DEspillO) / sqrt(12)
  up68o[i] = mean(DEspillO[i]) + error68o[i]
  low68o[i] = mean(DEspillO[i]) - error68o[i] 
}

# -- Cumulative multipliers
mDEoc1 <- cumsum(mDEo)[1]; print(mDEoc1)
mDEoc4 <- cumsum(mDEo)[5]; print(mDEoc4)
mDEoc8 <- cumsum(mDEo)[9]; print(mDEoc8)
mDEoc12 <- cumsum(mDEo)[1]; print(mDEoc12)

# -- Generate IRF graph (as in Fig. 3 of Alloza et al.)
v1 <- data.frame(cbind(DEspillO, up95o, low95o, up68o, low68o))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfDEo <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfDEo <- irfDEo + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfDEo1 <- irfDEo + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.1, 0.3, 0.1)) +
  ggtitle("Rest - GDP change (GOV shock in DE)")
irfDEo2 <- irfDEo1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                           panel.background = element_rect(fill = "white"), 
                           panel.border = element_rect(linetype = "solid", fill = NA))
irfDEo2


# --- Domestic govt spending effects DE
ryDE.l <- data.frame(shift(data2$ryDE, n = 1:12, type = "lead"))
names(ryDE.l) = c("ryDE.l1", "ryDE.l2", "ryDE.l3", "ryDE.l4", "ryDE.l5", "ryDE.l6",
                  "ryDE.l7", "ryDE.l8", "ryDE.l9", "ryDE.l10", "ryDE.l11", "ryDE.l12")
rgDE.l <- data.frame(shift(data2$rgDE, n = 1:12, type = "lead"))
names(rgDE.l) = c("rgDE.l1", "rgDE.l2", "rgDE.l3", "rgDE.l4", "rgDE.l5", "rgDE.l6",
                  "rgDE.l7", "rgDE.l8", "rgDE.l9", "rgDE.l10", "rgDE.l11", "rgDE.l12")
l.ryDEc <- data.frame(shift(data2$ryDE, n = 1:4, type = "lag"))
names(l.ryDEc) = c("l1.ryDEc", "l2.ryDEc", "l3.ryDEc", "l4.ryDEc")
l.debtDE <- data.frame(shift(data2$debtDE, n = 1:4, type = "lag"))
names(l.debtDE) = c("l1.debtDE", "l2.debtDE", "l3.debtDE", "l4.debtDE")
l.intDE <- data.frame(shift(data2$intDE, n = 1:4, type = "lag"))
names(l.intDE) = c("l1.intDE", "l2.intDE", "l3.intDE", "l4.intDE")
l.lrtrDE <- data.frame(shift(data2$lrtrDE, n = 1:4, type = "lag"))
names(l.lrtrDE) = c("l1.lrtrDE", "l2.lrtrDE", "l3.lrtrDE", "l4.lrtrDE")
l.lrgDE <- data.frame(shift(data2$lrgDE, n = 1:4, type = "lag"))
names(l.lrgDE) = c("l1.lrgDE", "l2.lrgDE", "l3.lrgDE", "l4.lrgDE")
l.lryDEc <- data.frame(shift(data2$lryDEc, n = 1:4, type = "lag"))
names(l.lryDEc) = c("l1.lryDEc", "l2.lryDEc", "l3.lryDEc", "l4.lryDEc")
data3 <- cbind(quarter = data2$quarter, ryDE = data2$ryDE, rgDE = data2$rgDE, debtDE = data2$debtDE, 
               intDE = data2$intDE, lrtrDE = data2$lrtrDE, lrgDE = data2$lrgDE, lryDE = data2$lryDE, 
               lryDEc = data2$lryDEc, shockDE3 = data2$shockDE3, l1.ryDE = data2$l1.ryDE, l1.rgDE = data2$l1.rgDE, 
               l.debtDE, l.intDE, l.lrtrDE, l.lrgDE, l.lryDEc, ryDE.l, rgDE.l)
h <- 12

# -- Equation 5
lhsDE50 <- (data3$ryDE - data3$l1.ryDE) / data3$l1.ryDE
lhsDE5 <- lapply(1:h, function(x) (data3[, 32+x] - data3$l1.ryDE) / data3$l1.ryDE)
lhsDE5 <- data.frame(lhsDE5)
names(lhsDE5) = paste("lhsDE5", 1:h, sep = "")
data3 <- cbind(data3, lhsDE50, lhsDE5)
DE5 <- lapply(1:13, function(x) lm(data3[, 56+x] ~ shockDE3 + l1.debtDE + l1.intDE + l1.lrtrDE + l1.lrgDE + l1.lryDEc + l2.debtDE + l2.intDE + l2.lrtrDE + l2.lrgDE + l2.lryDEc + l3.debtDE + l3.intDE + l3.lrtrDE + l3.lrgDE + l3.lryDEc + l4.debtDE + l4.intDE + l4.lrtrDE + l4.lrgDE + l4.lryDEc, data = data3))
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
lhsDE60 <- (data3$rgDE - data3$l1.rgDE) / data3$l1.ryDE
lhsDE6 <- lapply(1:h, function(x) (data3[, 44+x] - data3$l1.rgDE) / data3$l1.ryDE)
lhsDE6 <- data.frame(lhsDE6)
names(lhsDE6) = paste("lhsDE6", 1:h, sep = "")
data3 <- cbind(data3, lhsDE60, lhsDE6)
DE6 <- lapply(1:13, function(x) lm(data3[, 69+x] ~ shockDE3 + l1.debtDE + l1.intDE + l1.lrtrDE + l1.lrgDE + l1.lryDEc + l2.debtDE + l2.intDE + l2.lrtrDE + l2.lrgDE + l2.lryDEc + l3.debtDE + l3.intDE + l3.lrtrDE + l3.lrgDE + l3.lryDEc + l4.debtDE + l4.intDE + l4.lrtrDE + l4.lrgDE + l4.lryDEc, data = data3))
summariesDE6 <- lapply(DE6, summary)
DE6conf95 <- lapply(DE6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
DE6conf68 <- lapply(DE6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
DE6up95 <- lapply(1:13, function(x) DE6conf95[[x]][2,2])
DE6low95 <- lapply(1:13, function(x) DE6conf95[[x]][2,1])
DE6up68 <- lapply(1:13, function(x) DE6conf68[[x]][2,2])
DE6low68 <- lapply(1:13, function(x) DE6conf68[[x]][2,1])
gammaDE <- lapply(summariesDE6, function(x) x$coefficients[2,1])
names(gammaDE) <- paste("gammaDE", 0:h, sep = "")

# -- Domestic cumulative multiplier DE
mDEc <- lapply(1:13, function(x) cumsum(as.numeric(betaDE))[x] / cumsum(as.numeric(gammaDE))[x])
mDEc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaDE) / as.numeric(gammaDE))[x])
mDEc <- unlist(mDEc)

error95 <- 0
up95 <- 0
low95 <- 0
error68 <- 0
up68 <- 0
low68 <- 0

for (i in 1:13) {
  error95[i] = qnorm(0.975) * sd(mDEc) / sqrt(12)
  up95[i] = mean(mDEc[i]) + error95[i]
  low95[i] = mean(mDEc[i]) - error95[i]
  error68[i] = qnorm(0.84) * sd(mDEc) / sqrt(12)
  up68[i] = mean(mDEc[i]) + error68[i]
  low68[i] = mean(mDEc[i]) - error68[i]
}

# -- Generate IRF graph on domestic multipliers DE
v1 <- data.frame(cbind(mDEc = unlist(mDEc), up95, low95, up68, low68))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfDE <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfDE <- irfDE + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfDE1 <- irfDE + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-2, 6, 2)) +
  ggtitle("DE - multiplier (GOV shock in DE)")
irfDE2 <- irfDE1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                         panel.background = element_rect(fill = "white"), 
                         panel.border = element_rect(linetype = "solid", fill = NA))
irfDE2
