# --- Spillovers by destination IT

source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers NL and IT v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers FR and IT v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers IT and DE v4 1.R')
source('~/Studie/MSc ECO/Period 5-6 MSc thesis/MSc thesis RStudio project/Scripts/Spillovers IT and ES v4 1.R')

library(lmtest)
library(purrr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)

mITd <- 0
gamma1IT <- 0
gamma2IT <- 0
ITspillD <- 0
error95d <- 0
up95d <- 0
low95d <- 0
error68d <- 0
up68d <- 0
low68d <- 0

for (i in 1:13) {
  mITd[i] = sum(as.numeric(mDEITc[i]), as.numeric(mFRITc[i]), as.numeric(mESITc[i]), as.numeric(mNLITc[i]))
  gamma1IT[i] = sum(as.numeric(gammaDEIT[i]), as.numeric(gammaESIT[i]), as.numeric(gammaFRIT[i]), as.numeric(gammaNLIT[i]))
  gamma2IT[i] = sum(as.numeric(gammaITDE[i]), as.numeric(gammaITES[i]), as.numeric(gammaITFR[i]), as.numeric(gammaITNL[i]))
  ITspillD[i] = mITd[i] * (gamma1IT[i] / gamma2IT[i])  
}

mITd
ITspillD

for (i in 1:13) {
  error95d[i] = qnorm(0.975) * sd(ITspillD) / sqrt(12)
  up95d[i] = mean(ITspillD[i]) + error95d[i]
  low95d[i] = mean(ITspillD[i]) - error95d[i]
  error68d[i] = qnorm(0.84) * sd(ITspillD) / sqrt(12)
  up68d[i] = mean(ITspillD[i]) + error68d[i]
  low68d[i] = mean(ITspillD[i]) - error68d[i]
}

# -- Cumulative multipliers
mITdc1 <- cumsum(mITd)[1]; print(mITdc1)
mITdc4 <- cumsum(mITd)[5]; print(mITdc4)
mITdc8 <- cumsum(mITd)[9]; print(mITdc8)
mITdc12 <- cumsum(mITd)[13]; print(mITdc12)

# -- Generate IRF graph (as in Fig. 2 of Alloza et al.)
v1 <- data.frame(cbind(ITspillD, up95d, low95d, up68d, low68d))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfITd <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfITd <- irfITd + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfITd1 <- irfITd + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.5, 0.25, 0.25)) +
  ggtitle("IT - GDP change (GOV shock in rest)")
irfITd2 <- irfITd1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                           panel.background = element_rect(fill = "white"), 
                           panel.border = element_rect(linetype = "solid", fill = NA))
irfITd2


# --- Spillovers by origin IT
mITo <- 0
wIT <- 0
ITspillO <- 0
error95o <- 0
up95o <- 0
low95o <- 0
error68o <- 0
up68o <- 0
low68o <- 0

for (i in 1:13) {
  mITo[i] = sum(as.numeric(mITDEc[i]), as.numeric(mITFRc[i]), as.numeric(mITESc[i]), (as.numeric(mITNLc[i]) / 100))
  wIT[i] = sum(IT$Y) / sum(DE$Y, FR$Y, ES$Y, NL$Y, na.rm = TRUE)
  ITspillO[i] = mITo[i] * wIT[i]
}

# Note: for mITNLc (shock / 100) was used instead of (shock) 

mITo[5] <- mITo[5] / 100

mITo
ITspillO

for (i in 1:13) {
  error95o[i] = qnorm(0.975) * sd(ITspillO) / sqrt(12)
  up95o[i] = mean(ITspillO[i]) + error95o[i]
  low95o[i] = mean(ITspillO[i]) - error95o[i]
  error68o[i] = qnorm(0.84) * sd(ITspillO) / sqrt(12)
  up68o[i] = mean(ITspillO[i]) + error68o[i]
  low68o[i] = mean(ITspillO[i]) - error68o[i]
}

# -- Cumulative multipliers
mIToc1 <- cumsum(mITo)[1]; print(mIToc1)
mIToc4 <- cumsum(mITo)[5]; print(mIToc4)
mIToc8 <- cumsum(mITo)[9]; print(mIToc8)
mIToc12 <- cumsum(mITo)[13]; print(mIToc12)

# -- Generate IRF graph (as in Fig. 3 of Alloza et al.)
v1 <- data.frame(cbind(ITspillO, up95o, low95o, up68o, low68o))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfITo <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfITo <- irfITo + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfITo1 <- irfITo + coord_cartesian(xlim = c(1, 12)) +
  scale_x_continuous(breaks = seq(1, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.4, 0.1, 0.05)) +
  ggtitle("Rest - GDP change (GOV shock in IT)")
irfITo2 <- irfITo1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                           panel.background = element_rect(fill = "white"), 
                           panel.border = element_rect(linetype = "solid", fill = NA))
irfITo2

# --- Domestic govt spending effects IT
ryIT.l <- data.frame(shift(data2$ryIT, n = 1:12, type = "lead"))
names(ryIT.l) = c("ryIT.l1", "ryIT.l2", "ryIT.l3", "ryIT.l4", "ryIT.l5", "ryIT.l6",
                  "ryIT.l7", "ryIT.l8", "ryIT.l9", "ryIT.l10", "ryIT.l11", "ryIT.l12")
rgIT.l <- data.frame(shift(data2$rgIT, n = 1:12, type = "lead"))
names(rgIT.l) = c("rgIT.l1", "rgIT.l2", "rgIT.l3", "rgIT.l4", "rgIT.l5", "rgIT.l6",
                  "rgIT.l7", "rgIT.l8", "rgIT.l9", "rgIT.l10", "rgIT.l11", "rgIT.l12")
l.ryITc <- data.frame(shift(data2$ryIT, n = 1:4, type = "lag"))
names(l.ryITc) = c("l1.ryITc", "l2.ryITc", "l3.ryITc", "l4.ryITc")
l.debtIT <- data.frame(shift(data2$debtIT, n = 1:4, type = "lag"))
names(l.debtIT) = c("l1.debtIT", "l2.debtIT", "l3.debtIT", "l4.debtIT")
l.intIT <- data.frame(shift(data2$intIT, n = 1:4, type = "lag"))
names(l.intIT) = c("l1.intIT", "l2.intIT", "l3.intIT", "l4.intIT")
l.lrtrIT <- data.frame(shift(data2$lrtrIT, n = 1:4, type = "lag"))
names(l.lrtrIT) = c("l1.lrtrIT", "l2.lrtrIT", "l3.lrtrIT", "l4.lrtrIT")
l.lrgIT <- data.frame(shift(data2$lrgIT, n = 1:4, type = "lag"))
names(l.lrgIT) = c("l1.lrgIT", "l2.lrgIT", "l3.lrgIT", "l4.lrgIT")
l.lryITc <- data.frame(shift(data2$lryITc, n = 1:4, type = "lag"))
names(l.lryITc) = c("l1.lryITc", "l2.lryITc", "l3.lryITc", "l4.lryITc")
data3 <- cbind(quarter = data2$quarter, rgIT = data2$rgIT, ryIT = data2$ryIT, debtIT = data2$debtIT, 
               intIT = data2$intIT, lrtrIT = data2$lrtrIT, lrgIT = data2$lrgIT, lryIT = data2$lryIT, 
               lryITc = data2$lryITc, shockIT3 = data2$shockIT3, l1.ryIT = data2$l1.ryIT, 
               l1.rgIT = data2$l1.rgIT, l.debtIT, l.intIT, l.lrtrIT, l.lrgIT, l.lryITc, ryIT.l, rgIT.l)
h <- 12

# -- Equation 5
lhsIT50 <- (data3$ryIT - data3$l1.ryIT) / data3$l1.ryIT
lhsIT5 <- lapply(1:h, function(x) (data3[, 32+x] - data3$l1.ryIT) / data3$l1.ryIT)
lhsIT5 <- data.frame(lhsIT5)
names(lhsIT5) = paste("lhsIT5", 1:h, sep = "")
data3 <- cbind(data3, lhsIT50, lhsIT5)
IT5 <- lapply(1:13, function(x) lm(data3[, 56+x] ~ shockIT3 + l1.debtIT + l1.intIT + l1.lrtrIT + l1.lrgIT + l1.lryITc + l2.debtIT + l2.intIT + l2.lrtrIT + l2.lrgIT + l2.lryITc + l3.debtIT + l3.intIT + l3.lrtrIT + l3.lrgIT + l3.lryITc + l4.debtIT + l4.intIT + l4.lrtrIT + l4.lrgIT + l4.lryITc, data = data3))
summariesIT5 <- lapply(IT5, summary)
IT5conf95 <- lapply(IT5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
IT5conf68 <- lapply(IT5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
IT5up95 <- lapply(1:13, function(x) IT5conf95[[x]][2,2])
IT5low95 <- lapply(1:13, function(x) IT5conf95[[x]][2,1])
IT5up68 <- lapply(1:13, function(x) IT5conf68[[x]][2,2])
IT5low68 <- lapply(1:13, function(x) IT5conf68[[x]][2,1])
betaIT <- lapply(summariesIT5, function(x) x$coefficients[2,1])
names(betaIT) <- paste("betaIT", 0:h, sep = "")

# -- Equation 6
lhsIT60 <- (data3$rgIT - data3$l1.rgIT) / data3$l1.ryIT
lhsIT6 <- lapply(1:h, function(x) (data3[, 44+x] - data3$l1.rgIT) / data3$l1.ryIT)
lhsIT6 <- data.frame(lhsIT6)
names(lhsIT6) = paste("lhsIT6", 1:h, sep = "")
data3 <- cbind(data3, lhsIT60, lhsIT6)
IT6 <- lapply(1:13, function(x) lm(data3[, 69+x] ~ shockIT3 + l1.debtIT + l1.intIT + l1.lrtrIT + l1.lrgIT + l1.lryITc + l2.debtIT + l2.intIT + l2.lrtrIT + l2.lrgIT + l2.lryITc + l3.debtIT + l3.intIT + l3.lrtrIT + l3.lrgIT + l3.lryITc + l4.debtIT + l4.intIT + l4.lrtrIT + l4.lrgIT + l4.lryITc, data = data3))
summariesIT6 <- lapply(IT6, summary)
IT6conf95 <- lapply(IT6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
IT6conf68 <- lapply(IT6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
IT6up95 <- lapply(1:13, function(x) IT6conf95[[x]][2,2])
IT6low95 <- lapply(1:13, function(x) IT6conf95[[x]][2,1])
IT6up68 <- lapply(1:13, function(x) IT6conf68[[x]][2,2])
ITlow68 <- lapply(1:13, function(x) IT6conf68[[x]][2,1])
gammaIT <- lapply(summariesIT6, function(x) x$coefficients[2,1])
names(gammaIT) <- paste("gammaIT", 0:h, sep = "")

# -- Domestic cumulative multiplier IT
mITc <- lapply(1:13, function(x) cumsum(as.numeric(betaIT))[x] / cumsum(as.numeric(gammaIT))[x])
mITc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaIT) / as.numeric(gammaIT))[x])
mITc <- unlist(mITc)

error95 <- 0
up95 <- 0
low95 <- 0
error68 <- 0
up68 <- 0
low68 <- 0

for (i in 1:13) {
  error95[i] = qnorm(0.975) * sd(mITc) / sqrt(12)
  up95[i] = mean(mITc[i]) + error95[i]
  low95[i] = mean(mITc[i]) - error95[i]
  error68[i] = qnorm(0.84) * sd(mITc) / sqrt(12)
  up68[i] = mean(mITc[i]) + error68[i]
  low68[i] = mean(mITc[i]) - error68[i]
}

# -- Generate IRF graph on domestic multipliers IT
v1 <- data.frame(cbind(mITc = unlist(mITc), up95, low95, up68, low68))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfIT <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfIT <- irfIT + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfIT1 <- irfIT + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-2, 6, 2)) +
  ggtitle("IT - multiplier (GOV shock in IT)")
irfIT2 <- irfIT1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                         panel.background = element_rect(fill = "white"), 
                         panel.border = element_rect(linetype = "solid", fill = NA))
irfIT2
