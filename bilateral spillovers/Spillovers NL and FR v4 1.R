# --- Effect of Dutch govt spending shock on France

# Data period: 1980q1-2018q4 and 1999q1-2018q4
# 95% and 68% confidence intervals
# h = 4, 8 and 12

# OLS with left-hand side in growth rates, 4 lags of x(t-1), and shock2 * (100)^3

# Import data
library(readxl)
DE <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/Quarterly fiscal database 1980q1-2018q4.xlsx",
                 sheet = "DE")
IT <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/Quarterly fiscal database 1980q1-2018q4.xlsx",
                 sheet = "IT")
FR <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/Quarterly fiscal database 1980q1-2018q4.xlsx",
                 sheet = "FR")
ES <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/Quarterly fiscal database 1980q1-2018q4.xlsx",
                 sheet = "ES")
NL <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/Quarterly fiscal database 1999q1-2018q4.xlsx",
                 sheet = "NL")
oNL <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/ABP_other_variables.xlsx", 
                  sheet = "NL1", range = "B1:F157")
colnames(oNL) <- c("rxNL", "rmNL", "R", "D", "pop")
oFR <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/ABP_other_variables.xlsx", 
                  sheet = "FR", range = "B1:F157")
colnames(oFR) <- c("rxFR", "rmFR", "rateFR", "dFR", "popFR")


# Load packages
library(ggplot2)
library(gridExtra)
library(dplyr)
library(lmtest)
library(sandwich)
h <- 12

# Create dataframe
shockES <- ES$shockES
shockIT <- IT$shockIT
shockDE <- DE$shockDEq
data <- cbind(FR, NL, oNL, oFR, shockIT, shockES, shockDE)
data1 <- subset(data, select = -c(2:14, 28:41, 55:56))
names(data1)[2] <- "debtFR"
names(data1)[14] <- "shockFR"
names(data1)[15] <- "debtNL"
names(data1)[27] <- "shockNL"

# -- Re-scaling of left-hand side of Equations (5) and (6):
data2 <- data1 %>%
  mutate(l1.rgFR = lag(rgFR), l1.rgNL = lag(rgNL), l1.ryFR = lag(ryFR), l1.ryNL = lag(ryNL)) %>%
  mutate(l1.ryIT = lag(IT$ryIT), l1.ryES = lag(ES$ryES), l1.ryDE = lag(DE$ryDE)) %>%
  mutate(l1.lrgFR = log(l1.rgFR), l1.lrgNL = log(l1.rgNL), l1.lryFR = log(l1.ryFR), l1.lryNLc = log(l1.ryNL)) %>%
  mutate(shockNL2 = (shockNL / l1.ryFR) / sd((shockNL / l1.ryFR), na.rm = TRUE), 
         shockFR2 = (shockFR / l1.ryNL) / sd((shockFR / l1.ryNL), na.rm = TRUE),
         shockIT2 = (shockIT / l1.ryIT) / sd((shockIT / l1.ryIT), na.rm = TRUE), 
         shockDE2 = (shockDE / l1.ryDE) / sd((shockDE / l1.ryDE), na.rm = TRUE),
         shockES2 = (shockES / l1.ryES) / sd((shockES / l1.ryES), na.rm = TRUE)) %>% 
  mutate(shockIT3 = shockIT2 / 100, shockES3 = shockES2 / 100, shockFR3 = shockFR2 / 100, 
         shockDE3 = shockDE2 / 100, shockNL3 = shockNL2 / 100) %>%
  mutate(shockIT4 = shockIT2 / 10000, shockES4 = shockES2 / 10000, shockFR4 = shockFR2 / 10000, 
         shockDE4 = shockDE2 / 10000, shockNL4 = shockNL2 / 10000) %>%
  mutate(ryFR.l1 = lead(ryFR), ryFR.l2 = lead(ryFR, n = 2), ryFR.l3 = lead(ryFR, n = 3), 
         ryFR.l4 = lead(ryFR, n = 4), ryFR.l5 = lead(ryFR, n = 5), ryFR.l6 = lead(ryFR, n = 6),
         ryFR.l7 = lead(ryFR, n = 7), ryFR.l8 = lead(ryFR, n = 8), ryFR.l9 = lead(ryFR, n = 9),
         ryFR.l10 = lead(ryFR, n = 10), ryFR.l11 = lead(ryFR, n = 11), ryFR.l12 = lead(ryFR, n = 12)) %>%
  mutate(ryNL.l1 = lead(ryNL), ryNL.l2 = lead(ryNL, n = 2), ryNL.l3 = lead(ryNL, n = 3), 
         ryNL.l4 = lead(ryNL, n = 4), ryNL.l5 = lead(ryNL, n = 5), ryNL.l6 = lead(ryNL, n = 6),
         ryNL.l7 = lead(ryNL, n = 7), ryNL.l8 = lead(ryNL, n = 8), ryNL.l9 = lead(ryNL, n = 9),
         ryNL.l10 = lead(ryNL, n = 10), ryNL.l11 = lead(ryNL, n = 11), ryNL.l12 = lead(ryNL, n = 12)) %>%
  mutate(rgFR.l1 = lead(rgFR), rgFR.l2 = lead(rgFR, n = 2), rgFR.l3 = lead(rgFR, n = 3), 
         rgFR.l4 = lead(rgFR, n = 4), rgFR.l5 = lead(rgFR, n = 5), rgFR.l6 = lead(rgFR, n = 6),
         rgFR.l7 = lead(rgFR, n = 7), rgFR.l8 = lead(rgFR, n = 8), rgFR.l9 = lead(rgFR, n = 9),
         rgFR.l10 = lead(rgFR, n = 10), rgFR.l11 = lead(rgFR, n = 11), rgFR.l12 = lead(rgFR, n = 12)) %>%
  mutate(rgNL.l1 = lead(rgNL), rgNL.l2 = lead(rgNL, n = 2), rgNL.l3 = lead(rgNL, n = 3), 
         rgNL.l4 = lead(rgNL, n = 4), rgNL.l5 = lead(rgNL, n = 5), rgNL.l6 = lead(rgNL, n = 6),
         rgNL.l7 = lead(rgNL, n = 7), rgNL.l8 = lead(rgNL, n = 8), rgNL.l9 = lead(rgNL, n = 9),
         rgNL.l10 = lead(rgNL, n = 10), rgNL.l11 = lead(rgNL, n = 11), rgNL.l12 = lead(rgNL, n = 12)) %>%
  mutate(l1.debtFR = lag(debtFR, n = 1), l1.intFR = lag(intFR, n = 1), l1.lrtrFR = lag(lrtrFR, n = 1),
         l1.lrgFR = lag(lrgFR, n = 1), l1.lryFRc = lag(lryFRc, n = 1), l2.debtFR = lag(debtFR, n = 2),
         l2.intFR = lag(intFR, n = 2), l2.lrtrFR = lag(lrtrFR, n = 2), l2.lrgFR = lag(lrgFR, n = 2),
         l2.lryFRc = lag(lryFRc, n = 2), l3.debtFR = lag(debtFR, n = 3), l3.intFR = lag(intFR, n = 3),
         l3.lrtrFR = lag(lrtrFR, n = 3), l3.lrgFR = lag(lrgFR, n = 3), l3.lryFRc = lag(lryFRc, n = 3),
         l4.debtFR = lag(debtFR, n = 4), l4.intFR = lag(intFR, n = 4), l4.lrtrFR = lag(lrtrFR, n = 4),
         l4.lrgFR = lag(lrgFR, n = 4), l4.lryFRc = lag(lryFRc, n = 4)) %>%
  mutate(l1.debtNL = lag(debtNL, n = 1), l1.intNL = lag(intNL, n = 1), l1.lrtrNL = lag(lrtrNL, n = 1),
         l1.lrgNL = lag(lrgNL, n = 1), l1.lryNLc = lag(lryNLc, n = 1), l2.debtNL = lag(debtNL, n = 2),
         l2.intNL = lag(intNL, n = 2), l2.lrtrNL = lag(lrtrNL, n = 2), l2.lrgNL = lag(lrgNL, n = 2),
         l2.lryNLc = lag(lryNLc, n = 2), l3.debtNL = lag(debtNL, n = 3), l3.intNL = lag(intNL, n = 3),
         l3.lrtrNL = lag(lrtrNL, n = 3), l3.lrgNL = lag(lrgNL, n = 3), l3.lryNLc = lag(lryNLc, n = 3),
         l4.debtNL = lag(debtNL, n = 4), l4.intNL = lag(intNL, n = 4), l4.lrtrNL = lag(lrtrNL, n = 4),
         l4.lrgNL = lag(lrgNL, n = 4), l4.lryNLc = lag(lryNLc, n = 4))

# -- OLS regressions

# -- Equation 5
lhsNLFR50 <- (data2$ryFR - data2$l1.ryFR) / data2$l1.ryFR
lhsNLFR5 <- lapply(1:h, function(x) (data2[, 66+x] - data2$l1.ryFR) / data2$l1.ryFR)
lhsNLFR5 <- data.frame(lhsNLFR5)
names(lhsNLFR5) = paste("lhsNLFR5", 1:h, sep = "")
data3 <- cbind(data2, lhsNLFR50, lhsNLFR5)
NLFR5 <- lapply(1:13, function(x) lm(data3[, 151+x] ~ shockNL2 + l1.debtFR + l1.intFR + l1.lrtrFR + l1.lrgFR + l1.lryFRc + l2.debtFR + l2.intFR + l2.lrtrFR + l2.lrgFR + l2.lryFRc + l3.debtFR + l3.intFR + l3.lrtrFR + l3.lrgFR + l3.lryFRc + l4.debtFR + l4.intFR + l4.lrtrFR + l4.lrgFR + l4.lryFRc + shockDE2 + shockFR2 + shockES2 + shockIT2, data = data3))
summariesNLFR5 <- lapply(NLFR5, summary)
NLFR5conf95 <- lapply(NLFR5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
NLFR5conf68 <- lapply(NLFR5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
NLFR5up95 <- lapply(1:13, function(x) NLFR5conf95[[x]][2,2])
NLFR5low95 <- lapply(1:13, function(x) NLFR5conf95[[x]][2,1])
NLFR5up68 <- lapply(1:13, function(x) NLFR5conf68[[x]][2,2])
NLFR5low68 <- lapply(1:13, function(x) NLFR5conf68[[x]][2,1])
betaNLFR <- lapply(summariesNLFR5, function(x) x$coefficients[2,1])
names(betaNLFR) <- paste("betaNLFR", 0:h, sep = "")

# -- Equation 6
lhsNLFR60 <- (data3$rgNL - data3$l1.rgNL) / data3$l1.ryFR
lhsNLFR6 <- lapply(1:h, function(x) (data3[, 102+x] - data3$l1.rgNL) / data3$l1.ryFR)
lhsNLFR6 <- data.frame(lhsNLFR6)
names(lhsNLFR6) = paste("lhsNLFR6", 1:h, sep = "")
data3 <- cbind(data3, lhsNLFR60, lhsNLFR6)
NLFR6 <- lapply(1:13, function(x) lm(data3[, 164+x] ~ shockNL3 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc + shockDE3 + shockFR3 + shockES3 + shockIT3, data = data3))
summariesNLFR6 <- lapply(NLFR6, summary)
NLFR6conf95 <- lapply(NLFR6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
NLFR6conf68 <- lapply(NLFR6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
NLFR6up95 <- lapply(1:13, function(x) NLFR6conf95[[x]][2,2])
NLFR6low95 <- lapply(1:13, function(x) NLFR6conf95[[x]][2,1])
NLFR6up68 <- lapply(1:13, function(x) NLFR6conf68[[x]][2,2])
NLFR6low68 <- lapply(1:13, function(x) NLFR6conf68[[x]][2,1])
gammaNLFR <- lapply(summariesNLFR6, function(x) x$coefficients[2,1])
names(gammaNLFR) <- paste("gammaNLFR", 0:h, sep = "")

# -- Cumulative multiplier
mNLFRc <- lapply(1:13, function(x) cumsum(as.numeric(betaNLFR))[x] / cumsum(as.numeric(gammaNLFR))[x])
mNLFRc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaNLFR) / as.numeric(gammaNLFR))[x])

# -- Generate IRF graph (as in Fig. 1 of Alloza et al.)
v1 <- data.frame(cbind(betaNLFR = unlist(betaNLFR), NLFR5up95 = unlist(NLFR5up95), NLFR5low95 = unlist(NLFR5low95), 
                       NLFR5up68 = unlist(NLFR5up68), NLFR5low68 = unlist(NLFR5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfNLFR <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfNLFR <- irfNLFR + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfNLFR1 <- irfNLFR + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.007, 0.003, 0.002)) +
  ggtitle("FR - GDP change (GOV shock in NL)")
irfNLFR2 <- irfNLFR1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfNLFR2

# -- Generate IRF graph of GOV response
v1 <- data.frame(cbind(gammaNLFR = unlist(gammaNLFR), NLFR6up95 = unlist(NLFR6up95), NLFR6low95 = unlist(NLFR6low95), 
                       NLFR6up68 = unlist(NLFR6up68), NLFR6low68 = unlist(NLFR6low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfNLFR3 <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfNLFR3 <- irfNLFR3 + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfNLFR4 <- irfNLFR3 + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.05, 0.15, 0.05)) +
  ggtitle("NL - GOV change (GOV shock in NL)")
irfNLFR5 <- irfNLFR4 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfNLFR5


# --- Effect of French govt spending shock on Netherlands

# -- Equation 5
lhsFRNL50 <- (data3$ryNL - data3$l1.ryNL) / data3$l1.ryNL
lhsFRNL5 <- lapply(1:h, function(x) (data3[, 78+x] - data3$l1.ryNL) / data3$l1.ryNL)
lhsFRNL5 <- data.frame(lhsFRNL5)
names(lhsFRNL5) = paste("lhsFRNL5", 1:h, sep = "")
data3 <- cbind(data3, lhsFRNL50, lhsFRNL5)
FRNL5 <- lapply(1:13, function(x) lm(data3[, 177+x] ~ shockFR2 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc + shockDE2 + shockES2 + shockIT2 + shockNL2, data = data3))
summariesFRNL5 <- lapply(FRNL5, summary)
FRNL5conf95 <- lapply(FRNL5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
FRNL5conf68 <- lapply(FRNL5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
FRNL5up95 <- lapply(1:13, function(x) FRNL5conf95[[x]][2,2])
FRNL5low95 <- lapply(1:13, function(x) FRNL5conf95[[x]][2,1])
FRNL5up68 <- lapply(1:13, function(x) FRNL5conf68[[x]][2,2])
FRNL5low68 <- lapply(1:13, function(x) FRNL5conf68[[x]][2,1])
betaFRNL <- lapply(summariesFRNL5, function(x) x$coefficients[2,1])
names(betaFRNL) <- paste("betaFRNL", 0:h, sep = "")

# -- Equation 6
lhsFRNL60 <- (data3$rgFR - data3$l1.rgFR) / data3$l1.ryNL
lhsFRNL6 <- lapply(1:h, function(x) (data3[, 90+x] - data3$l1.rgFR) / data3$l1.ryNL)
lhsFRNL6 <- data.frame(lhsFRNL6)
names(lhsFRNL6) = paste("lhsFRNL6", 1:h, sep = "")
data3 <- cbind(data3, lhsFRNL60, lhsFRNL6)
FRNL6 <- lapply(1:13, function(x) lm(data3[, 190+x] ~ shockFR3 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc + shockDE3 + shockES3 + shockIT3 + shockNL3, data = data3))
summariesFRNL6 <- lapply(FRNL6, summary)
FRNL6conf95 <- lapply(FRNL6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
FRNL6conf68 <- lapply(FRNL6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
FRNL6up95 <- lapply(1:13, function(x) FRNL6conf95[[x]][2,2])
FRNL6low95 <- lapply(1:13, function(x) FRNL6conf95[[x]][2,1])
FRNL6up68 <- lapply(1:13, function(x) FRNL6conf68[[x]][2,2])
FRNL6low68 <- lapply(1:13, function(x) FRNL6conf68[[x]][2,1])
gammaFRNL <- lapply(summariesFRNL6, function(x) x$coefficients[2,1])
names(gammaFRNL) <- paste("gammaFRNL", 0:h, sep = "")

# -- Cumulative multiplier
mFRNLc <- lapply(1:13, function(x) cumsum(as.numeric(betaFRNL))[x] / cumsum(as.numeric(gammaFRNL))[x])
mFRNLc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaFRNL) / as.numeric(gammaFRNL))[x])

# -- Generate IRF graph (as in Fig. 1 of Alloza et al.)
v1 <- data.frame(cbind(betaFRNL = unlist(betaFRNL), FRNL5up95 = unlist(FRNL5up95), FRNL5low95 = unlist(FRNL5low95), 
                       FRNL5up68 = unlist(FRNL5up68), FRNL5low68 = unlist(FRNL5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfFRNL <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfFRNL <- irfFRNL + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfFRNL1 <- irfFRNL + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.006, 0.003, 0.003)) +
  ggtitle("NL - GDP change (GOV shock in FR)")
irfFRNL2 <- irfFRNL1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfFRNL2

# -- Generate IRF graph of GOV response
v1 <- data.frame(cbind(gammaFRNL = unlist(gammaFRNL), FRNL6up95 = unlist(FRNL6up95), FRNL6low95 = unlist(FRNL6low95), 
                       FRNL6up68 = unlist(FRNL6up68), FRNL6low68 = unlist(FRNL6low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfFRNL3 <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfFRNL3 <- irfFRNL3 + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfFRNL4 <- irfFRNL3 + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.8, 0.6, 0.2)) +
  ggtitle("FR - GOV change (GOV shock in FR)")
irfFRNL5 <- irfFRNL4 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfFRNL5
