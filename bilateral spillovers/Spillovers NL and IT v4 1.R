# --- Effect of Dutch govt spending shock on Italy

# Data period:1980q1-2018q4 and  1999q1-2018q4
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
oIT <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/ABP_other_variables.xlsx", 
                  sheet = "IT", range = "B1:F157")
colnames(oIT) <- c("rxIT", "rmIT", "R", "D", "pop")


# Load packages
library(ggplot2)
library(gridExtra)
library(dplyr)
library(lmtest)
library(sandwich)
h <- 12

# Create dataframe
shockFR <- FR$shockFR
shockES <- ES$shockES
shockDE <- DE$shockDEq
data <- cbind(IT, NL, oNL, oIT, shockES, shockFR, shockDE)
data1 <- subset(data, select = -c(2:14, 28:41, 55:56))
names(data1)[2] <- "debtIT"
names(data1)[14] <- "shockIT"
names(data1)[15] <- "debtNL"
names(data1)[27] <- "shockNL"

# -- Re-scaling of left-hand side of Equations (5) and (6):
data2 <- data1 %>%
  mutate(l1.rgIT = lag(rgIT), l1.rgNL = lag(rgNL), l1.ryIT = lag(ryIT), l1.ryNL = lag(ryNL)) %>%
  mutate(l1.ryES = lag(ES$ryES), l1.ryFR = lag(FR$ryFR), l1.ryDE = lag(DE$ryDE)) %>%
  mutate(l1.lrgIT = log(l1.rgIT), l1.lrgNL = log(l1.rgNL), l1.lryIT = log(l1.ryIT), l1.lryNLc = log(l1.ryNL)) %>%
  mutate(shockIT2 = (shockIT / l1.ryNL) / sd((shockIT / l1.ryNL), na.rm = TRUE), 
         shockNL2 = (shockNL / l1.ryIT) / sd((shockNL / l1.ryIT), na.rm = TRUE)) %>%
  mutate(shockES2 = (shockES / l1.ryES) / sd((shockES / l1.ryES), na.rm = TRUE), 
         shockFR2 = (shockFR / l1.ryFR) / sd((shockFR / l1.ryFR), na.rm = TRUE), 
         shockDE2 = (shockDE / l1.ryDE) / sd((shockDE / l1.ryDE), na.rm = TRUE)) %>%
  mutate(shockIT3 = shockIT2 / 100, shockNL3 = shockNL2 / 100, shockES3 = shockES2 / 100, 
         shockFR3 = shockFR2 / 100, shockDE3 = shockDE2 / 100) %>%
  mutate(shockIT4 = shockIT2 / 10000, shockNL4 = shockNL2 / 10000, shockES4 = shockES2 / 10000, 
         shockFR4 = shockFR2 / 10000, shockDE4 = shockDE2 / 10000) %>%
  mutate(ryIT.l1 = lead(ryIT), ryIT.l2 = lead(ryIT, n = 2), ryIT.l3 = lead(ryIT, n = 3), 
         ryIT.l4 = lead(ryIT, n = 4), ryIT.l5 = lead(ryIT, n = 5), ryIT.l6 = lead(ryIT, n = 6),
         ryIT.l7 = lead(ryIT, n = 7), ryIT.l8 = lead(ryIT, n = 8), ryIT.l9 = lead(ryIT, n = 9),
         ryIT.l10 = lead(ryIT, n = 10), ryIT.l11 = lead(ryIT, n = 11), ryIT.l12 = lead(ryIT, n = 12)) %>%
  mutate(ryNL.l1 = lead(ryNL), ryNL.l2 = lead(ryNL, n = 2), ryNL.l3 = lead(ryNL, n = 3), 
         ryNL.l4 = lead(ryNL, n = 4), ryNL.l5 = lead(ryNL, n = 5), ryNL.l6 = lead(ryNL, n = 6),
         ryNL.l7 = lead(ryNL, n = 7), ryNL.l8 = lead(ryNL, n = 8), ryNL.l9 = lead(ryNL, n = 9),
         ryNL.l10 = lead(ryNL, n = 10), ryNL.l11 = lead(ryNL, n = 11), ryNL.l12 = lead(ryNL, n = 12)) %>%
  mutate(rgIT.l1 = lead(rgIT), rgIT.l2 = lead(rgIT, n = 2), rgIT.l3 = lead(rgIT, n = 3), 
         rgIT.l4 = lead(rgIT, n = 4), rgIT.l5 = lead(rgIT, n = 5), rgIT.l6 = lead(rgIT, n = 6),
         rgIT.l7 = lead(rgIT, n = 7), rgIT.l8 = lead(rgIT, n = 8), rgIT.l9 = lead(rgIT, n = 9),
         rgIT.l10 = lead(rgIT, n = 10), rgIT.l11 = lead(rgIT, n = 11), rgIT.l12 = lead(rgIT, n = 12)) %>%
  mutate(rgNL.l1 = lead(rgNL), rgNL.l2 = lead(rgNL, n = 2), rgNL.l3 = lead(rgNL, n = 3), 
         rgNL.l4 = lead(rgNL, n = 4), rgNL.l5 = lead(rgNL, n = 5), rgNL.l6 = lead(rgNL, n = 6),
         rgNL.l7 = lead(rgNL, n = 7), rgNL.l8 = lead(rgNL, n = 8), rgNL.l9 = lead(rgNL, n = 9),
         rgNL.l10 = lead(rgNL, n = 10), rgNL.l11 = lead(rgNL, n = 11), rgNL.l12 = lead(rgNL, n = 12)) %>%
  mutate(l1.debtIT = lag(debtIT, n = 1), l1.intIT = lag(intIT, n = 1), l1.lrtrIT = lag(lrtrIT, n = 1),
         l1.lrgIT = lag(lrgIT, n = 1), l1.lryITc = lag(lryITc, n = 1), l2.debtIT = lag(debtIT, n = 2),
         l2.intIT = lag(intIT, n = 2), l2.lrtrIT = lag(lrtrIT, n = 2), l2.lrgIT = lag(lrgIT, n = 2),
         l2.lryITc = lag(lryITc, n = 2), l3.debtIT = lag(debtIT, n = 3), l3.intIT = lag(intIT, n = 3),
         l3.lrtrIT = lag(lrtrIT, n = 3), l3.lrgIT = lag(lrgIT, n = 3), l3.lryITc = lag(lryITc, n = 3),
         l4.debtIT = lag(debtIT, n = 4), l4.intIT = lag(intIT, n = 4), l4.lrtrIT = lag(lrtrIT, n = 4),
         l4.lrgIT = lag(lrgIT, n = 4), l4.lryITc = lag(lryITc, n = 4)) %>%
  mutate(l1.debtNL = lag(debtNL, n = 1), l1.intNL = lag(intNL, n = 1), l1.lrtrNL = lag(lrtrNL, n = 1),
         l1.lrgNL = lag(lrgNL, n = 1), l1.lryNLc = lag(lryNLc, n = 1), l2.debtNL = lag(debtNL, n = 2),
         l2.intNL = lag(intNL, n = 2), l2.lrtrNL = lag(lrtrNL, n = 2), l2.lrgNL = lag(lrgNL, n = 2),
         l2.lryNLc = lag(lryNLc, n = 2), l3.debtNL = lag(debtNL, n = 3), l3.intNL = lag(intNL, n = 3),
         l3.lrtrNL = lag(lrtrNL, n = 3), l3.lrgNL = lag(lrgNL, n = 3), l3.lryNLc = lag(lryNLc, n = 3),
         l4.debtNL = lag(debtNL, n = 4), l4.intNL = lag(intNL, n = 4), l4.lrtrNL = lag(lrtrNL, n = 4),
         l4.lrgNL = lag(lrgNL, n = 4), l4.lryNLc = lag(lryNLc, n = 4))

# -- OLS regressions

# -- Equation 5
lhsNLIT50 <- (data2$ryIT - data2$l1.ryIT) / data2$l1.ryIT
lhsNLIT5 <- lapply(1:h, function(x) (data2[, 66+x] - data2$l1.ryIT) / data2$l1.ryIT)
lhsNLIT5 <- data.frame(lhsNLIT5)
names(lhsNLIT5) = paste("lhsNLIT5", 1:h, sep = "")
data3 <- cbind(data2, lhsNLIT50, lhsNLIT5)
NLIT5 <- lapply(1:13, function(x) lm(data3[, 151+x] ~ shockNL2 + l1.debtIT + l1.intIT + l1.lrtrIT + l1.lrgIT + l1.lryITc + l2.debtIT + l2.intIT + l2.lrtrIT + l2.lrgIT + l2.lryITc + l3.debtIT + l3.intIT + l3.lrtrIT + l3.lrgIT + l3.lryITc + l4.debtIT + l4.intIT + l4.lrtrIT + l4.lrgIT + l4.lryITc + shockDE2 + shockFR2 + shockES2 + shockIT2, data = data3))
summariesNLIT5 <- lapply(NLIT5, summary)
NLIT5conf95 <- lapply(NLIT5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
NLIT5conf68 <- lapply(NLIT5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
NLIT5up95 <- lapply(1:13, function(x) NLIT5conf95[[x]][2,2])
NLIT5low95 <- lapply(1:13, function(x) NLIT5conf95[[x]][2,1])
NLIT5up68 <- lapply(1:13, function(x) NLIT5conf68[[x]][2,2])
NLIT5low68 <- lapply(1:13, function(x) NLIT5conf68[[x]][2,1])
betaNLIT <- lapply(summariesNLIT5, function(x) x$coefficients[2,1])
names(betaNLIT) <- paste("betaNLIT", 0:h, sep = "")

# -- Equation 6
lhsNLIT60 <- (data3$rgNL - data3$l1.rgNL) / data2$l1.ryIT
lhsNLIT6 <- lapply(1:h, function(x) (data3[, 102+x] - data3$l1.rgNL) / data3$l1.ryIT)
lhsNLIT6 <- data.frame(lhsNLIT6)
names(lhsNLIT6) = paste("lhsNLIT6", 1:h, sep = "")
data3 <- cbind(data3, lhsNLIT60, lhsNLIT6)
NLIT6 <- lapply(1:13, function(x) lm(data3[, 164+x] ~ shockNL3 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc + shockDE3 + shockFR3 + shockES3 + shockIT3, data = data3))
summariesNLIT6 <- lapply(NLIT6, summary)
NLIT6conf95 <- lapply(NLIT6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
NLIT6conf68 <- lapply(NLIT6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
NLIT6up95 <- lapply(1:13, function(x) NLIT6conf95[[x]][2,2])
NLIT6low95 <- lapply(1:13, function(x) NLIT6conf95[[x]][2,1])
NLIT6up68 <- lapply(1:13, function(x) NLIT6conf68[[x]][2,2])
NLIT6low68 <- lapply(1:13, function(x) NLIT6conf68[[x]][2,1])
gammaNLIT <- lapply(summariesNLIT6, function(x) x$coefficients[2,1])
names(gammaNLIT) <- paste("gammaNLIT", 0:h, sep = "")

# -- Cumulative multiplier
mNLITc <- lapply(1:13, function(x) cumsum(as.numeric(betaNLIT))[x] / cumsum(as.numeric(gammaNLIT))[x])
mNLITc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaNLIT) / as.numeric(gammaNLIT))[x])

# -- Generate IRF graph of GDP response
v1 <- data.frame(cbind(betaNLIT = unlist(betaNLIT), NLIT5up95 = unlist(NLIT5up95), NLIT5low95 = unlist(NLIT5low95), 
                       NLIT5up68 = unlist(NLIT5up68), NLIT5low68 = unlist(NLIT5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfNLIT <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfNLIT <- irfNLIT + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfNLIT1 <- irfNLIT + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.006, 0.006, 0.002)) +
  ggtitle("IT - GDP change (GOV shock in NL)")
irfNLIT2 <- irfNLIT1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfNLIT2

# -- Generate IRF graph of GOV response
v1 <- data.frame(cbind(gammaNLIT = unlist(gammaNLIT), NLIT6up95 = unlist(NLIT6up95), NLIT6low95 = unlist(NLIT6low95), 
                       NLIT6up68 = unlist(NLIT6up68), NLIT6low68 = unlist(NLIT6low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfNLIT3 <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfNLIT3 <- irfNLIT3 + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfNLIT4 <- irfNLIT3 + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.05, 0.2, 0.05)) +
  ggtitle("NL - GOV change (GOV shock in NL)")
irfNLIT5 <- irfNLIT4 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfNLIT5


# --- Effect of Italian govt spending shock on Netherlands

# -- Equation 5
lhsITNL50 <- (data3$ryNL - data3$l1.ryNL) / data3$l1.ryNL
lhsITNL5 <- lapply(1:h, function(x) (data3[, 78+x] - data3$l1.ryNL) / data3$l1.ryNL)
lhsITNL5 <- data.frame(lhsITNL5)
names(lhsITNL5) = paste("lhsITNL5", 1:h, sep = "")
data3 <- cbind(data3, lhsITNL50, lhsITNL5)
ITNL5 <- lapply(1:13, function(x) lm(data3[, 177+x] ~ shockIT3 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc + shockDE3 + shockFR3 + shockES3 + shockNL3, data = data3))
summariesITNL5 <- lapply(ITNL5, summary)
ITNL5conf95 <- lapply(ITNL5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ITNL5conf68 <- lapply(ITNL5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ITNL5up95 <- lapply(1:13, function(x) ITNL5conf95[[x]][2,2])
ITNL5low95 <- lapply(1:13, function(x) ITNL5conf95[[x]][2,1])
ITNL5up68 <- lapply(1:13, function(x) ITNL5conf68[[x]][2,2])
ITNL5low68 <- lapply(1:13, function(x) ITNL5conf68[[x]][2,1])
betaITNL <- lapply(summariesITNL5, function(x) x$coefficients[2,1])
names(betaITNL) <- paste("betaITNL", 0:h, sep = "")

# -- Equation 6
lhsITNL60 <- (data3$rgIT - data3$l1.rgIT) / data3$l1.rgIT
lhsITNL6 <- lapply(1:h, function(x) (data3[, 90+x] - data3$l1.rgIT) / data3$l1.ryNL)
lhsITNL6 <- data.frame(lhsITNL6)
names(lhsITNL6) = paste("lhsITNL6", 1:h, sep = "")
data3 <- cbind(data3, lhsITNL60, lhsITNL6)
ITNL6 <- lapply(1:13, function(x) lm(data3[, 190+x] ~ shockIT3 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc + shockDE3 + shockFR3 + shockES3 + shockNL3, data = data3))
summariesITNL6 <- lapply(ITNL6, summary)
ITNL6conf95 <- lapply(ITNL6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ITNL6conf68 <- lapply(ITNL6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ITNL6up95 <- lapply(1:13, function(x) ITNL6conf95[[x]][2,2])
ITNL6low95 <- lapply(1:13, function(x) ITNL6conf95[[x]][2,1])
ITNL6up68 <- lapply(1:13, function(x) ITNL6conf68[[x]][2,2])
ITNL6low68 <- lapply(1:13, function(x) ITNL6conf68[[x]][2,1])
gammaITNL <- lapply(summariesITNL6, function(x) x$coefficients[2,1])
names(gammaITNL) <- paste("gammaITNL", 0:h, sep = "")

# -- Cumulative multiplier
mITNLc <- lapply(1:13, function(x) cumsum(as.numeric(betaITNL))[x] / cumsum(as.numeric(gammaITNL))[x])
mITNLc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaITNL) / as.numeric(gammaITNL))[x])

# -- Generate IRF graph of GDP response
v1 <- data.frame(cbind(betaITNL = unlist(betaITNL), ITNL5up95 = unlist(ITNL5up95), ITNL5low95 = unlist(ITNL5low95), 
                       ITNL5up68 = unlist(ITNL5up68), ITNL5low68 = unlist(ITNL5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfITNL <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfITNL <- irfITNL + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfITNL1 <- irfITNL + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.25)) +
  ggtitle("NL - GDP change (GOV shock in IT)")
irfITNL2 <- irfITNL1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfITNL2

# -- Generate IRF graph of GOV response
v1 <- data.frame(cbind(gammaITNL = unlist(gammaITNL), ITNL6up95 = unlist(ITNL6up95), ITNL6low95 = unlist(ITNL6low95), 
                       ITNL6up68 = unlist(ITNL6up68), ITNL6low68 = unlist(ITNL6low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfITNL3 <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfITNL3 <- irfITNL3 + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfITNL4 <- irfITNL3 + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.8, 0.8, 0.2)) +
  ggtitle("IT - GOV change (GOV shock in IT)")
irfITNL5 <- irfITNL4 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfITNL5
