# --- Effect of Dutch govt spending shock on Spain

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
oES <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/ABP_other_variables.xlsx", 
                  sheet = "ES", range = "B1:F157")
colnames(oES) <- c("rxES", "rmES", "R", "D", "pop")


# Load packages
library(ggplot2)
library(gridExtra)
library(dplyr)
library(lmtest)
library(sandwich)
h <- 12

# Create dataframe
shockFR <- FR$shockFR
shockIT <- IT$shockIT
shockDE <- DE$shockDEq
data <- cbind(ES, NL, oNL, oES, shockIT, shockFR, shockDE)
data1 <- subset(data, select = -c(2:14, 28:41, 55:56))
names(data1)[2] <- "debtES"
names(data1)[14] <- "shockES"
names(data1)[15] <- "debtNL"
names(data1)[27] <- "shockNL"

# -- Re-scaling of left-hand side of Equations (5) and (6):
data2 <- data1 %>%
  mutate(l1.rgES = lag(rgES), l1.rgNL = lag(rgNL), l1.ryES = lag(ryES), l1.ryNL = lag(ryNL)) %>%
  mutate(l1.ryIT = lag(IT$ryIT), l1.ryFR = lag(FR$ryFR), l1.ryDE = lag(DE$ryDE)) %>%
  mutate(l1.lrgES = log(l1.rgES), l1.lrgNL = log(l1.rgNL), l1.lryES = log(l1.ryES), l1.lryNLc = log(l1.ryNL)) %>%
  mutate(shockNL2 = (shockNL / l1.ryES) / sd((shockNL / l1.ryES), na.rm = TRUE), 
         shockFR2 = (shockFR / l1.ryFR) / sd((shockFR / l1.ryFR), na.rm = TRUE),
         shockES2 = (shockES / l1.ryNL) / sd((shockES / l1.ryNL), na.rm = TRUE), 
         shockDE2 = (shockDE / l1.ryDE) / sd((shockDE / l1.ryDE), na.rm = TRUE),
         shockIT2 = (shockIT / l1.ryIT) / sd((shockIT / l1.ryIT), na.rm = TRUE)) %>% 
  mutate(shockIT3 = shockIT2 / 100, shockES3 = shockES2 / 100, shockFR3 = shockFR2 / 100, 
         shockDE3 = shockDE2 / 100, shockNL3 = shockNL2 / 100) %>%
  mutate(shockIT4 = shockIT2 / 10000, shockES4 = shockES2 / 10000, shockFR4 = shockFR2 / 10000, 
         shockDE4 = shockDE2 / 10000, shockNL4 = shockNL2 / 10000) %>%
  mutate(ryES.l1 = lead(ryES), ryES.l2 = lead(ryES, n = 2), ryES.l3 = lead(ryES, n = 3), 
         ryES.l4 = lead(ryES, n = 4), ryES.l5 = lead(ryES, n = 5), ryES.l6 = lead(ryES, n = 6),
         ryES.l7 = lead(ryES, n = 7), ryES.l8 = lead(ryES, n = 8), ryES.l9 = lead(ryES, n = 9),
         ryES.l10 = lead(ryES, n = 10), ryES.l11 = lead(ryES, n = 11), ryES.l12 = lead(ryES, n = 12)) %>%
  mutate(ryNL.l1 = lead(ryNL), ryNL.l2 = lead(ryNL, n = 2), ryNL.l3 = lead(ryNL, n = 3), 
         ryNL.l4 = lead(ryNL, n = 4), ryNL.l5 = lead(ryNL, n = 5), ryNL.l6 = lead(ryNL, n = 6),
         ryNL.l7 = lead(ryNL, n = 7), ryNL.l8 = lead(ryNL, n = 8), ryNL.l9 = lead(ryNL, n = 9),
         ryNL.l10 = lead(ryNL, n = 10), ryNL.l11 = lead(ryNL, n = 11), ryNL.l12 = lead(ryNL, n = 12)) %>%
  mutate(rgES.l1 = lead(rgES), rgES.l2 = lead(rgES, n = 2), rgES.l3 = lead(rgES, n = 3), 
         rgES.l4 = lead(rgES, n = 4), rgES.l5 = lead(rgES, n = 5), rgES.l6 = lead(rgES, n = 6),
         rgES.l7 = lead(rgES, n = 7), rgES.l8 = lead(rgES, n = 8), rgES.l9 = lead(rgES, n = 9),
         rgES.l10 = lead(rgES, n = 10), rgES.l11 = lead(rgES, n = 11), rgES.l12 = lead(rgES, n = 12)) %>%
  mutate(rgNL.l1 = lead(rgNL), rgNL.l2 = lead(rgNL, n = 2), rgNL.l3 = lead(rgNL, n = 3), 
         rgNL.l4 = lead(rgNL, n = 4), rgNL.l5 = lead(rgNL, n = 5), rgNL.l6 = lead(rgNL, n = 6),
         rgNL.l7 = lead(rgNL, n = 7), rgNL.l8 = lead(rgNL, n = 8), rgNL.l9 = lead(rgNL, n = 9),
         rgNL.l10 = lead(rgNL, n = 10), rgNL.l11 = lead(rgNL, n = 11), rgNL.l12 = lead(rgNL, n = 12)) %>%
  mutate(l1.debtES = lag(debtES, n = 1), l1.intES = lag(intES, n = 1), l1.lrtrES = lag(lrtrES, n = 1),
         l1.lrgES = lag(lrgES, n = 1), l1.lryESc = lag(lryESc, n = 1), l2.debtES = lag(debtES, n = 2),
         l2.intES = lag(intES, n = 2), l2.lrtrES = lag(lrtrES, n = 2), l2.lrgES = lag(lrgES, n = 2),
         l2.lryESc = lag(lryESc, n = 2), l3.debtES = lag(debtES, n = 3), l3.intES = lag(intES, n = 3),
         l3.lrtrES = lag(lrtrES, n = 3), l3.lrgES = lag(lrgES, n = 3), l3.lryESc = lag(lryESc, n = 3),
         l4.debtES = lag(debtES, n = 4), l4.intES = lag(intES, n = 4), l4.lrtrES = lag(lrtrES, n = 4),
         l4.lrgES = lag(lrgES, n = 4), l4.lryESc = lag(lryESc, n = 4)) %>%
  mutate(l1.debtNL = lag(debtNL, n = 1), l1.intNL = lag(intNL, n = 1), l1.lrtrNL = lag(lrtrNL, n = 1),
         l1.lrgNL = lag(lrgNL, n = 1), l1.lryNLc = lag(lryNLc, n = 1), l2.debtNL = lag(debtNL, n = 2),
         l2.intNL = lag(intNL, n = 2), l2.lrtrNL = lag(lrtrNL, n = 2), l2.lrgNL = lag(lrgNL, n = 2),
         l2.lryNLc = lag(lryNLc, n = 2), l3.debtNL = lag(debtNL, n = 3), l3.intNL = lag(intNL, n = 3),
         l3.lrtrNL = lag(lrtrNL, n = 3), l3.lrgNL = lag(lrgNL, n = 3), l3.lryNLc = lag(lryNLc, n = 3),
         l4.debtNL = lag(debtNL, n = 4), l4.intNL = lag(intNL, n = 4), l4.lrtrNL = lag(lrtrNL, n = 4),
         l4.lrgNL = lag(lrgNL, n = 4), l4.lryNLc = lag(lryNLc, n = 4))

# -- OLS regressions

# -- Equation 5
lhsNLES50 <- (data2$ryES - data2$l1.ryES) / data2$l1.ryES
lhsNLES5 <- lapply(1:h, function(x) (data2[, 66+x] - data2$l1.ryES) / data2$l1.ryES)
lhsNLES5 <- data.frame(lhsNLES5)
names(lhsNLES5) = paste("lhsNLES5", 1:h, sep = "")
data3 <- cbind(data2, lhsNLES50, lhsNLES5)
NLES5 <- lapply(1:13, function(x) lm(data3[, 151+x] ~ shockNL3 + l1.debtES + l1.intES + l1.lrtrES + l1.lrgES + l1.lryESc + l2.debtES + l2.intES + l2.lrtrES + l2.lrgES + l2.lryESc + l3.debtES + l3.intES + l3.lrtrES + l3.lrgES + l3.lryESc + l4.debtES + l4.intES + l4.lrtrES + l4.lrgES + l4.lryESc + shockDE3 + shockFR3 + shockES3 + shockIT3, data = data3))
summariesNLES5 <- lapply(NLES5, summary)
NLES5conf95 <- lapply(NLES5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
NLES5conf68 <- lapply(NLES5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
NLES5up95 <- lapply(1:13, function(x) NLES5conf95[[x]][2,2])
NLES5low95 <- lapply(1:13, function(x) NLES5conf95[[x]][2,1])
NLES5up68 <- lapply(1:13, function(x) NLES5conf68[[x]][2,2])
NLES5low68 <- lapply(1:13, function(x) NLES5conf68[[x]][2,1])
betaNLES <- lapply(summariesNLES5, function(x) x$coefficients[2,1])
names(betaNLES) <- paste("betaNLES", 0:h, sep = "")

# -- Equation 6
lhsNLES60 <- (data3$rgNL - data3$l1.rgNL) / data3$l1.ryES
lhsNLES6 <- lapply(1:h, function(x) (data3[, 102+x] - data3$l1.rgNL) / data3$l1.ryES)
lhsNLES6 <- data.frame(lhsNLES6)
names(lhsNLES6) = paste("lhsNLES6", 1:h, sep = "")
data3 <- cbind(data3, lhsNLES60, lhsNLES6)
NLES6 <- lapply(1:13, function(x) lm(data3[, 164+x] ~ shockNL3 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc + shockDE3 + shockFR3 + shockES3 + shockIT3, data = data3))
summariesNLES6 <- lapply(NLES6, summary)
NLES6conf95 <- lapply(NLES6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
NLES6conf68 <- lapply(NLES6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
NLES6up95 <- lapply(1:13, function(x) NLES6conf95[[x]][2,2])
NLES6low95 <- lapply(1:13, function(x) NLES6conf95[[x]][2,1])
NLES6up68 <- lapply(1:13, function(x) NLES6conf68[[x]][2,2])
NLES6low68 <- lapply(1:13, function(x) NLES6conf68[[x]][2,1])
gammaNLES <- lapply(summariesNLES6, function(x) x$coefficients[2,1])
names(gammaNLES) <- paste("gammaNLES", 0:h, sep = "")

# -- Cumulative multiplier
mNLESc <- lapply(1:13, function(x) cumsum(as.numeric(betaNLES))[x] / cumsum(as.numeric(gammaNLES))[x])
mNLESc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaNLES) / as.numeric(gammaNLES))[x])

# -- Generate IRF graph (as in Fig. 1 of Alloza et al.)
v1 <- data.frame(cbind(betaNLES = unlist(betaNLES), NLES5up95 = unlist(NLES5up95), NLES5low95 = unlist(NLES5low95), 
                       NLES5up68 = unlist(NLES5up68), NLES5low68 = unlist(NLES5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfNLES <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfNLES <- irfNLES + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfNLES1 <- irfNLES + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.4, 0.4, 0.2)) +
  ggtitle("ES - GDP change (GOV shock in NL)")
irfNLES2 <- irfNLES1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfNLES2

# -- Generate IRF graph of GOV response
v1 <- data.frame(cbind(gammaNLES = unlist(gammaNLES), NLES6up95 = unlist(NLES6up95), NLES6low95 = unlist(NLES6low95), 
                       NLES6up68 = unlist(NLES6up68), NLES6low68 = unlist(NLES6low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfNLES3 <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfNLES3 <- irfNLES3 + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfNLES4 <- irfNLES3 + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.1, 0.3, 0.1)) +
  ggtitle("NL - GOV change (GOV shock in NL)")
irfNLES5 <- irfNLES4 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfNLES5


# --- Effect of Spanish govt spending shock on Netherlands

# -- Equation 5
lhsESNL50 <- (data3$ryNL - data3$l1.ryNL) / data3$l1.ryNL
lhsESNL5 <- lapply(1:h, function(x) (data3[, 78+x] - data3$l1.ryNL) / data3$l1.ryNL)
lhsESNL5 <- data.frame(lhsESNL5)
names(lhsESNL5) = paste("lhsESNL5", 1:h, sep = "")
data3 <- cbind(data3, lhsESNL50, lhsESNL5)
ESNL5 <- lapply(1:13, function(x) lm(data3[, 177+x] ~ shockES3 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc + shockDE3 + shockFR3 + shockIT3 + shockNL3, data = data3))
summariesESNL5 <- lapply(ESNL5, summary)
ESNL5conf95 <- lapply(ESNL5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ESNL5conf68 <- lapply(ESNL5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ESNL5up95 <- lapply(1:13, function(x) ESNL5conf95[[x]][2,2])
ESNL5low95 <- lapply(1:13, function(x) ESNL5conf95[[x]][2,1])
ESNL5up68 <- lapply(1:13, function(x) ESNL5conf68[[x]][2,2])
ESNL5low68 <- lapply(1:13, function(x) ESNL5conf68[[x]][2,1])
betaESNL <- lapply(summariesESNL5, function(x) x$coefficients[2,1])
names(betaESNL) <- paste("betaESNL", 0:h, sep = "")

# -- Equation 6
lhsESNL60 <- (data3$rgES - data3$l1.rgES) / data3$l1.ryNL
lhsESNL6 <- lapply(1:h, function(x) (data3[, 90+x] - data3$l1.rgES) / data3$l1.ryNL)
lhsESNL6 <- data.frame(lhsESNL6)
names(lhsESNL6) = paste("lhsESNL6", 1:h, sep = "")
data3 <- cbind(data3, lhsESNL60, lhsESNL6)
ESNL6 <- lapply(1:13, function(x) lm(data3[, 190+x] ~ shockES3 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc + shockDE3 + shockFR3 + shockIT3 + shockNL3, data = data3))
summariesESNL6 <- lapply(ESNL6, summary)
ESNL6conf95 <- lapply(ESNL6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ESNL6conf68 <- lapply(ESNL6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ESNL6up95 <- lapply(1:13, function(x) ESNL6conf95[[x]][2,2])
ESNL6low95 <- lapply(1:13, function(x) ESNL6conf95[[x]][2,1])
ESNL6up68 <- lapply(1:13, function(x) ESNL6conf68[[x]][2,2])
ESNL6low68 <- lapply(1:13, function(x) ESNL6conf68[[x]][2,1])
gammaESNL <- lapply(summariesESNL6, function(x) x$coefficients[2,1])
names(gammaESNL) <- paste("gammaESNL", 0:h, sep = "")

# -- Cumulative multiplier
mESNLc <- lapply(1:13, function(x) cumsum(as.numeric(betaESNL))[x] / cumsum(as.numeric(gammaESNL))[x])
mESNLc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaESNL) / as.numeric(gammaESNL))[x])

# -- Generate IRF graph (as in Fig. 1 of Alloza et al.)
v1 <- data.frame(cbind(betaESNL = unlist(betaESNL), ESNL5up95 = unlist(ESNL5up95), ESNL5low95 = unlist(ESNL5low95), 
                       ESNL5up68 = unlist(ESNL5up68), ESNL5low68 = unlist(ESNL5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfESNL <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfESNL <- irfESNL + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfESNL1 <- irfESNL + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.2, 0.6, 0.2)) +
  ggtitle("NL - GDP change (GOV shock in ES)")
irfESNL2 <- irfESNL1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfESNL2

# -- Generate IRF graph of GOV response
v1 <- data.frame(cbind(gammaESNL = unlist(gammaESNL), ESNL6up95 = unlist(ESNL6up95), ESNL6low95 = unlist(ESNL6low95), 
                       ESNL6up68 = unlist(ESNL6up68), ESNL6low68 = unlist(ESNL6low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfESNL3 <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfESNL3 <- irfESNL3 + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfESNL4 <- irfESNL3 + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.25, 1.25, 0.25)) +
  ggtitle("ES - GOV change (GOV shock in ES)")
irfESNL5 <- irfESNL4 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfESNL5
