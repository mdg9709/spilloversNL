# --- Effect of French govt spending shock on Italy

# Data period: 1999q1-2018q4
# 95% and 68% confidence intervals
# h = 4, 8 and 12

# OLS with left-hand side in growth rates and 4 lags of x(t-1)

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
oIT <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/ABP_other_variables.xlsx", 
                  sheet = "IT", range = "B1:F157")
colnames(oIT) <- c("rxIT", "rmIT", "R", "D", "pop")
oFR <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/ABP_other_variables.xlsx", 
                  sheet = "FR", range = "B1:F157")
colnames(oFR) <- c("rxFR", "rmFR", "R", "D", "pop")

# Load packages
library(ggplot2)
library(gridExtra)
library(dplyr)
library(lmtest)
library(sandwich)
h <- 12

# Create dataframe
shockDE <- DE$shockDEq
shockNL <- NL$shockNL
shockES <- ES$shockES
data <- cbind(IT, FR, oIT, oFR, shockNL, shockDE, shockES)
data1 <- subset(data, select = -c(2:14, 28:41))
names(data1)[2] <- "debtIT"
names(data1)[14] <- "shockIT"
names(data1)[15] <- "debtFR"
names(data1)[27] <- "shockFR"

# -- Re-scaling of left-hand side of Equations (5) and (6):
data2 <- data1 %>%
  mutate(l1.rgIT = lag(rgIT), l1.rgFR = lag(rgFR), l1.ryIT = lag(ryIT), l1.ryFR = lag(ryFR)) %>%
  mutate(l1.ryNL = lag(NL$ryNL), l1.ryES = lag(ES$ryES), l1.ryDE = lag(DE$ryDE)) %>%
  mutate(l1.lrgIT = log(l1.rgIT), l1.lrgFR = log(l1.rgFR), l1.lryIT = log(l1.ryIT), l1.lryFR = log(l1.ryFR)) %>%
  mutate(shockIT2 = (shockIT / l1.ryFR) / sd((shockIT / l1.ryFR), na.rm = TRUE), 
         shockFR2 = (shockFR / l1.ryIT) / sd((shockFR / l1.ryIT), na.rm = TRUE),
         shockNL2 = (shockNL / l1.ryNL) / sd((shockNL / l1.ryNL), na.rm = TRUE), 
         shockDE2 = (shockDE / l1.ryDE) / sd((shockDE / l1.ryDE), na.rm = TRUE),
         shockES2 = (shockES / l1.ryES) / sd((shockES / l1.ryES), na.rm = TRUE)) %>% 
  mutate(shockIT3 = shockIT2 / 100, shockES3 = shockES2 / 100, shockFR3 = shockFR2 / 100, 
         shockDE3 = shockDE2 / 100, shockNL3 = shockNL2 / 100) %>%
  mutate(shockIT4 = shockIT2 / 10000, shockES4 = shockES2 / 10000, shockFR4 = shockFR2 / 10000, 
         shockDE4 = shockDE2 / 10000, shockNL4 = shockNL2 / 10000) %>%
  mutate(ryIT.l1 = lead(ryIT), ryIT.l2 = lead(ryIT, n = 2), ryIT.l3 = lead(ryIT, n = 3), 
         ryIT.l4 = lead(ryIT, n = 4), ryIT.l5 = lead(ryIT, n = 5), ryIT.l6 = lead(ryIT, n = 6),
         ryIT.l7 = lead(ryIT, n = 7), ryIT.l8 = lead(ryIT, n = 8), ryIT.l9 = lead(ryIT, n = 9),
         ryIT.l10 = lead(ryIT, n = 10), ryIT.l11 = lead(ryIT, n = 11), ryIT.l12 = lead(ryIT, n = 12)) %>%
  mutate(ryFR.l1 = lead(ryFR), ryFR.l2 = lead(ryFR, n = 2), ryFR.l3 = lead(ryFR, n = 3), 
         ryFR.l4 = lead(ryFR, n = 4), ryFR.l5 = lead(ryFR, n = 5), ryFR.l6 = lead(ryFR, n = 6),
         ryFR.l7 = lead(ryFR, n = 7), ryFR.l8 = lead(ryFR, n = 8), ryFR.l9 = lead(ryFR, n = 9),
         ryFR.l10 = lead(ryFR, n = 10), ryFR.l11 = lead(ryFR, n = 11), ryFR.l12 = lead(ryFR, n = 12)) %>%
  mutate(rgIT.l1 = lead(rgIT), rgIT.l2 = lead(rgIT, n = 2), rgIT.l3 = lead(rgIT, n = 3), 
         rgIT.l4 = lead(rgIT, n = 4), rgIT.l5 = lead(rgIT, n = 5), rgIT.l6 = lead(rgIT, n = 6),
         rgIT.l7 = lead(rgIT, n = 7), rgIT.l8 = lead(rgIT, n = 8), rgIT.l9 = lead(rgIT, n = 9),
         rgIT.l10 = lead(rgIT, n = 10), rgIT.l11 = lead(rgIT, n = 11), rgIT.l12 = lead(rgIT, n = 12)) %>%
  mutate(rgFR.l1 = lead(rgFR), rgFR.l2 = lead(rgFR, n = 2), rgFR.l3 = lead(rgFR, n = 3), 
         rgFR.l4 = lead(rgFR, n = 4), rgFR.l5 = lead(rgFR, n = 5), rgFR.l6 = lead(rgFR, n = 6),
         rgFR.l7 = lead(rgFR, n = 7), rgFR.l8 = lead(rgFR, n = 8), rgFR.l9 = lead(rgFR, n = 9),
         rgFR.l10 = lead(rgFR, n = 10), rgFR.l11 = lead(rgFR, n = 11), rgFR.l12 = lead(rgFR, n = 12)) %>%
  mutate(l1.debtIT = lag(debtIT, n = 1), l1.intIT = lag(intIT, n = 1), l1.lrtrIT = lag(lrtrIT, n = 1),
         l1.lrgIT = lag(lrgIT, n = 1), l1.lryITc = lag(lryITc, n = 1), l2.debtIT = lag(debtIT, n = 2),
         l2.intIT = lag(intIT, n = 2), l2.lrtrIT = lag(lrtrIT, n = 2), l2.lrgIT = lag(lrgIT, n = 2),
         l2.lryITc = lag(lryITc, n = 2), l3.debtIT = lag(debtIT, n = 3), l3.intIT = lag(intIT, n = 3),
         l3.lrtrIT = lag(lrtrIT, n = 3), l3.lrgIT = lag(lrgIT, n = 3), l3.lryITc = lag(lryITc, n = 3),
         l4.debtIT = lag(debtIT, n = 4), l4.intIT = lag(intIT, n = 4), l4.lrtrIT = lag(lrtrIT, n = 4),
         l4.lrgIT = lag(lrgIT, n = 4), l4.lryITc = lag(lryITc, n = 4)) %>%
  mutate(l1.debtFR = lag(debtFR, n = 1), l1.intFR = lag(intFR, n = 1), l1.lrtrFR = lag(lrtrFR, n = 1),
         l1.lrgFR = lag(lrgFR, n = 1), l1.lryFRc = lag(lryFRc, n = 1), l2.debtFR = lag(debtFR, n = 2),
         l2.intFR = lag(intFR, n = 2), l2.lrtrFR = lag(lrtrFR, n = 2), l2.lrgFR = lag(lrgFR, n = 2),
         l2.lryFRc = lag(lryFRc, n = 2), l3.debtFR = lag(debtFR, n = 3), l3.intFR = lag(intFR, n = 3),
         l3.lrtrFR = lag(lrtrFR, n = 3), l3.lrgFR = lag(lrgFR, n = 3), l3.lryFRc = lag(lryFRc, n = 3),
         l4.debtFR = lag(debtFR, n = 4), l4.intFR = lag(intFR, n = 4), l4.lrtrFR = lag(lrtrFR, n = 4),
         l4.lrgFR = lag(lrgFR, n = 4), l4.lryFRc = lag(lryFRc, n = 4))

# --- OLS regressions

# -- Equation 5
lhsITFR50 <- (data2$ryFR - data2$l1.ryFR) / data2$l1.ryFR
lhsITFR5 <- lapply(1:h, function(x) (data2[, 78+x] - data2$l1.ryFR) / data2$l1.ryFR)
lhsITFR5 <- data.frame(lhsITFR5)
names(lhsITFR5) = paste("lhsITFR5", 1:h, sep = "")
data3 <- cbind(data2, lhsITFR50, lhsITFR5)
ITFR5 <- lapply(1:13, function(x) lm(data3[, 152+x] ~ shockIT2 + l1.debtFR + l1.intFR + l1.lrtrFR + l1.lrgFR + l1.lryFRc + l2.debtFR + l2.intFR + l2.lrtrFR + l2.lrgFR + l2.lryFRc + l3.debtFR + l3.intFR + l3.lrtrFR + l3.lrgFR + l3.lryFRc + l4.debtFR + l4.intFR + l4.lrtrFR + l4.lrgFR + l4.lryFRc + shockDE2 + shockFR2 + shockES2 + shockNL2, data = data3))
summariesITFR5 <- lapply(ITFR5, summary)
ITFR5conf95 <- lapply(ITFR5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ITFR5conf68 <- lapply(ITFR5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ITFR5up95 <- lapply(1:13, function(x) ITFR5conf95[[x]][2,2])
ITFR5low95 <- lapply(1:13, function(x) ITFR5conf95[[x]][2,1])
ITFR5up68 <- lapply(1:13, function(x) ITFR5conf68[[x]][2,2])
ITFR5low68 <- lapply(1:13, function(x) ITFR5conf68[[x]][2,1])
betaITFR <- lapply(summariesITFR5, function(x) x$coefficients[2,1])
names(betaITFR) <- paste("betaITFR", 0:h, sep = "")

# -- Equation 6
lhsITFR60 <- (data3$rgIT - data3$l1.rgIT) / data3$l1.ryFR
lhsITFR6 <- lapply(1:h, function(x) (data3[, 90+x] - data3$l1.rgIT) / data3$l1.ryFR)
lhsITFR6 <- data.frame(lhsITFR6)
names(lhsITFR6) = paste("lhsITFR6", 1:h, sep = "")
data3 <- cbind(data3, lhsITFR60, lhsITFR6)
ITFR6 <- lapply(1:13, function(x) lm(data3[, 165+x] ~ shockIT3 + l1.debtIT + l1.intIT + l1.lrtrIT + l1.lrgIT + l1.lryITc + l2.debtIT + l2.intIT + l2.lrtrIT + l2.lrgIT + l2.lryITc + l3.debtIT + l3.intIT + l3.lrtrIT + l3.lrgIT + l3.lryITc + l4.debtIT + l4.intIT + l4.lrtrIT + l4.lrgIT + l4.lryITc + shockDE3 + shockFR3 + shockES3 + shockNL3, data = data3))
summariesITFR6 <- lapply(ITFR6, summary)
ITFR6conf95 <- lapply(ITFR6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ITFR6conf68 <- lapply(ITFR6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ITFR6up95 <- lapply(1:13, function(x) ITFR6conf95[[x]][2,2])
ITFR6low95 <- lapply(1:13, function(x) ITFR6conf95[[x]][2,1])
ITFR6up68 <- lapply(1:13, function(x) ITFR6conf68[[x]][2,2])
ITFR6low68 <- lapply(1:13, function(x) ITFR6conf68[[x]][2,1])
gammaITFR <- lapply(summariesITFR6, function(x) x$coefficients[2,1])
names(gammaITFR) <- paste("gammaITFR", 0:h, sep = "")

# -- Cumulative multiplier
mITFRc <- lapply(1:13, function(x) cumsum(as.numeric(betaITFR))[x] / cumsum(as.numeric(gammaITFR))[x])
mITFRc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaITFR) / as.numeric(gammaITFR))[x])

# -- Generate IRF graph (as in Fig. 1 of Alloza et al.)
v1 <- data.frame(cbind(betaITFR = unlist(betaITFR), ITFR5up95 = unlist(ITFR5up95), ITFR5low95 = unlist(ITFR5low95), 
                       ITFR5up68 = unlist(ITFR5up68), ITFR5low68 = unlist(ITFR5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfITFR <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfITFR <- irfITFR + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfITFR1 <- irfITFR + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-1, 0.5, 0.25)) +
  ggtitle("FR - GDP change (GOV shock in IT)")
irfITFR2 <- irfITFR1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfITFR2

# -- Generate IRF graph of GOV response
v1 <- data.frame(cbind(gammaITFR = unlist(gammaITFR), ITFR6up95 = unlist(ITFR6up95), ITFR6low95 = unlist(ITFR6low95), 
                       ITFR6up68 = unlist(ITFR6up68), ITFR6low68 = unlist(ITFR6low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfITFR3 <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfITFR3 <- irfITFR3 + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfITFR4 <- irfITFR3 + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.2, 0.3, 0.1)) +
  ggtitle("IT - GOV change (GOV shock in IT)")
irfITFR5 <- irfITFR4 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfITFR5


# --- Effect of French gov't spending shock on Italy

# -- Equation 5
lhsFRIT50 <- (data3$ryIT - data3$l1.ryIT) / data3$l1.ryIT
lhsFRIT5 <- lapply(1:h, function(x) (data3[, 66+x] - data3$l1.ryIT) / data3$l1.ryIT)
lhsFRIT5 <- data.frame(lhsFRIT5)
names(lhsFRIT5) = paste("lhsFRIT5", 1:h, sep = "")
data3 <- cbind(data3, lhsFRIT50, lhsFRIT5)
FRIT5 <- lapply(1:13, function(x) lm(data3[, 178+x] ~ shockFR2 + l1.debtIT + l1.intIT + l1.lrtrIT + l1.lrgIT + l1.lryITc + l2.debtIT + l2.intIT + l2.lrtrIT + l2.lrgIT + l2.lryITc + l3.debtIT + l3.intIT + l3.lrtrIT + l3.lrgIT + l3.lryITc + l4.debtIT + l4.intIT + l4.lrtrIT + l4.lrgIT + l4.lryITc + shockIT2 + shockDE2 + shockES2 + shockNL2, data = data3))
summariesFRIT5 <- lapply(FRIT5, summary)
FRIT5conf95 <- lapply(FRIT5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
FRIT5conf68 <- lapply(FRIT5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
FRIT5up95 <- lapply(1:13, function(x) FRIT5conf95[[x]][2,2])
FRIT5low95 <- lapply(1:13, function(x) FRIT5conf95[[x]][2,1])
FRIT5up68 <- lapply(1:13, function(x) FRIT5conf68[[x]][2,2])
FRIT5low68 <- lapply(1:13, function(x) FRIT5conf68[[x]][2,1])
betaFRIT <- lapply(summariesFRIT5, function(x) x$coefficients[2,1])
names(betaFRIT) <- paste("betaFRIT", 0:h, sep = "")

# -- Equation 6
lhsFRIT60 <- (data3$rgFR - data3$l1.rgFR) / data3$l1.ryIT
lhsFRIT6 <- lapply(1:h, function(x) (data3[, 102+x] - data3$l1.rgFR) / data3$l1.ryIT)
lhsFRIT6 <- data.frame(lhsFRIT6)
names(lhsFRIT6) = paste("lhsFRIT6", 1:h, sep = "")
data3 <- cbind(data3, lhsFRIT60, lhsFRIT6)
FRIT6 <- lapply(1:13, function(x) lm(data3[, 191+x] ~ shockFR3 + l1.debtFR + l1.intFR + l1.lrtrFR + l1.lrgFR + l1.lryITc + l2.debtFR + l2.intFR + l2.lrtrFR + l2.lrgFR + l2.lryITc + l3.debtFR + l3.intFR + l3.lrtrFR + l3.lrgFR + l3.lryITc + l4.debtFR + l4.intFR + l4.lrtrFR + l4.lrgFR + l4.lryITc + shockIT3 + shockDE3 + shockES3 + shockNL3, data = data3))
summariesFRIT6 <- lapply(FRIT6, summary)
FRIT6conf95 <- lapply(FRIT6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
FRIT6conf68 <- lapply(FRIT6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
FRIT6up95 <- lapply(1:13, function(x) FRIT6conf95[[x]][2,2])
FRIT6low95 <- lapply(1:13, function(x) FRIT6conf95[[x]][2,1])
FRIT6up68 <- lapply(1:13, function(x) FRIT6conf68[[x]][2,2])
FRIT6low68 <- lapply(1:13, function(x) FRIT6conf68[[x]][2,1])
gammaFRIT <- lapply(summariesFRIT6, function(x) x$coefficients[2,1])
names(gammaFRIT) <- paste("gammaFRIT", 0:h, sep = "")

# -- Cumulative multiplier
mFRITc <- lapply(1:13, function(x) cumsum(as.numeric(betaFRIT))[x] / cumsum(as.numeric(gammaFRIT))[x])
mFRITc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaFRIT) / as.numeric(gammaFRIT))[x])

# -- Generate IRF graph (as in Fig. 1 of Alloza et al.)
v1 <- data.frame(cbind(betaFRIT = unlist(betaFRIT), FRIT5up95 = unlist(FRIT5up95), FRIT5low95 = unlist(FRIT5low95), 
                       FRIT5up68 = unlist(FRIT5up68), FRIT5low68 = unlist(FRIT5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfFRIT <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfFRIT <- irfFRIT + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfFRIT1 <- irfFRIT + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.25)) +
  ggtitle("IT - GDP change (GOV shock in FR)")
irfFRIT2 <- irfFRIT1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfFRIT2

# -- Generate IRF graph of GOV response
v1 <- data.frame(cbind(gammaFRIT = unlist(gammaFRIT), FRIT6up95 = unlist(FRIT6up95), FRIT6low95 = unlist(FRIT6low95), 
                       FRIT6up68 = unlist(FRIT6up68), FRIT6low68 = unlist(FRIT6low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfFRIT3 <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfFRIT3 <- irfFRIT3 + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfFRIT4 <- irfFRIT3 + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.2, 0.2, 0.1)) +
  ggtitle("FR - GOV change (GOV shock in FR)")
irfFRIT5 <- irfFRIT4 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfFRIT5
