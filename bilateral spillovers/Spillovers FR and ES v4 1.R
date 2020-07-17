# --- Effect of French govt spending shock on Spain

# Data period: 1980q1-2018q4
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
oFR <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/ABP_other_variables.xlsx", 
                  sheet = "FR", range = "B1:F157")
colnames(oFR) <- c("rxFR", "rmFR", "R", "D", "pop")
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
shockDE <- DE$shockDEq
shockNL <- NL$shockNL
shockIT <- IT$shockIT
data <- cbind(ES, FR, oES, oFR, shockNL, shockDE, shockIT)
data1 <- subset(data, select = -c(2:14, 28:41))
names(data1)[2] <- "debtES"
names(data1)[14] <- "shockES"
names(data1)[15] <- "debtFR"
names(data1)[27] <- "shockFR"

# -- Re-scaling of left-hand side of Equations (5) and (6):
data2 <- data1 %>%
  mutate(l1.rgES = lag(rgES), l1.rgFR = lag(rgFR), l1.ryES = lag(ryES), l1.ryFR = lag(ryFR)) %>%
  mutate(l1.ryNL = lag(NL$ryNL), l1.ryDE = lag(DE$ryDE), l1.ryIT = lag(IT$ryIT)) %>%
  mutate(l1.lrgES = log(l1.rgES), l1.lrgFR = log(l1.rgFR), l1.lryES = log(l1.ryES), l1.lryFR = log(l1.ryFR)) %>%
  mutate(shockES2 = (shockES / l1.ryFR) / sd((shockES / l1.ryFR), na.rm = TRUE), 
         shockFR2 = (shockFR / l1.ryES) / sd((shockFR / l1.ryES), na.rm = TRUE),
         shockIT2 = (shockIT / l1.ryIT) / sd((shockIT / l1.ryIT), na.rm = TRUE), 
         shockDE2 = (shockDE / l1.ryDE) / sd((shockDE / l1.ryDE), na.rm = TRUE),
         shockNL2 = (shockNL / l1.ryNL) / sd((shockNL / l1.ryNL), na.rm = TRUE)) %>% 
  mutate(shockIT3 = shockIT2 / 100, shockES3 = shockES2 / 100, shockFR3 = shockFR2 / 100, 
         shockDE3 = shockDE2 / 100, shockNL3 = shockNL2 / 100) %>%
  mutate(shockIT4 = shockIT2 / 10000, shockES4 = shockES2 / 10000, shockFR4 = shockFR2 / 10000, 
         shockDE4 = shockDE2 / 10000, shockNL4 = shockNL2 / 10000) %>%
  mutate(ryES.l1 = lead(ryES), ryES.l2 = lead(ryES, n = 2), ryES.l3 = lead(ryES, n = 3), 
         ryES.l4 = lead(ryES, n = 4), ryES.l5 = lead(ryES, n = 5), ryES.l6 = lead(ryES, n = 6),
         ryES.l7 = lead(ryES, n = 7), ryES.l8 = lead(ryES, n = 8), ryES.l9 = lead(ryES, n = 9),
         ryES.l10 = lead(ryES, n = 10), ryES.l11 = lead(ryES, n = 11), ryES.l12 = lead(ryES, n = 12)) %>%
  mutate(ryFR.l1 = lead(ryFR), ryFR.l2 = lead(ryFR, n = 2), ryFR.l3 = lead(ryFR, n = 3), 
         ryFR.l4 = lead(ryFR, n = 4), ryFR.l5 = lead(ryFR, n = 5), ryFR.l6 = lead(ryFR, n = 6),
         ryFR.l7 = lead(ryFR, n = 7), ryFR.l8 = lead(ryFR, n = 8), ryFR.l9 = lead(ryFR, n = 9),
         ryFR.l10 = lead(ryFR, n = 10), ryFR.l11 = lead(ryFR, n = 11), ryFR.l12 = lead(ryFR, n = 12)) %>%
  mutate(rgES.l1 = lead(rgES), rgES.l2 = lead(rgES, n = 2), rgES.l3 = lead(rgES, n = 3), 
         rgES.l4 = lead(rgES, n = 4), rgES.l5 = lead(rgES, n = 5), rgES.l6 = lead(rgES, n = 6),
         rgES.l7 = lead(rgES, n = 7), rgES.l8 = lead(rgES, n = 8), rgES.l9 = lead(rgES, n = 9),
         rgES.l10 = lead(rgES, n = 10), rgES.l11 = lead(rgES, n = 11), rgES.l12 = lead(rgES, n = 12)) %>%
  mutate(rgFR.l1 = lead(rgFR), rgFR.l2 = lead(rgFR, n = 2), rgFR.l3 = lead(rgFR, n = 3), 
         rgFR.l4 = lead(rgFR, n = 4), rgFR.l5 = lead(rgFR, n = 5), rgFR.l6 = lead(rgFR, n = 6),
         rgFR.l7 = lead(rgFR, n = 7), rgFR.l8 = lead(rgFR, n = 8), rgFR.l9 = lead(rgFR, n = 9),
         rgFR.l10 = lead(rgFR, n = 10), rgFR.l11 = lead(rgFR, n = 11), rgFR.l12 = lead(rgFR, n = 12)) %>%
  mutate(l1.debtES = lag(debtES, n = 1), l1.intES = lag(intES, n = 1), l1.lrtrES = lag(lrtrES, n = 1),
         l1.lrgES = lag(lrgES, n = 1), l1.lryESc = lag(lryESc, n = 1), l2.debtES = lag(debtES, n = 2),
         l2.intES = lag(intES, n = 2), l2.lrtrES = lag(lrtrES, n = 2), l2.lrgES = lag(lrgES, n = 2),
         l2.lryESc = lag(lryESc, n = 2), l3.debtES = lag(debtES, n = 3), l3.intES = lag(intES, n = 3),
         l3.lrtrES = lag(lrtrES, n = 3), l3.lrgES = lag(lrgES, n = 3), l3.lryESc = lag(lryESc, n = 3),
         l4.debtES = lag(debtES, n = 4), l4.intES = lag(intES, n = 4), l4.lrtrES = lag(lrtrES, n = 4),
         l4.lrgES = lag(lrgES, n = 4), l4.lryESc = lag(lryESc, n = 4)) %>%
  mutate(l1.debtFR = lag(debtFR, n = 1), l1.intFR = lag(intFR, n = 1), l1.lrtrFR = lag(lrtrFR, n = 1),
         l1.lrgFR = lag(lrgFR, n = 1), l1.lryFRc = lag(lryFRc, n = 1), l2.debtFR = lag(debtFR, n = 2),
         l2.intFR = lag(intFR, n = 2), l2.lrtrFR = lag(lrtrFR, n = 2), l2.lrgFR = lag(lrgFR, n = 2),
         l2.lryFRc = lag(lryFRc, n = 2), l3.debtFR = lag(debtFR, n = 3), l3.intFR = lag(intFR, n = 3),
         l3.lrtrFR = lag(lrtrFR, n = 3), l3.lrgFR = lag(lrgFR, n = 3), l3.lryFRc = lag(lryFRc, n = 3),
         l4.debtFR = lag(debtFR, n = 4), l4.intFR = lag(intFR, n = 4), l4.lrtrFR = lag(lrtrFR, n = 4),
         l4.lrgFR = lag(lrgFR, n = 4), l4.lryFRc = lag(lryFRc, n = 4))

# -- OLS regressions

# -- Equation 5
lhsFRES50 <- (data2$ryES - data2$l1.ryES) / data2$l1.ryES
lhsFRES5 <- lapply(1:h, function(x) (data2[, 66+x] - data2$l1.ryES) / data2$l1.ryES)
lhsFRES5 <- data.frame(lhsFRES5)
names(lhsFRES5) = paste("lhsFRES5", 1:h, sep = "")
data3 <- cbind(data2, lhsFRES50, lhsFRES5)
FRES5 <- lapply(1:13, function(x) lm(data3[, 152+x] ~ shockFR2 + l1.debtES + l1.intES + l1.lrtrES + l1.lrgES + l1.lryESc + l2.debtES + l2.intES + l2.lrtrES + l2.lrgES + l2.lryESc + l3.debtES + l3.intES + l3.lrtrES + l3.lrgES + l3.lryESc + l4.debtES + l4.intES + l4.lrtrES + l4.lrgES + l4.lryESc + shockDE2 + shockNL2 + shockES2 + shockIT2, data = data3))
summariesFRES5 <- lapply(FRES5, summary)
FRES5conf95 <- lapply(FRES5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
FRES5conf68 <- lapply(FRES5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
FRES5up95 <- lapply(1:13, function(x) FRES5conf95[[x]][2,2])
FRES5low95 <- lapply(1:13, function(x) FRES5conf95[[x]][2,1])
FRES5up68 <- lapply(1:13, function(x) FRES5conf68[[x]][2,2])
FRES5low68 <- lapply(1:13, function(x) FRES5conf68[[x]][2,1])
betaFRES <- lapply(summariesFRES5, function(x) x$coefficients[2,1])
names(betaFRES) <- paste("betaFRES", 0:h, sep = "")

# -- Equation 6
lhsFRES60 <- (data3$rgFR - data3$l1.rgFR) / data3$l1.ryES
lhsFRES6 <- lapply(1:h, function(x) (data3[, 102+x] - data3$l1.rgFR) / data3$l1.ryES)
lhsFRES6 <- data.frame(lhsFRES6)
names(lhsFRES6) = paste("lhsFRES6", 1:h, sep = "")
data3 <- cbind(data3, lhsFRES60, lhsFRES6)
FRES6 <- lapply(1:13, function(x) lm(data3[, 165+x] ~ shockFR3 + l1.debtFR + l1.intFR + l1.lrtrFR + l1.lrgFR + l1.lryESc + l2.debtFR + l2.intFR + l2.lrtrFR + l2.lrgFR + l2.lryESc + l3.debtFR + l3.intFR + l3.lrtrFR + l3.lrgFR + l3.lryESc + l4.debtFR + l4.intFR + l4.lrtrFR + l4.lrgFR + l4.lryESc + shockDE3 + shockNL3 + shockES3 + shockIT3, data = data3))
summariesFRES6 <- lapply(FRES6, summary)
FRES6conf95 <- lapply(FRES6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
FRES6conf68 <- lapply(FRES6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
FRES6up95 <- lapply(1:13, function(x) FRES6conf95[[x]][2,2])
FRES6low95 <- lapply(1:13, function(x) FRES6conf95[[x]][2,1])
FRES6up68 <- lapply(1:13, function(x) FRES6conf68[[x]][2,2])
FRES6low68 <- lapply(1:13, function(x) FRES6conf68[[x]][2,1])
gammaFRES <- lapply(summariesFRES6, function(x) x$coefficients[2,1])
names(gammaFRES) <- paste("gammaFRES", 0:h, sep = "")

# -- Cumulative multiplier
mFRESc <- lapply(1:13, function(x) cumsum(as.numeric(betaFRES))[x] / cumsum(as.numeric(gammaFRES))[x])
mFRESc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaFRES) / as.numeric(gammaFRES))[x])

# -- Generate IRF graph (as in Fig. 1 of Alloza et al.)
v1 <- data.frame(cbind(betaFRES = unlist(betaFRES), FRES5up95 = unlist(FRES5up95), FRES5low95 = unlist(FRES5low95), 
                       FRES5up68 = unlist(FRES5up68), FRES5low68 = unlist(FRES5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfFRES <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfFRES <- irfFRES + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfFRES1 <- irfFRES + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.75, 0.75, 0.25)) +
  ggtitle("ES - GDP change (GOV shock in FR)")
irfFRES2 <- irfFRES1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfFRES2

# -- Generate IRF graph of GOV response
v1 <- data.frame(cbind(gammaFRES = unlist(gammaFRES), FRES6up95 = unlist(FRES6up95), FRES6low95 = unlist(FRES6low95), 
                       FRES6up68 = unlist(FRES6up68), FRES6low68 = unlist(FRES6low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfFRES3 <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfFRES3 <- irfFRES3 + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfFRES4 <- irfFRES3 + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.4, 0.4, 0.2)) +
  ggtitle("FR - GOV change (GOV shock in FR)")
irfFRES5 <- irfFRES4 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfFRES5


# --- Effect of Spanish govt spending shock on France

# -- Equation 5
lhsESFR50 <- (data3$ryFR - data3$l1.ryFR) / data3$l1.ryFR
lhsESFR5 <- lapply(1:h, function(x) (data3[, 78+x] - data3$l1.ryFR) / data3$l1.ryFR)
lhsESFR5 <- data.frame(lhsESFR5)
names(lhsESFR5) = paste("lhsESFR5", 1:h, sep = "")
data3 <- cbind(data3, lhsESFR50, lhsESFR5)
ESFR5 <- lapply(1:13, function(x) lm(data3[, 178+x] ~ shockES3 + l1.debtFR + l1.intFR + l1.lrtrFR + l1.lrgFR + l1.lryFRc + l2.debtFR + l2.intFR + l2.lrtrFR + l2.lrgFR + l2.lryFRc + l3.debtFR + l3.intFR + l3.lrtrFR + l3.lrgFR + l3.lryFRc + l4.debtFR + l4.intFR + l4.lrtrFR + l4.lrgFR + l4.lryFRc + shockDE3 + shockFR3 + shockIT3 + shockNL3, data = data3))
summariesESFR5 <- lapply(ESFR5, summary)
ESFR5conf95 <- lapply(ESFR5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ESFR5conf68 <- lapply(ESFR5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ESFR5up95 <- lapply(1:13, function(x) ESFR5conf95[[x]][2,2])
ESFR5low95 <- lapply(1:13, function(x) ESFR5conf95[[x]][2,1])
ESFR5up68 <- lapply(1:13, function(x) ESFR5conf68[[x]][2,2])
ESFR5low68 <- lapply(1:13, function(x) ESFR5conf68[[x]][2,1])
betaESFR <- lapply(summariesESFR5, function(x) x$coefficients[2,1])
names(betaESFR) <- paste("betaESFR", 0:h, sep = "")

# -- Equation 6
lhsESFR60 <- (data3$rgES - data3$l1.rgES) / data3$l1.ryFR
lhsESFR6 <- lapply(1:h, function(x) (data3[, 90+x] - data3$l1.rgES) / data3$l1.ryFR)
lhsESFR6 <- data.frame(lhsESFR6)
names(lhsESFR6) = paste("lhsESFR6", 1:h, sep = "")
data3 <- cbind(data3, lhsESFR60, lhsESFR6)
ESFR6 <- lapply(1:13, function(x) lm(data3[, 191+x] ~ shockES3 + l1.debtES + l1.intES + l1.lrtrES + l1.lrgES + l1.lryFRc + l2.debtES + l2.intES + l2.lrtrES + l2.lrgES + l2.lryFRc + l3.debtES + l3.intES + l3.lrtrES + l3.lrgES + l3.lryFRc + l4.debtES + l4.intES + l4.lrtrES + l4.lrgES + l4.lryFRc + shockDE3 + shockFR3 + shockIT3 + shockNL3, data = data3))
summariesESFR6 <- lapply(ESFR6, summary)
ESFR6conf95 <- lapply(ESFR6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ESFR6conf68 <- lapply(ESFR6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ESFR6up95 <- lapply(1:13, function(x) ESFR6conf95[[x]][2,2])
ESFR6low95 <- lapply(1:13, function(x) ESFR6conf95[[x]][2,1])
ESFR6up68 <- lapply(1:13, function(x) ESFR6conf68[[x]][2,2])
ESFR6low68 <- lapply(1:13, function(x) ESFR6conf68[[x]][2,1])
gammaESFR <- lapply(summariesESFR6, function(x) x$coefficients[2,1])
names(gammaESFR) <- paste("gammaESFR", 0:h, sep = "")

# -- Cumulative multiplier
mESFRc <- lapply(1:13, function(x) cumsum(as.numeric(betaESFR))[x] / cumsum(as.numeric(gammaESFR))[x])
mESFRc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaESFR) / as.numeric(gammaESFR))[x])

# -- Generate IRF graph (as in Fig. 1 of Alloza et al.)
v1 <- data.frame(cbind(betaESFR = unlist(betaESFR), ESFR5up95 = unlist(ESFR5up95), ESFR5low95 = unlist(ESFR5low95), 
                       ESFR5up68 = unlist(ESFR5up68), ESFR5low68 = unlist(ESFR5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfESFR <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfESFR <- irfESFR + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfESFR1 <- irfESFR + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.3, 0.5, 0.2)) +
  ggtitle("FR - GDP change (GOV shock in ES)")
irfESFR2 <- irfESFR1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfESFR2

# -- Generate IRF graph of GOV response
v1 <- data.frame(cbind(gammaESFR = unlist(gammaESFR), ESFR6up95 = unlist(ESFR6up95), ESFR6low95 = unlist(ESFR6low95), 
                       ESFR6up68 = unlist(ESFR6up68), ESFR6low68 = unlist(ESFR6low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfESFR3 <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfESFR3 <- irfESFR3 + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfESFR4 <- irfESFR3 + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.2, 0.3, 0.1)) +
  ggtitle("ES - GOV change (GOV shock in ES)")
irfESFR5 <- irfESFR4 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfESFR5
