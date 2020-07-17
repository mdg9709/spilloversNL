# --- Effect of Spanish govt spending shock on Italy

# Data period: 1980q1-2018q4
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
shockFR <- FR$shockFR
data <- cbind(IT, ES, oIT, oES, shockNL, shockDE, shockFR)
data1 <- subset(data, select = -c(2:14, 28:41))
names(data1)[2] <- "debtIT"
names(data1)[14] <- "shockIT"
names(data1)[15] <- "debtES"
names(data1)[27] <- "shockES"

# -- Re-scaling of left-hand side of Equations (5) and (6):
data2 <- data1 %>%
  mutate(l1.rgIT = lag(rgIT), l1.rgES = lag(rgES), l1.ryIT = lag(ryIT), l1.ryES = lag(ryES)) %>%
  mutate(l1.ryNL = lag(NL$ryNL), l1.ryFR = lag(FR$ryFR), l1.ryDE = lag(DE$ryDE)) %>%
  mutate(l1.lrgIT = log(l1.rgIT), l1.lrgES = log(l1.rgES), l1.lryIT = log(l1.ryIT), l1.lryES = log(l1.ryES)) %>%
  mutate(shockIT2 = (shockIT / l1.ryES) / sd((shockIT / l1.ryES), na.rm = TRUE), 
         shockFR2 = (shockFR / l1.ryFR) / sd((shockFR / l1.ryFR), na.rm = TRUE),
         shockNL2 = (shockNL / l1.ryNL) / sd((shockNL / l1.ryNL), na.rm = TRUE), 
         shockDE2 = (shockDE / l1.ryDE) / sd((shockDE / l1.ryDE), na.rm = TRUE),
         shockES2 = (shockES / l1.ryIT) / sd((shockES / l1.ryIT), na.rm = TRUE)) %>% 
  mutate(shockIT3 = shockIT2 / 100, shockES3 = shockES2 / 100, shockFR3 = shockFR2 / 100, 
         shockDE3 = shockDE2 / 100, shockNL3 = shockNL2 / 100) %>%
  mutate(shockIT4 = shockIT2 / 10000, shockES4 = shockES2 / 10000, shockFR4 = shockFR2 / 10000, 
         shockDE4 = shockDE2 / 10000, shockNL4 = shockNL2 / 10000) %>%
  mutate(ryIT.l1 = lead(ryIT), ryIT.l2 = lead(ryIT, n = 2), ryIT.l3 = lead(ryIT, n = 3), 
         ryIT.l4 = lead(ryIT, n = 4), ryIT.l5 = lead(ryIT, n = 5), ryIT.l6 = lead(ryIT, n = 6),
         ryIT.l7 = lead(ryIT, n = 7), ryIT.l8 = lead(ryIT, n = 8), ryIT.l9 = lead(ryIT, n = 9),
         ryIT.l10 = lead(ryIT, n = 10), ryIT.l11 = lead(ryIT, n = 11), ryIT.l12 = lead(ryIT, n = 12)) %>%
  mutate(ryES.l1 = lead(ryES), ryES.l2 = lead(ryES, n = 2), ryES.l3 = lead(ryES, n = 3), 
         ryES.l4 = lead(ryES, n = 4), ryES.l5 = lead(ryES, n = 5), ryES.l6 = lead(ryES, n = 6),
         ryES.l7 = lead(ryES, n = 7), ryES.l8 = lead(ryES, n = 8), ryES.l9 = lead(ryES, n = 9),
         ryES.l10 = lead(ryES, n = 10), ryES.l11 = lead(ryES, n = 11), ryES.l12 = lead(ryES, n = 12)) %>%
  mutate(rgIT.l1 = lead(rgIT), rgIT.l2 = lead(rgIT, n = 2), rgIT.l3 = lead(rgIT, n = 3), 
         rgIT.l4 = lead(rgIT, n = 4), rgIT.l5 = lead(rgIT, n = 5), rgIT.l6 = lead(rgIT, n = 6),
         rgIT.l7 = lead(rgIT, n = 7), rgIT.l8 = lead(rgIT, n = 8), rgIT.l9 = lead(rgIT, n = 9),
         rgIT.l10 = lead(rgIT, n = 10), rgIT.l11 = lead(rgIT, n = 11), rgIT.l12 = lead(rgIT, n = 12)) %>%
  mutate(rgES.l1 = lead(rgES), rgES.l2 = lead(rgES, n = 2), rgES.l3 = lead(rgES, n = 3), 
         rgES.l4 = lead(rgES, n = 4), rgES.l5 = lead(rgES, n = 5), rgES.l6 = lead(rgES, n = 6),
         rgES.l7 = lead(rgES, n = 7), rgES.l8 = lead(rgES, n = 8), rgES.l9 = lead(rgES, n = 9),
         rgES.l10 = lead(rgES, n = 10), rgES.l11 = lead(rgES, n = 11), rgES.l12 = lead(rgES, n = 12)) %>%
  mutate(l1.debtIT = lag(debtIT, n = 1), l1.intIT = lag(intIT, n = 1), l1.lrtrIT = lag(lrtrIT, n = 1),
         l1.lrgIT = lag(lrgIT, n = 1), l1.lryITc = lag(lryITc, n = 1), l2.debtIT = lag(debtIT, n = 2),
         l2.intIT = lag(intIT, n = 2), l2.lrtrIT = lag(lrtrIT, n = 2), l2.lrgIT = lag(lrgIT, n = 2),
         l2.lryITc = lag(lryITc, n = 2), l3.debtIT = lag(debtIT, n = 3), l3.intIT = lag(intIT, n = 3),
         l3.lrtrIT = lag(lrtrIT, n = 3), l3.lrgIT = lag(lrgIT, n = 3), l3.lryITc = lag(lryITc, n = 3),
         l4.debtIT = lag(debtIT, n = 4), l4.intIT = lag(intIT, n = 4), l4.lrtrIT = lag(lrtrIT, n = 4),
         l4.lrgIT = lag(lrgIT, n = 4), l4.lryITc = lag(lryITc, n = 4)) %>%
  mutate(l1.debtES = lag(debtES, n = 1), l1.intES = lag(intES, n = 1), l1.lrtrES = lag(lrtrES, n = 1),
         l1.lrgES = lag(lrgES, n = 1), l1.lryESc = lag(lryESc, n = 1), l2.debtES = lag(debtES, n = 2),
         l2.intES = lag(intES, n = 2), l2.lrtrES = lag(lrtrES, n = 2), l2.lrgES = lag(lrgES, n = 2),
         l2.lryESc = lag(lryESc, n = 2), l3.debtES = lag(debtES, n = 3), l3.intES = lag(intES, n = 3),
         l3.lrtrES = lag(lrtrES, n = 3), l3.lrgES = lag(lrgES, n = 3), l3.lryESc = lag(lryESc, n = 3),
         l4.debtES = lag(debtES, n = 4), l4.intES = lag(intES, n = 4), l4.lrtrES = lag(lrtrES, n = 4),
         l4.lrgES = lag(lrgES, n = 4), l4.lryESc = lag(lryESc, n = 4))

# -- OLS regressions

# -- Equation 5
lhsITES50 <- (data2$ryES - data2$l1.ryES) / data2$l1.ryES
lhsITES5 <- lapply(1:h, function(x) (data2[, 78+x] - data2$l1.ryES) / data2$l1.ryES)
lhsITES5 <- data.frame(lhsITES5)
names(lhsITES5) = paste("lhsITES5", 1:h, sep = "")
data3 <- cbind(data2, lhsITES50, lhsITES5)
ITES5 <- lapply(1:13, function(x) lm(data3[, 152+x] ~ shockIT2 + l1.debtES + l1.intES + l1.lrtrES + l1.lrgES + l1.lryESc + l2.debtES + l2.intES + l2.lrtrES + l2.lrgES + l2.lryESc + l3.debtES + l3.intES + l3.lrtrES + l3.lrgES + l3.lryESc + l4.debtES + l4.intES + l4.lrtrES + l4.lrgES + l4.lryESc + shockDE2 + shockFR2 + shockES2 + shockNL2, data = data3))
summariesITES5 <- lapply(ITES5, summary)
ITES5conf95 <- lapply(ITES5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ITES5conf68 <- lapply(ITES5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ITES5up95 <- lapply(1:13, function(x) ITES5conf95[[x]][2,2])
ITES5low95 <- lapply(1:13, function(x) ITES5conf95[[x]][2,1])
ITES5up68 <- lapply(1:13, function(x) ITES5conf68[[x]][2,2])
ITES5low68 <- lapply(1:13, function(x) ITES5conf68[[x]][2,1])
betaITES <- lapply(summariesITES5, function(x) x$coefficients[2,1])
names(betaITES) <- paste("betaITES", 0:h, sep = "")

# -- Equation 6
lhsITES60 <- (data3$rgIT - data3$l1.rgIT) / data3$l1.ryES
lhsITES6 <- lapply(1:h, function(x) (data3[, 90+x] - data3$l1.rgIT) / data3$l1.ryES)
lhsITES6 <- data.frame(lhsITES6)
names(lhsITES6) = paste("lhsITES6", 1:h, sep = "")
data3 <- cbind(data3, lhsITES60, lhsITES6)
ITES6 <- lapply(1:13, function(x) lm(data3[, 165+x] ~ shockIT3 + l1.debtIT + l1.intIT + l1.lrtrIT + l1.lrgIT + l1.lryITc + l2.debtIT + l2.intIT + l2.lrtrIT + l2.lrgIT + l2.lryITc + l3.debtIT + l3.intIT + l3.lrtrIT + l3.lrgIT + l3.lryITc + l4.debtIT + l4.intIT + l4.lrtrIT + l4.lrgIT + l4.lryITc + shockDE3 + shockFR3 + shockES3 + shockNL3, data = data3))
summariesITES6 <- lapply(ITES6, summary)
ITES6conf95 <- lapply(ITES6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ITES6conf68 <- lapply(ITES6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ITES6up95 <- lapply(1:13, function(x) ITES6conf95[[x]][2,2])
ITES6low95 <- lapply(1:13, function(x) ITES6conf95[[x]][2,1])
ITES6up68 <- lapply(1:13, function(x) ITES6conf68[[x]][2,2])
ITES6low68 <- lapply(1:13, function(x) ITES6conf68[[x]][2,1])
gammaITES <- lapply(summariesITES6, function(x) x$coefficients[2,1])
names(gammaITES) <- paste("gammaITES", 1:h, sep = "")

# -- Cumulative multiplier
mITESc <- lapply(1:13, function(x) cumsum(as.numeric(betaITES))[x] / cumsum(as.numeric(gammaITES))[x])
mITESc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaITES) / as.numeric(gammaITES))[x])

# -- Generate IRF graph (as in Fig. 1 of Alloza et al.)
v1 <- data.frame(cbind(betaITES = unlist(betaITES), ITES5up95 = unlist(ITES5up95), ITES5low95 = unlist(ITES5low95), 
                       ITES5up68 = unlist(ITES5up68), ITES5low68 = unlist(ITES5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfITES <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfITES <- irfITES + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfITES1 <- irfITES + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-1, 1, 0.25)) +
  ggtitle("ES - GDP change (GOV shock in IT)")
irfITES2 <- irfITES1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfITES2

# -- Generate IRF graph of GOV response
v1 <- data.frame(cbind(gammaITES = unlist(gammaITES), ITES6up95 = unlist(ITES6up95), ITES6low95 = unlist(ITES6low95), 
                       ITES6up68 = unlist(ITES6up68), ITES6low68 = unlist(ITES6low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfITES3 <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfITES3 <- irfITES3 + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfITES4 <- irfITES3 + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.4, 0.8, 0.2)) +
  ggtitle("IT - GOV change (GOV shock in IT)")
irfITES5 <- irfITES4 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfITES5


# --- Effect of Spanish govt spending shock on Italy

# -- Equation 5
lhsESIT50 <- (data3$ryIT - data3$l1.ryIT) / data3$l1.ryIT
lhsESIT5 <- lapply(1:h, function(x) (data3[, 66+x] - data3$l1.ryIT) / data3$l1.ryIT)
lhsESIT5 <- data.frame(lhsESIT5)
names(lhsESIT5) = paste("lhsESIT5", 1:h, sep = "")
data3 <- cbind(data3, lhsESIT50, lhsESIT5)
ESIT5 <- lapply(1:13, function(x) lm(data3[, 178+x] ~ shockES2 + l1.debtIT + l1.intIT + l1.lrtrIT + l1.lrgIT + l1.lryITc + l2.debtIT + l2.intIT + l2.lrtrIT + l2.lrgIT + l2.lryITc + l3.debtIT + l3.intIT + l3.lrtrIT + l3.lrgIT + l3.lryITc + l4.debtIT + l4.intIT + l4.lrtrIT + l4.lrgIT + l4.lryITc + shockDE2 + shockFR2 + shockIT2 + shockNL2, data = data3))
summariesESIT5 <- lapply(ESIT5, summary)
ESIT5conf95 <- lapply(ESIT5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ESIT5conf68 <- lapply(ESIT5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ESIT5up95 <- lapply(1:13, function(x) ESIT5conf95[[x]][2,2])
ESIT5low95 <- lapply(1:13, function(x) ESIT5conf95[[x]][2,1])
ESIT5up68 <- lapply(1:13, function(x) ESIT5conf68[[x]][2,2])
ESIT5low68 <- lapply(1:13, function(x) ESIT5conf68[[x]][2,1])
betaESIT <- lapply(summariesESIT5, function(x) x$coefficients[2,1])
names(betaESIT) <- paste("betaESIT", 0:h, sep = "")

# -- Equation 6
lhsESIT60 <- (data3$rgES - data3$l1.rgES) / data3$l1.ryIT
lhsESIT6 <- lapply(1:h, function(x) (data3[, 102+x] - data3$l1.rgES) / data3$l1.ryIT)
lhsESIT6 <- data.frame(lhsESIT6)
names(lhsESIT6) = paste("lhsESIT6", 1:h, sep = "")
data3 <- cbind(data3, lhsESIT60, lhsESIT6)
ESIT6 <- lapply(1:13, function(x) lm(data3[, 191+x] ~ shockES3 + l1.debtES + l1.intES + l1.lrtrES + l1.lrgES + l1.lryITc + l2.debtES + l2.intES + l2.lrtrES + l2.lrgES + l2.lryITc + l3.debtES + l3.intES + l3.lrtrES + l3.lrgES + l3.lryITc + l4.debtES + l4.intES + l4.lrtrES + l4.lrgES + l4.lryITc + shockDE3 + shockFR3 + shockIT3 + shockNL3, data = data3))
summariesESIT6 <- lapply(ESIT6, summary)
ESIT6conf95 <- lapply(ESIT6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ESIT6conf68 <- lapply(ESIT6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ESIT6up95 <- lapply(1:13, function(x) ESIT6conf95[[x]][2,2])
ESIT6low95 <- lapply(1:13, function(x) ESIT6conf95[[x]][2,1])
ESIT6up68 <- lapply(1:13, function(x) ESIT6conf68[[x]][2,2])
ESIT6low68 <- lapply(1:13, function(x) ESIT6conf68[[x]][2,1])
gammaESIT <- lapply(summariesESIT6, function(x) x$coefficients[2,1])
names(gammaESIT) <- paste("gammaESIT", 0:h, sep = "")

# -- Cumulative multiplier
mESITc <- lapply(1:13, function(x) cumsum(as.numeric(betaESIT))[x] / cumsum(as.numeric(gammaESIT))[x])
mESITc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaESIT) / as.numeric(gammaESIT))[x])

# -- Generate IRF graph (as in Fig. 1 of Alloza et al.)
v1 <- data.frame(cbind(betaESIT = unlist(betaESIT), ESIT5up95 = unlist(ESIT5up95), ESIT5low95 = unlist(ESIT5low95), 
                       ESIT5up68 = unlist(ESIT5up68), ESIT5low68 = unlist(ESIT5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfESIT <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfESIT <- irfESIT + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfESIT1 <- irfESIT + coord_cartesian(xlim = c(1, 12)) +
  scale_x_continuous(breaks = seq(1, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.2, 0.6, 0.1)) +
  ggtitle("IT - GDP change (GOV shock in ES)")
irfESIT2 <- irfESIT1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfESIT2

# -- Generate IRF graph of GOV response
v1 <- data.frame(cbind(gammaESIT = unlist(gammaESIT), ESIT6up95 = unlist(ESIT6up95), ESIT6low95 = unlist(ESIT6low95), 
                       ESIT6up68 = unlist(ESIT6up68), ESIT6low68 = unlist(ESIT6low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfESIT3 <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfESIT3 <- irfESIT3 + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfESIT4 <- irfESIT3 + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.2, 0.3, 0.1)) +
  ggtitle("ES - GOV change (GOV shock in ES)")
irfESIT5 <- irfESIT4 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfESIT5
