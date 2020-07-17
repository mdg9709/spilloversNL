# --- Effect of Spanish govt spending shock on Germany

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
oDE <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/ABP_other_variables.xlsx", 
                  sheet = "DE", range = "B1:F157")
colnames(oDE) <- c("rxDE", "rmDE", "R", "D", "pop")
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
shockNL <- NL$shockNL
shockIT <- IT$shockIT
data <- cbind(DE, ES, oDE, oES, shockNL, shockFR, shockIT)
data1 <- subset(data, select = -c(2:14, 28:41))
names(data1)[2] <- "debtDE"
names(data1)[14] <- "shockDE"
names(data1)[15] <- "debtES"
names(data1)[27] <- "shockES"

# -- Re-scaling of left-hand side of Equations (5) and (6):
data2 <- data1 %>%
  mutate(l1.rgDE = lag(rgDE), l1.rgES = lag(rgES), l1.ryDE = lag(ryDE), l1.ryES = lag(ryES)) %>%
  mutate(l1.ryNL = lag(NL$ryNL), l1.ryFR = lag(FR$ryFR), l1.ryIT = lag(IT$ryIT)) %>%
  mutate(l1.lrgDE = log(l1.rgDE), l1.lrgES = log(l1.rgES), l1.lryDE = log(l1.ryDE), l1.lryES = log(l1.ryES)) %>%
  mutate(shockNL2 = (shockNL / l1.ryNL) / sd((shockNL / l1.ryNL), na.rm = TRUE), 
         shockFR2 = (shockFR / l1.ryFR) / sd((shockFR / l1.ryFR), na.rm = TRUE),
         shockES2 = (shockES / l1.ryDE) / sd((shockES / l1.ryDE), na.rm = TRUE), 
         shockDE2 = (shockDE / l1.ryES) / sd((shockDE / l1.ryES), na.rm = TRUE),
         shockIT2 = (shockIT / l1.ryIT) / sd((shockIT / l1.ryIT), na.rm = TRUE)) %>% 
  mutate(shockIT3 = shockIT2 / 100, shockES3 = shockES2 / 100, shockFR3 = shockFR2 / 100, 
         shockDE3 = shockDE2 / 100, shockNL3 = shockNL2 / 100) %>%
  mutate(shockIT4 = shockIT2 / 10000, shockES4 = shockES2 / 10000, shockFR4 = shockFR2 / 10000, 
         shockDE4 = shockDE2 / 10000, shockNL4 = shockNL2 / 10000) %>%
  mutate(ryDE.l1 = lead(ryDE), ryDE.l2 = lead(ryDE, n = 2), ryDE.l3 = lead(ryDE, n = 3), 
         ryDE.l4 = lead(ryDE, n = 4), ryDE.l5 = lead(ryDE, n = 5), ryDE.l6 = lead(ryDE, n = 6),
         ryDE.l7 = lead(ryDE, n = 7), ryDE.l8 = lead(ryDE, n = 8), ryDE.l9 = lead(ryDE, n = 9),
         ryDE.l10 = lead(ryDE, n = 10), ryDE.l11 = lead(ryDE, n = 11), ryDE.l12 = lead(ryDE, n = 12)) %>%
  mutate(ryES.l1 = lead(ryES), ryES.l2 = lead(ryES, n = 2), ryES.l3 = lead(ryES, n = 3), 
         ryES.l4 = lead(ryES, n = 4), ryES.l5 = lead(ryES, n = 5), ryES.l6 = lead(ryES, n = 6),
         ryES.l7 = lead(ryES, n = 7), ryES.l8 = lead(ryES, n = 8), ryES.l9 = lead(ryES, n = 9),
         ryES.l10 = lead(ryES, n = 10), ryES.l11 = lead(ryES, n = 11), ryES.l12 = lead(ryES, n = 12)) %>%
  mutate(rgDE.l1 = lead(rgDE), rgDE.l2 = lead(rgDE, n = 2), rgDE.l3 = lead(rgDE, n = 3), 
         rgDE.l4 = lead(rgDE, n = 4), rgDE.l5 = lead(rgDE, n = 5), rgDE.l6 = lead(rgDE, n = 6),
         rgDE.l7 = lead(rgDE, n = 7), rgDE.l8 = lead(rgDE, n = 8), rgDE.l9 = lead(rgDE, n = 9),
         rgDE.l10 = lead(rgDE, n = 10), rgDE.l11 = lead(rgDE, n = 11), rgDE.l12 = lead(rgDE, n = 12)) %>%
  mutate(rgES.l1 = lead(rgES), rgES.l2 = lead(rgES, n = 2), rgES.l3 = lead(rgES, n = 3), 
         rgES.l4 = lead(rgES, n = 4), rgES.l5 = lead(rgES, n = 5), rgES.l6 = lead(rgES, n = 6),
         rgES.l7 = lead(rgES, n = 7), rgES.l8 = lead(rgES, n = 8), rgES.l9 = lead(rgES, n = 9),
         rgES.l10 = lead(rgES, n = 10), rgES.l11 = lead(rgES, n = 11), rgES.l12 = lead(rgES, n = 12)) %>%
  mutate(l1.debtDE = lag(debtDE, n = 1), l1.intDE = lag(intDE, n = 1), l1.lrtrDE = lag(lrtrDE, n = 1),
         l1.lrgDE = lag(lrgDE, n = 1), l1.lryDEc = lag(lryDEc, n = 1), l2.debtDE = lag(debtDE, n = 2),
         l2.intDE = lag(intDE, n = 2), l2.lrtrDE = lag(lrtrDE, n = 2), l2.lrgDE = lag(lrgDE, n = 2),
         l2.lryDEc = lag(lryDEc, n = 2), l3.debtDE = lag(debtDE, n = 3), l3.intDE = lag(intDE, n = 3),
         l3.lrtrDE = lag(lrtrDE, n = 3), l3.lrgDE = lag(lrgDE, n = 3), l3.lryDEc = lag(lryDEc, n = 3),
         l4.debtDE = lag(debtDE, n = 4), l4.intDE = lag(intDE, n = 4), l4.lrtrDE = lag(lrtrDE, n = 4),
         l4.lrgDE = lag(lrgDE, n = 4), l4.lryDEc = lag(lryDEc, n = 4)) %>%
  mutate(l1.debtES = lag(debtES, n = 1), l1.intES = lag(intES, n = 1), l1.lrtrES = lag(lrtrES, n = 1),
         l1.lrgES = lag(lrgES, n = 1), l1.lryESc = lag(lryESc, n = 1), l2.debtES = lag(debtES, n = 2),
         l2.intES = lag(intES, n = 2), l2.lrtrES = lag(lrtrES, n = 2), l2.lrgES = lag(lrgES, n = 2),
         l2.lryESc = lag(lryESc, n = 2), l3.debtES = lag(debtES, n = 3), l3.intES = lag(intES, n = 3),
         l3.lrtrES = lag(lrtrES, n = 3), l3.lrgES = lag(lrgES, n = 3), l3.lryESc = lag(lryESc, n = 3),
         l4.debtES = lag(debtES, n = 4), l4.intES = lag(intES, n = 4), l4.lrtrES = lag(lrtrES, n = 4),
         l4.lrgES = lag(lrgES, n = 4), l4.lryESc = lag(lryESc, n = 4))

# -- OLS regressions

# -- Equation 5
lhsESDE50 <- (data2$ryDE - data2$l1.ryDE) / data2$l1.ryDE
lhsESDE5 <- lapply(1:h, function(x) (data2[, 66+x] - data2$l1.ryDE) / data2$l1.ryDE)
lhsESDE5 <- data.frame(lhsESDE5)
names(lhsESDE5) = paste("lhsESDE5", 1:h, sep = "")
data3 <- cbind(data2, lhsESDE50, lhsESDE5)
ESDE5 <- lapply(1:13, function(x) lm(data3[, 152+x] ~ shockES3 + l1.debtDE + l1.intDE + l1.lrtrDE + l1.lrgDE + l1.lryDEc + l2.debtDE + l2.intDE + l2.lrtrDE + l2.lrgDE + l2.lryDEc + l3.debtDE + l3.intDE + l3.lrtrDE + l3.lrgDE + l3.lryDEc + l4.debtDE + l4.intDE + l4.lrtrDE + l4.lrgDE + l4.lryDEc + shockDE3 + shockFR3 + shockNL3 + shockIT3, data = data3))
summariesESDE5 <- lapply(ESDE5, summary)
ESDE5conf95 <- lapply(ESDE5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ESDE5conf68 <- lapply(ESDE5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ESDE5up95 <- lapply(1:13, function(x) ESDE5conf95[[x]][2,2])
ESDE5low95 <- lapply(1:13, function(x) ESDE5conf95[[x]][2,1])
ESDE5up68 <- lapply(1:13, function(x) ESDE5conf68[[x]][2,2])
ESDE5low68 <- lapply(1:13, function(x) ESDE5conf68[[x]][2,1])
betaESDE <- lapply(summariesESDE5, function(x) x$coefficients[2,1])
names(betaESDE) <- paste("betaESDE", 0:h, sep = "")

# -- Equation 6
lhsESDE60 <- (data3$rgES - data3$l1.rgES) / data3$l1.ryDE
lhsESDE6 <- lapply(1:h, function(x) (data3[, 102+x] - data3$l1.rgES) / data3$l1.ryDE)
lhsESDE6 <- data.frame(lhsESDE6)
names(lhsESDE6) = paste("lhsESDE6", 1:h, sep = "")
data3 <- cbind(data3, lhsESDE60, lhsESDE6)
ESDE6 <- lapply(1:13, function(x) lm(data3[, 165+x] ~ shockES3 + l1.debtES + l1.intES + l1.lrtrES + l1.lrgES + l1.lryESc + l2.debtES + l2.intES + l2.lrtrES + l2.lrgES + l2.lryESc + l3.debtES + l3.intES + l3.lrtrES + l3.lrgES + l3.lryESc + l4.debtES + l4.intES + l4.lrtrES + l4.lrgES + l4.lryESc + shockDE3 + shockFR3 + shockNL3 + shockIT3, data = data3))
summariesESDE6 <- lapply(ESDE6, summary)
ESDE6conf95 <- lapply(ESDE6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ESDE6conf68 <- lapply(ESDE6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ESDE6up95 <- lapply(1:13, function(x) ESDE6conf95[[x]][2,2])
ESDE6low95 <- lapply(1:13, function(x) ESDE6conf95[[x]][2,1])
ESDE6up68 <- lapply(1:13, function(x) ESDE6conf68[[x]][2,2])
ESDE6low68 <- lapply(1:13, function(x) ESDE6conf68[[x]][2,1])
gammaESDE <- lapply(summariesESDE6, function(x) x$coefficients[2,1])
names(gammaESDE) <- paste("gammaESDE", 0:h, sep = "")

# -- Cumulative multiplier
mESDEc <- lapply(1:13, function(x) cumsum(as.numeric(betaESDE))[x] / cumsum(as.numeric(gammaESDE))[x])
mESDEc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaESDE) / as.numeric(gammaESDE))[x])

# -- Generate IRF graph (as in Fig. 1 of Alloza et al.)
v1 <- data.frame(cbind(betaESDE = unlist(betaESDE), ESDE5up95 = unlist(ESDE5up95), ESDE5low95 = unlist(ESDE5low95), 
                       ESDE5up68 = unlist(ESDE5up68), ESDE5low68 = unlist(ESDE5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfESDE <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfESDE <- irfESDE + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfESDE1 <- irfESDE + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.5, 0.7, 0.2)) +
  ggtitle("DE - GDP change (GOV shock in ES)")
irfESDE2 <- irfESDE1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfESDE2

# -- Generate IRF graph of GOV response
v1 <- data.frame(cbind(gammaESDE = unlist(gammaESDE), ESDE6up95 = unlist(ESDE6up95), ESDE6low95 = unlist(ESDE6low95), 
                       ESDE6up68 = unlist(ESDE6up68), ESDE6low68 = unlist(ESDE6low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfESDE3 <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfESDE3 <- irfESDE3 + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfESDE4 <- irfESDE3 + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.2, 0.25, 0.05)) +
  ggtitle("ES - GOV change (GOV shock in ES)")
irfESDE5 <- irfESDE4 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfESDE5


# --- Effect of German govt spending shock on Spain

# -- Equation 5
lhsDEES50 <- (data3$ryES - data3$l1.ryES) / data3$l1.ryES
lhsDEES5 <- lapply(1:h, function(x) (data3[, 78+x] - data3$l1.ryES) / data3$l1.ryES)
lhsDEES5 <- data.frame(lhsDEES5)
names(lhsDEES5) = paste("lhsDEES5", 1:h, sep = "")
data3 <- cbind(data3, lhsDEES50, lhsDEES5)
DEES5 <- lapply(1:13, function(x) lm(data3[, 178+x] ~ shockDE2 + l1.debtES + l1.intES + l1.lrtrES + l1.lrgES + l1.lryESc + l2.debtES + l2.intES + l2.lrtrES + l2.lrgES + l2.lryESc + l3.debtES + l3.intES + l3.lrtrES + l3.lrgES + l3.lryESc + l4.debtES + l4.intES + l4.lrtrES + l4.lrgES + l4.lryESc + shockIT2 + shockFR2 + shockES2 + shockNL2, data = data3))
summariesDEES5 <- lapply(DEES5, summary)
DEES5conf95 <- lapply(DEES5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
DEES5conf68 <- lapply(DEES5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
DEES5up95 <- lapply(1:13, function(x) DEES5conf95[[x]][2,2])
DEES5low95 <- lapply(1:13, function(x) DEES5conf95[[x]][2,1])
DEES5up68 <- lapply(1:13, function(x) DEES5conf68[[x]][2,2])
DEES5low68 <- lapply(1:13, function(x) DEES5conf68[[x]][2,1])
betaDEES <- lapply(summariesDEES5, function(x) x$coefficients[2,1])
names(betaDEES) <- paste("betaDEES", 0:h, sep = "")

# -- Equation 6
lhsDEES60 <- (data3$rgDE - data3$l1.rgDE) / data3$l1.ryES
lhsDEES6 <- lapply(1:h, function(x) (data3[, 90+x] - data3$l1.rgDE) / data3$l1.ryES)
lhsDEES6 <- data.frame(lhsDEES6)
names(lhsDEES6) = paste("lhsDEES6", 1:h, sep = "")
data3 <- cbind(data3, lhsDEES60, lhsDEES6)
DEES6 <- lapply(1:13, function(x) lm(data3[, 191+x] ~ shockDE3 + l1.debtDE + l1.intDE + l1.lrtrDE + l1.lrgDE + l1.lryDEc + l2.debtDE + l2.intDE + l2.lrtrDE + l2.lrgDE + l2.lryDEc + l3.debtDE + l3.intDE + l3.lrtrDE + l3.lrgDE + l3.lryDEc + l4.debtDE + l4.intDE + l4.lrtrDE + l4.lrgDE + l4.lryDEc + shockIT3 + shockFR3 + shockES3 + shockNL3, data = data3))
summariesDEES6 <- lapply(DEES6, summary)
DEES6conf95 <- lapply(DEES6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
DEES6conf68 <- lapply(DEES6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
DEES6up95 <- lapply(1:13, function(x) DEES6conf95[[x]][2,2])
DEES6low95 <- lapply(1:13, function(x) DEES6conf95[[x]][2,1])
DEES6up68 <- lapply(1:13, function(x) DEES6conf68[[x]][2,2])
DEES6low68 <- lapply(1:13, function(x) DEES6conf68[[x]][2,1])
gammaDEES <- lapply(summariesDEES6, function(x) x$coefficients[2,1])
names(gammaDEES) <- paste("gammaDEES", 0:h, sep = "")

# -- Cumulative multiplier
mDEESc <- lapply(1:13, function(x) cumsum(as.numeric(betaDEES))[x] / cumsum(as.numeric(gammaDEES))[x])
mDEESc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaDEES) / as.numeric(gammaDEES))[x])

# -- Generate IRF graph (as in Fig. 1 of Alloza et al.)
v1 <- data.frame(cbind(betaDEES = unlist(betaDEES), DEES5up95 = unlist(DEES5up95), DEES5low95 = unlist(DEES5low95), 
                       DEES5up68 = unlist(DEES5up68), DEES5low68 = unlist(DEES5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfDEES <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfDEES <- irfDEES + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfDEES1 <- irfDEES + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.4, 1.6, 0.4)) +
  ggtitle("ES - GDP change (GOV shock in DE)")
irfDEES2 <- irfDEES1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfDEES2

# -- Generate IRF graph of GOV response
v1 <- data.frame(cbind(gammaDEES = unlist(gammaDEES), DEES6up95 = unlist(DEES6up95), DEES6low95 = unlist(DEES6low95), 
                       DEES6up68 = unlist(DEES6up68), DEES6low68 = unlist(DEES6low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfDEES3 <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfDEES3 <- irfDEES3 + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfDEES4 <- irfDEES3 + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.8, 0.8, 0.2)) +
  ggtitle("DE - GOV change (GOV shock in DE)")
irfDEES5 <- irfDEES4 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfDEES5
