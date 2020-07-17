# --- Effect of French govt spending shock on Germany

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
oFR <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/ABP_other_variables.xlsx", 
                  sheet = "FR", range = "B1:F157")
colnames(oFR) <- c("rxFR", "rmFR", "R", "D", "popFR")
oDE <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/ABP_other_variables.xlsx", 
                  sheet = "DE", range = "B1:F157")
colnames(oDE) <- c("rxDE", "rmDE", "R", "D", "popDE")

# Load packages
library(ggplot2)
library(gridExtra)
library(dplyr)
library(lmtest)
library(sandwich)
h <- 12

# Create dataframe
shockES <- ES$shockES
shockNL <- NL$shockNL
shockIT <- IT$shockIT
data <- cbind(DE, FR, oDE, oFR, shockNL, shockES, shockIT)
data1 <- subset(data, select = -c(2:14, 28:41))
names(data1)[2] <- "debtDE"
names(data1)[14] <- "shockDE"
names(data1)[15] <- "debtFR"
names(data1)[27] <- "shockFR"

# -- Re-scaling of left-hand side of Equations (5) and (6):
data2 <- data1 %>%
  mutate(l1.rgDE = lag(rgDE), l1.rgFR = lag(rgFR), l1.ryDE = lag(ryDE), l1.ryFR = lag(ryFR)) %>%
  mutate(l1.ryNL = lag(NL$ryNL), l1.ryES = lag(ES$ryES), l1.ryIT = lag(IT$ryIT)) %>%
  mutate(l1.lrgDE = log(l1.rgDE), l1.lrgFR = log(l1.rgFR), l1.lryDE = log(l1.ryDE), l1.lryFR = log(l1.ryFR)) %>%
  mutate(shockDE2 = (shockDE / l1.ryFR) / sd((shockDE / l1.ryFR), na.rm = TRUE), 
         shockFR2 = (shockFR / l1.ryDE) / sd((shockFR / l1.ryDE), na.rm = TRUE),
         shockIT2 = (shockIT / l1.ryIT) / sd((shockIT / l1.ryIT), na.rm = TRUE), 
         shockES2 = (shockES / l1.ryES) / sd((shockES / l1.ryES), na.rm = TRUE),
         shockNL2 = (shockNL / l1.ryNL) / sd((shockNL / l1.ryNL), na.rm = TRUE)) %>% 
  mutate(shockIT3 = shockIT2 / 100, shockES3 = shockES2 / 100, shockFR3 = shockFR2 / 100, 
         shockDE3 = shockDE2 / 100, shockNL3 = shockNL2 / 100) %>%
  mutate(shockIT4 = shockIT2 * 100, shockES4 = shockES2 * 100, shockFR4 = shockFR2 * 100, 
         shockDE4 = shockDE2 * 100, shockNL4 = shockNL2 * 100) %>%
  mutate(ryDE.l1 = lead(ryDE), ryDE.l2 = lead(ryDE, n = 2), ryDE.l3 = lead(ryDE, n = 3), 
         ryDE.l4 = lead(ryDE, n = 4), ryDE.l5 = lead(ryDE, n = 5), ryDE.l6 = lead(ryDE, n = 6),
         ryDE.l7 = lead(ryDE, n = 7), ryDE.l8 = lead(ryDE, n = 8), ryDE.l9 = lead(ryDE, n = 9),
         ryDE.l10 = lead(ryDE, n = 10), ryDE.l11 = lead(ryDE, n = 11), ryDE.l12 = lead(ryDE, n = 12)) %>%
  mutate(ryFR.l1 = lead(ryFR), ryFR.l2 = lead(ryFR, n = 2), ryFR.l3 = lead(ryFR, n = 3), 
         ryFR.l4 = lead(ryFR, n = 4), ryFR.l5 = lead(ryFR, n = 5), ryFR.l6 = lead(ryFR, n = 6),
         ryFR.l7 = lead(ryFR, n = 7), ryFR.l8 = lead(ryFR, n = 8), ryFR.l9 = lead(ryFR, n = 9),
         ryFR.l10 = lead(ryFR, n = 10), ryFR.l11 = lead(ryFR, n = 11), ryFR.l12 = lead(ryFR, n = 12)) %>%
  mutate(rgDE.l1 = lead(rgDE), rgDE.l2 = lead(rgDE, n = 2), rgDE.l3 = lead(rgDE, n = 3), 
         rgDE.l4 = lead(rgDE, n = 4), rgDE.l5 = lead(rgDE, n = 5), rgDE.l6 = lead(rgDE, n = 6),
         rgDE.l7 = lead(rgDE, n = 7), rgDE.l8 = lead(rgDE, n = 8), rgDE.l9 = lead(rgDE, n = 9),
         rgDE.l10 = lead(rgDE, n = 10), rgDE.l11 = lead(rgDE, n = 11), rgDE.l12 = lead(rgDE, n = 12)) %>%
  mutate(rgFR.l1 = lead(rgFR), rgFR.l2 = lead(rgFR, n = 2), rgFR.l3 = lead(rgFR, n = 3), 
         rgFR.l4 = lead(rgFR, n = 4), rgFR.l5 = lead(rgFR, n = 5), rgFR.l6 = lead(rgFR, n = 6),
         rgFR.l7 = lead(rgFR, n = 7), rgFR.l8 = lead(rgFR, n = 8), rgFR.l9 = lead(rgFR, n = 9),
         rgFR.l10 = lead(rgFR, n = 10), rgFR.l11 = lead(rgFR, n = 11), rgFR.l12 = lead(rgFR, n = 12)) %>%
  mutate(l1.debtDE = lag(debtDE, n = 1), l1.intDE = lag(intDE, n = 1), l1.lrtrDE = lag(lrtrDE, n = 1),
         l1.lrgDE = lag(lrgDE, n = 1), l1.lryDEc = lag(lryDEc, n = 1), l2.debtDE = lag(debtDE, n = 2),
         l2.intDE = lag(intDE, n = 2), l2.lrtrDE = lag(lrtrDE, n = 2), l2.lrgDE = lag(lrgDE, n = 2),
         l2.lryDEc = lag(lryDEc, n = 2), l3.debtDE = lag(debtDE, n = 3), l3.intDE = lag(intDE, n = 3),
         l3.lrtrDE = lag(lrtrDE, n = 3), l3.lrgDE = lag(lrgDE, n = 3), l3.lryDEc = lag(lryDEc, n = 3),
         l4.debtDE = lag(debtDE, n = 4), l4.intDE = lag(intDE, n = 4), l4.lrtrDE = lag(lrtrDE, n = 4),
         l4.lrgDE = lag(lrgDE, n = 4), l4.lryDEc = lag(lryDEc, n = 4)) %>%
  mutate(l1.debtFR = lag(debtFR, n = 1), l1.intFR = lag(intFR, n = 1), l1.lrtrFR = lag(lrtrFR, n = 1),
         l1.lrgFR = lag(lrgFR, n = 1), l1.lryFRc = lag(lryFRc, n = 1), l2.debtFR = lag(debtFR, n = 2),
         l2.intFR = lag(intFR, n = 2), l2.lrtrFR = lag(lrtrFR, n = 2), l2.lrgFR = lag(lrgFR, n = 2),
         l2.lryFRc = lag(lryFRc, n = 2), l3.debtFR = lag(debtFR, n = 3), l3.intFR = lag(intFR, n = 3),
         l3.lrtrFR = lag(lrtrFR, n = 3), l3.lrgFR = lag(lrgFR, n = 3), l3.lryFRc = lag(lryFRc, n = 3),
         l4.debtFR = lag(debtFR, n = 4), l4.intFR = lag(intFR, n = 4), l4.lrtrFR = lag(lrtrFR, n = 4),
         l4.lrgFR = lag(lrgFR, n = 4), l4.lryFRc = lag(lryFRc, n = 4))

# -- OLS regressions

# -- Equation 5
lhsFRDE50 <- (data2$ryDE - data2$l1.ryDE)/ data2$l1.ryDE
lhsFRDE5 <- lapply(1:h, function(x) (data2[, 66+x] - data2$l1.ryDE) / data2$l1.ryDE)
lhsFRDE5 <- data.frame(lhsFRDE5)
names(lhsFRDE5) = paste("lhsFRDE5", 1:h, sep = "")
data3 <- cbind(data2, lhsFRDE50, lhsFRDE5)
FRDE5 <- lapply(1:13, function(x) lm(data3[, 152+x] ~ shockFR4 + l1.debtDE + l1.intDE + l1.lrtrDE + l1.lrgDE + l1.lryDEc + l2.debtDE + l2.intDE + l2.lrtrDE + l2.lrgDE + l2.lryDEc + l3.debtDE + l3.intDE + l3.lrtrDE + l3.lrgDE + l3.lryDEc + l4.debtDE + l4.intDE + l4.lrtrDE + l4.lrgDE + l4.lryDEc + shockDE4 + shockNL4 + shockES4 + shockIT4, data = data3))
summariesFRDE5 <- lapply(FRDE5, summary)
FRDE5conf95 <- lapply(FRDE5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
FRDE5conf68 <- lapply(FRDE5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
FRDE5up95 <- lapply(1:13, function(x) FRDE5conf95[[x]][2,2])
FRDE5low95 <- lapply(1:13, function(x) FRDE5conf95[[x]][2,1])
FRDE5up68 <- lapply(1:13, function(x) FRDE5conf68[[x]][2,2])
FRDE5low68 <- lapply(1:13, function(x) FRDE5conf68[[x]][2,1])
betaFRDE <- lapply(summariesFRDE5, function(x) x$coefficients[2,1])
names(betaFRDE) <- paste("betaFRDE", 0:h, sep = "")

# -- Equation 6
lhsFRDE60 <- (data3$rgFR - data3$l1.rgFR) / data3$l1.ryDE
lhsFRDE6 <- lapply(1:h, function(x) (data3[, 102+x] - data3$l1.rgFR) / data3$l1.ryDE)
lhsFRDE6 <- data.frame(lhsFRDE6)
names(lhsFRDE6) = paste("lhsFRDE6", 1:h, sep = "")
data3 <- cbind(data3, lhsFRDE60, lhsFRDE6)
FRDE6 <- lapply(1:13, function(x) lm(data3[, 165+x] ~ shockFR3 + l1.debtFR + l1.intFR + l1.lrtrFR + l1.lrgFR + l1.lryFRc + l2.debtFR + l2.intFR + l2.lrtrFR + l2.lrgFR + l2.lryFRc + l3.debtFR + l3.intFR + l3.lrtrFR + l3.lrgFR + l3.lryFRc + l4.debtFR + l4.intFR + l4.lrtrFR + l4.lrgFR + l4.lryFRc + shockDE3 + shockNL3 + shockES3 + shockIT3, data = data3))
summariesFRDE6 <- lapply(FRDE6, summary)
FRDE6conf95 <- lapply(FRDE6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
FRDE6conf68 <- lapply(FRDE6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
FRDE6up95 <- lapply(1:13, function(x) FRDE6conf95[[x]][2,2])
FRDE6low95 <- lapply(1:13, function(x) FRDE6conf95[[x]][2,1])
FRDE6up68 <- lapply(1:13, function(x) FRDE6conf68[[x]][2,2])
FRDE6low68 <- lapply(1:13, function(x) FRDE6conf68[[x]][2,1])
gammaFRDE <- lapply(summariesFRDE6, function(x) x$coefficients[2,1])
names(gammaFRDE) <- paste("gammaFRDE", 0:h, sep = "")

# -- Cumulative multiplier
mFRDEc <- lapply(1:13, function(x) cumsum(as.numeric(betaFRDE))[x] / cumsum(as.numeric(gammaFRDE))[x])
mFRDEc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaFRDE) / as.numeric(gammaFRDE))[x])

# -- Generate IRF graph (as in Fig. 1 of Alloza et al.)
v1 <- data.frame(cbind(betaFRDE = unlist(betaFRDE), FRDE5up95 = unlist(FRDE5up95), FRDE5low95 = unlist(FRDE5low95), 
                       FRDE5up68 = unlist(FRDE5up68), FRDE5low68 = unlist(FRDE5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfFRDE <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfFRDE <- irfFRDE + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfFRDE1 <- irfFRDE + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.75, 0.75, 0.25)) +
  ggtitle("DE - GDP change (GOV shock in FR)")
irfFRDE2 <- irfFRDE1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfFRDE2

# -- Generate IRF graph of GOV response
v1 <- data.frame(cbind(gammaFRDE = unlist(gammaFRDE), FRDE6up95 = unlist(FRDE6up95), FRDE6low95 = unlist(FRDE6low95), 
                       FRDE6up68 = unlist(FRDE6up68), FRDE6low68 = unlist(FRDE6low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfFRDE3 <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfFRDE3 <- irfFRDE3 + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfFRDE4 <- irfFRDE3 + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.2, 0.2, 0.05)) +
  ggtitle("FR - GOV change (GOV shock in FR)")
irfFRDE5 <- irfFRDE4 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfFRDE5


# --- Effect of German govt spending shock on France

# -- Equation 5
lhsDEFR50 <- (data3$ryFR - data3$l1.ryFR) / data3$l1.ryFR
lhsDEFR5 <- lapply(1:h, function(x) (data3[, 78+x] - data3$l1.ryFR) / data3$l1.ryFR)
lhsDEFR5 <- data.frame(lhsDEFR5)
names(lhsDEFR5) = paste("lhsDEFR5", 1:h, sep = "")
data3 <- cbind(data3, lhsDEFR50, lhsDEFR5)
DEFR5 <- lapply(1:13, function(x) lm(data3[, 178+x] ~ shockDE2 + l1.debtFR + l1.intFR + l1.lrtrFR + l1.lrgFR + l1.lryFRc + l2.debtFR + l2.intFR + l2.lrtrFR + l2.lrgFR + l2.lryFRc + l3.debtFR + l3.intFR + l3.lrtrFR + l3.lrgFR + l3.lryFRc + l4.debtFR + l4.intFR + l4.lrtrFR + l4.lrgFR + l4.lryFRc + shockIT2 + shockFR2 + shockES2 + shockNL2, data = data3))
summariesDEFR5 <- lapply(DEFR5, summary)
DEFR5conf95 <- lapply(DEFR5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
DEFR5conf68 <- lapply(DEFR5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
DEFR5up95 <- lapply(1:13, function(x) DEFR5conf95[[x]][2,2])
DEFR5low95 <- lapply(1:13, function(x) DEFR5conf95[[x]][2,1])
DEFR5up68 <- lapply(1:13, function(x) DEFR5conf68[[x]][2,2])
DEFR5low68 <- lapply(1:13, function(x) DEFR5conf68[[x]][2,1])
betaDEFR <- lapply(summariesDEFR5, function(x) x$coefficients[2,1])
names(betaDEFR) <- paste("betaDEFR", 0:h, sep = "")

# -- Equation 6
lhsDEFR60 <- (data3$rgDE - data3$l1.rgDE) / data3$l1.ryFR
lhsDEFR6 <- lapply(1:h, function(x) (data3[, 90+x] - data3$l1.rgDE) / data3$l1.ryFR)
lhsDEFR6 <- data.frame(lhsDEFR6)
names(lhsDEFR6) = paste("lhsDEFR6", 1:h, sep = "")
data3 <- cbind(data3, lhsDEFR60, lhsDEFR6)
DEFR6 <- lapply(1:13, function(x) lm(data3[, 191+x] ~ shockDE3 + l1.debtDE + l1.intDE + l1.lrtrDE + l1.lrgDE + l1.lryFRc + l2.debtDE + l2.intDE + l2.lrtrDE + l2.lrgDE + l2.lryFRc + l3.debtDE + l3.intDE + l3.lrtrDE + l3.lrgDE + l3.lryFRc + l4.debtDE + l4.intDE + l4.lrtrDE + l4.lrgDE + l4.lryFRc + shockIT3 + shockFR3 + shockES3 + shockNL3, data = data3))
summariesDEFR6 <- lapply(DEFR6, summary)
DEFR6conf95 <- lapply(DEFR6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
DEFR6conf68 <- lapply(DEFR6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
DEFR6up95 <- lapply(1:13, function(x) DEFR6conf95[[x]][2,2])
DEFR6low95 <- lapply(1:13, function(x) DEFR6conf95[[x]][2,1])
DEFR6up68 <- lapply(1:13, function(x) DEFR6conf68[[x]][2,2])
DEFR6low68 <- lapply(1:13, function(x) DEFR6conf68[[x]][2,1])
gammaDEFR <- lapply(summariesDEFR6, function(x) x$coefficients[2,1])
names(gammaDEFR) <- paste("gammaDEFR", 1:0, sep = "")

# -- Cumulative multiplier
mDEFRc <- lapply(1:13, function(x) cumsum(as.numeric(betaDEFR))[x] / cumsum(as.numeric(gammaDEFR))[x])
mDEFRc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaDEFR) / as.numeric(gammaDEFR))[x])

# -- Generate IRF graph (as in Fig. 1 of Alloza et al.)
v1 <- data.frame(cbind(betaDEFR = unlist(betaDEFR), DEFR5up95 = unlist(DEFR5up95), DEFR5low95 = unlist(DEFR5low95), 
                       DEFR5up68 = unlist(DEFR5up68), DEFR5low68 = unlist(DEFR5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfDEFR <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfDEFR <- irfDEFR + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfDEFR1 <- irfDEFR + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.5, 1.5, 0.5)) +
  ggtitle("FR - GDP change (GOV shock in DE)")
irfDEFR2 <- irfDEFR1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfDEFR2

# -- Generate IRF graph of GOV response
v1 <- data.frame(cbind(gammaDEFR = unlist(gammaDEFR), DEFR6up95 = unlist(DEFR6up95), DEFR6low95 = unlist(DEFR6low95), 
                       DEFR6up68 = unlist(DEFR6up68), DEFR6low68 = unlist(DEFR6low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfDEFR3 <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfDEFR3 <- irfDEFR3 + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfDEFR4 <- irfDEFR3 + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.25, 0.25, 0.1)) +
  ggtitle("DE - GOV change (GOV shock in DE)")
irfDEFR5 <- irfDEFR4 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfDEFR5
