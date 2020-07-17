# --- Effect of Italian govt spending shock on Germany

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
colnames(oIT) <- c("rxIT", "rmIT", "R", "D", "popIT")
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
shockFR <- FR$shockFR
shockNL <- NL$shockNL
shockES <- ES$shockES
data <- cbind(DE, IT, oIT, oDE, shockNL, shockFR, shockES)
data1 <- subset(data, select = -c(2:14, 28:41))
names(data1)[2] <- "debtDE"
names(data1)[14] <- "shockDE"
names(data1)[15] <- "debtIT"
names(data1)[27] <- "shockIT"

# -- Re-scaling of left-hand side of Equations (5) and (6):
data2 <- data1 %>%
  mutate(l1.rgDE = lag(rgDE), l1.rgIT = lag(rgIT), l1.ryDE = lag(ryDE), l1.ryIT = lag(ryIT)) %>%
  mutate(l1.ryES = lag(ES$ryES), l1.ryFR = lag(FR$ryFR), l1.ryNL = lag(NL$ryNL)) %>%
  mutate(l1.lrgDE = log(l1.rgDE), l1.lrgIT = log(l1.rgIT), l1.lryDE = log(l1.ryDE), l1.lryITc = log(l1.ryIT)) %>%
  mutate(shockIT2 = (shockIT / l1.ryDE) / sd((shockIT / l1.ryDE), na.rm = TRUE), 
         shockNL2 = (shockNL / l1.ryNL) / sd((shockNL / l1.ryNL), na.rm = TRUE), 
         shockES2 = (shockES / l1.ryES) / sd((shockES / l1.ryES), na.rm = TRUE), 
         shockFR2 = (shockFR / l1.ryFR) / sd((shockFR / l1.ryFR), na.rm = TRUE), 
         shockDE2 = (shockDE / l1.ryIT) / sd((shockDE / l1.ryIT), na.rm = TRUE)) %>%
  mutate(shockIT3 = shockIT2 / 100, shockNL3 = shockNL2 / 100, shockES3 = shockES2 / 100, 
         shockFR3 = shockFR2 / 100, shockDE3 = shockDE2 / 100) %>%
  mutate(shockIT4 = shockIT2 / 10000, shockNL4 = shockNL2 / 10000, shockES4 = shockES2 / 10000, 
         shockFR4 = shockFR2 / 10000, shockDE4 = shockDE2 / 10000) %>%
  mutate(ryDE.l1 = lead(ryDE), ryDE.l2 = lead(ryDE, n = 2), ryDE.l3 = lead(ryDE, n = 3), 
         ryDE.l4 = lead(ryDE, n = 4), ryDE.l5 = lead(ryDE, n = 5), ryDE.l6 = lead(ryDE, n = 6),
         ryDE.l7 = lead(ryDE, n = 7), ryDE.l8 = lead(ryDE, n = 8), ryDE.l9 = lead(ryDE, n = 9),
         ryDE.l10 = lead(ryDE, n = 10), ryDE.l11 = lead(ryDE, n = 11), ryDE.l12 = lead(ryDE, n = 12)) %>%
  mutate(ryIT.l1 = lead(ryIT), ryIT.l2 = lead(ryIT, n = 2), ryIT.l3 = lead(ryIT, n = 3), 
         ryIT.l4 = lead(ryIT, n = 4), ryIT.l5 = lead(ryIT, n = 5), ryIT.l6 = lead(ryIT, n = 6),
         ryIT.l7 = lead(ryIT, n = 7), ryIT.l8 = lead(ryIT, n = 8), ryIT.l9 = lead(ryIT, n = 9),
         ryIT.l10 = lead(ryIT, n = 10), ryIT.l11 = lead(ryIT, n = 11), ryIT.l12 = lead(ryIT, n = 12)) %>%
  mutate(rgDE.l1 = lead(rgDE), rgDE.l2 = lead(rgDE, n = 2), rgDE.l3 = lead(rgDE, n = 3), 
         rgDE.l4 = lead(rgDE, n = 4), rgDE.l5 = lead(rgDE, n = 5), rgDE.l6 = lead(rgDE, n = 6),
         rgDE.l7 = lead(rgDE, n = 7), rgDE.l8 = lead(rgDE, n = 8), rgDE.l9 = lead(rgDE, n = 9),
         rgDE.l10 = lead(rgDE, n = 10), rgDE.l11 = lead(rgDE, n = 11), rgDE.l12 = lead(rgDE, n = 12)) %>%
  mutate(rgIT.l1 = lead(rgIT), rgIT.l2 = lead(rgIT, n = 2), rgIT.l3 = lead(rgIT, n = 3), 
         rgIT.l4 = lead(rgIT, n = 4), rgIT.l5 = lead(rgIT, n = 5), rgIT.l6 = lead(rgIT, n = 6),
         rgIT.l7 = lead(rgIT, n = 7), rgIT.l8 = lead(rgIT, n = 8), rgIT.l9 = lead(rgIT, n = 9),
         rgIT.l10 = lead(rgIT, n = 10), rgIT.l11 = lead(rgIT, n = 11), rgIT.l12 = lead(rgIT, n = 12)) %>%
  mutate(l1.debtDE = lag(debtDE, n = 1), l1.intDE = lag(intDE, n = 1), l1.lrtrDE = lag(lrtrDE, n = 1),
         l1.lrgDE = lag(lrgDE, n = 1), l1.lryDEc = lag(lryDEc, n = 1), l2.debtDE = lag(debtDE, n = 2),
         l2.intDE = lag(intDE, n = 2), l2.lrtrDE = lag(lrtrDE, n = 2), l2.lrgDE = lag(lrgDE, n = 2),
         l2.lryDEc = lag(lryDEc, n = 2), l3.debtDE = lag(debtDE, n = 3), l3.intDE = lag(intDE, n = 3),
         l3.lrtrDE = lag(lrtrDE, n = 3), l3.lrgDE = lag(lrgDE, n = 3), l3.lryDEc = lag(lryDEc, n = 3),
         l4.debtDE = lag(debtDE, n = 4), l4.intDE = lag(intDE, n = 4), l4.lrtrDE = lag(lrtrDE, n = 4),
         l4.lrgDE = lag(lrgDE, n = 4), l4.lryDEc = lag(lryDEc, n = 4)) %>%
  mutate(l1.debtIT = lag(debtIT, n = 1), l1.intIT = lag(intIT, n = 1), l1.lrtrIT = lag(lrtrIT, n = 1),
         l1.lrgIT = lag(lrgIT, n = 1), l1.lryITc = lag(lryITc, n = 1), l2.debtIT = lag(debtIT, n = 2),
         l2.intIT = lag(intIT, n = 2), l2.lrtrIT = lag(lrtrIT, n = 2), l2.lrgIT = lag(lrgIT, n = 2),
         l2.lryITc = lag(lryITc, n = 2), l3.debtIT = lag(debtIT, n = 3), l3.intIT = lag(intIT, n = 3),
         l3.lrtrIT = lag(lrtrIT, n = 3), l3.lrgIT = lag(lrgIT, n = 3), l3.lryITc = lag(lryITc, n = 3),
         l4.debtIT = lag(debtIT, n = 4), l4.intIT = lag(intIT, n = 4), l4.lrtrIT = lag(lrtrIT, n = 4),
         l4.lrgIT = lag(lrgIT, n = 4), l4.lryITc = lag(lryITc, n = 4))


# --- OLS regressions

# -- Equation 5
lhsITDE50 <- (data2$ryDE - data2$l1.ryDE) / data2$l1.ryDE
lhsITDE5 <- lapply(1:h, function(x) (data2[, 66+x] - data2$l1.ryDE) / data2$l1.ryDE)
lhsITDE5 <- data.frame(lhsITDE5)
names(lhsITDE5) = paste("lhsITDE5", 1:h, sep = "")
data3 <- cbind(data2, lhsITDE50, lhsITDE5)
ITDE5 <- lapply(1:13, function(x) lm(data3[, 151+x] ~ shockIT2 + l1.debtDE + l1.intDE + l1.lrtrDE + l1.lrgDE + l1.lryDEc + l2.debtDE + l2.intDE + l2.lrtrDE + l2.lrgDE + l2.lryDEc + l3.debtDE + l3.intDE + l3.lrtrDE + l3.lrgDE + l3.lryDEc + l4.debtDE + l4.intDE + l4.lrtrDE + l4.lrgDE + l4.lryDEc + shockDE2 + shockFR2 + shockES2 + shockNL2, data = data3))
summariesITDE5 <- lapply(ITDE5, summary)
ITDE5conf95 <- lapply(ITDE5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ITDE5conf68 <- lapply(ITDE5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ITDE5up95 <- lapply(1:13, function(x) ITDE5conf95[[x]][2,2])
ITDE5low95 <- lapply(1:13, function(x) ITDE5conf95[[x]][2,1])
ITDE5up68 <- lapply(1:13, function(x) ITDE5conf68[[x]][2,2])
ITDE5low68 <- lapply(1:13, function(x) ITDE5conf68[[x]][2,1])
betaITDE <- lapply(summariesITDE5, function(x) x$coefficients[2,1])
names(betaITDE) <- paste("betaITDE", 0:h, sep = "")

# -- Equation 6
lhsITDE60 <- (data3$rgIT - data3$l1.rgIT) / data3$l1.ryDE
lhsITDE6 <- lapply(1:h, function(x) (data3[, 102+x] - data3$l1.rgIT) / data3$l1.ryDE)
lhsITDE6 <- data.frame(lhsITDE6)
names(lhsITDE6) = paste("lhsITDE6", 1:h, sep = "")
data3 <- cbind(data3, lhsITDE60, lhsITDE6)
ITDE6 <- lapply(1:13, function(x) lm(data3[, 164+x] ~ shockIT3 + l1.debtIT + l1.intIT + l1.lrtrIT + l1.lrgIT + l1.lryITc + l2.debtIT + l2.intIT + l2.lrtrIT + l2.lrgIT + l2.lryITc + l3.debtIT + l3.intIT + l3.lrtrIT + l3.lrgIT + l3.lryITc + l4.debtIT + l4.intIT + l4.lrtrIT + l4.lrgIT + l4.lryITc + shockDE3 + shockFR3 + shockES3 + shockNL3, data = data3))
summariesITDE6 <- lapply(ITDE6, summary)
ITDE6conf95 <- lapply(ITDE6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
ITDE6conf68 <- lapply(ITDE6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
ITDE6up95 <- lapply(1:13, function(x) ITDE6conf95[[x]][2,2])
ITDE6low95 <- lapply(1:13, function(x) ITDE6conf95[[x]][2,1])
ITDE6up68 <- lapply(1:13, function(x) ITDE6conf68[[x]][2,2])
ITDE6low68 <- lapply(1:13, function(x) ITDE6conf68[[x]][2,1])
gammaITDE <- lapply(summariesITDE6, function(x) x$coefficients[2,1])
names(gammaITDE) <- paste("gammaITDE", 0:h, sep = "")

# -- Cumulative multiplier
mITDEc <- lapply(1:13, function(x) cumsum(as.numeric(betaITDE))[x] / cumsum(as.numeric(gammaITDE))[x])
mITDEc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaITDE) / as.numeric(gammaITDE))[x])

# -- Generate IRF graph of GDP response
v1 <- data.frame(cbind(betaITDE = unlist(betaITDE), ITDE5up95 = unlist(ITDE5up95), ITDE5low95 = unlist(ITDE5low95), 
                       ITDE5up68 = unlist(ITDE5up68), ITDE5low68 = unlist(ITDE5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfITDE <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfITDE <- irfITDE + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfITDE1 <- irfITDE + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-1, 1, 0.25)) +
  ggtitle("DE - GDP change (GOV shock in IT)")
irfITDE2 <- irfITDE1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfITDE2

# -- Generate IRF graph of GOV response
v1 <- data.frame(cbind(gammaITDE = unlist(gammaITDE), ITDE6up95 = unlist(ITDE6up95), ITDE6low95 = unlist(ITDE6low95), 
                       ITDE6up68 = unlist(ITDE6up68), ITDE6low68 = unlist(ITDE6low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfITDE3 <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfITDE3 <- irfITDE3 + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfITDE4 <- irfITDE3 + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.15, 0.25, 0.1)) +
  ggtitle("IT - GOV change (GOV shock in IT)")
irfITDE5 <- irfITDE4 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfITDE5


# --- Effect of German govt spending shock on Italy

# -- Equation 5
lhsDEIT50 <- (data3$ryIT - data3$l1.ryIT) / data3$l1.ryIT
lhsDEIT5 <- lapply(1:h, function(x) (data3[, 78+x] - data3$l1.ryIT) / data3$l1.ryIT)
lhsDEIT5 <- data.frame(lhsDEIT5)
names(lhsDEIT5) = paste("lhsDEIT5", 1:h, sep = "")
data3 <- cbind(data3, lhsDEIT50, lhsDEIT5)
DEIT5 <- lapply(1:13, function(x) lm(data3[, 177+x] ~ shockDE2 + l1.debtIT + l1.intIT + l1.lrtrIT + l1.lrgIT + l1.lryITc + l2.debtIT + l2.intIT + l2.lrtrIT + l2.lrgIT + l2.lryITc + l3.debtIT + l3.intIT + l3.lrtrIT + l3.lrgIT + l3.lryITc + l4.debtIT + l4.intIT + l4.lrtrIT + l4.lrgIT + l4.lryITc + shockIT2 + shockFR2 + shockES2 + shockNL2, data = data3))
summariesDEIT5 <- lapply(DEIT5, summary)
DEIT5conf95 <- lapply(DEIT5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
DEIT5conf68 <- lapply(DEIT5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
DEIT5up95 <- lapply(1:13, function(x) DEIT5conf95[[x]][2,2])
DEIT5low95 <- lapply(1:13, function(x) DEIT5conf95[[x]][2,1])
DEIT5up68 <- lapply(1:13, function(x) DEIT5conf68[[x]][2,2])
DEIT5low68 <- lapply(1:13, function(x) DEIT5conf68[[x]][2,1])
betaDEIT <- lapply(summariesDEIT5, function(x) x$coefficients[2,1])
names(betaDEIT) <- paste("betaDEIT", 0:h, sep = "")

# -- Equation 6
lhsDEIT60 <- (data3$rgDE - data3$l1.rgDE) / data3$l1.ryIT
lhsDEIT6 <- lapply(1:h, function(x) (data3[, 90+x] - data3$l1.rgDE) / data3$l1.ryIT)
lhsDEIT6 <- data.frame(lhsDEIT6)
names(lhsDEIT6) = paste("lhsDEIT6", 1:h, sep = "")
data3 <- cbind(data3, lhsDEIT60, lhsDEIT6)
DEIT6 <- lapply(1:13, function(x) lm(data3[, 190+x] ~ shockDE3 + l1.debtDE + l1.intDE + l1.lrtrDE + l1.lrgDE + l1.lryDEc + l2.debtDE + l2.intDE + l2.lrtrDE + l2.lrgDE + l2.lryDEc + l3.debtDE + l3.intDE + l3.lrtrDE + l3.lrgDE + l3.lryDEc + l4.debtDE + l4.intDE + l4.lrtrDE + l4.lrgDE + l4.lryDEc + shockIT3 + shockFR3 + shockES3 + shockNL3, data = data3))
summariesDEIT6 <- lapply(DEIT6, summary)
DEIT6conf95 <- lapply(DEIT6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
DEIT6conf68 <- lapply(DEIT6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
DEIT6up95 <- lapply(1:13, function(x) DEIT6conf95[[x]][2,2])
DEIT6low95 <- lapply(1:13, function(x) DEIT6conf95[[x]][2,1])
DEIT6up68 <- lapply(1:13, function(x) DEIT6conf68[[x]][2,2])
DEIT6low68 <- lapply(1:13, function(x) DEIT6conf68[[x]][2,1])
gammaDEIT <- lapply(summariesDEIT6, function(x) x$coefficients[2,1])
names(gammaDEIT) <- paste("gammaDEIT", 0:h, sep = "")

# -- Cumulative multiplier
mDEITc <- lapply(1:13, function(x) cumsum(as.numeric(betaDEIT))[x] / cumsum(as.numeric(gammaDEIT))[x])
mDEITc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaDEIT) / as.numeric(gammaDEIT))[x])

# -- Generate IRF graph of GDP response
v1 <- data.frame(cbind(betaDEIT = unlist(betaDEIT), DEIT5up95 = unlist(DEIT5up95), DEIT5low95 = unlist(DEIT5low95), 
                       DEIT5up68 = unlist(DEIT5up68), DEIT5low68 = unlist(DEIT5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfDEIT <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfDEIT <- irfDEIT + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfDEIT1 <- irfDEIT + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.6, 1.8, 0.4)) +
  ggtitle("IT - GDP change (GOV shock in DE)")
irfDEIT2 <- irfDEIT1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfDEIT2

# -- Generate IRF graph of GOV response
v1 <- data.frame(cbind(gammaDEIT = unlist(gammaDEIT), DEIT6up95 = unlist(DEIT6up95), DEIT6low95 = unlist(DEIT6low95), 
                       DEIT6up68 = unlist(DEIT6up68), DEIT6low68 = unlist(DEIT6low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfDEIT3 <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfDEIT3 <- irfDEIT3 + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfDEIT4 <- irfDEIT3 + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.1)) +
  ggtitle("DE - GOV change (GOV shock in DE)")
irfDEIT5 <- irfDEIT4 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfDEIT5
