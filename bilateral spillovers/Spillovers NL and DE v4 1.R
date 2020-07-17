# --- Effect of Dutch govt spending shock on Germany

# Data period: 1980q1-2018q4 and 1999q1-2018q4
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
oNL <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/ABP_other_variables.xlsx", 
                  sheet = "NL1", range = "B1:F157")
colnames(oNL) <- c("rxNL", "rmNL", "R", "D", "pop")
oDE <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/ABP_other_variables.xlsx", 
                  sheet = "DE", range = "B1:F157")
colnames(oDE) <- c("rxDE", "rmDE", "R", "D", "pop")

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
shockES <- ES$shockES
data <- cbind(DE, NL, oNL, oDE, shockIT, shockFR, shockES)
data1 <- subset(data, select = -c(2:14, 28:41, 55:56))
names(data1)[2] <- "debtDE"
names(data1)[14] <- "shockDE"
names(data1)[15] <- "debtNL"
names(data1)[27] <- "shockNL"

# -- Re-scaling of left-hand side of Equations (5) and (6):
data2 <- data1 %>%
  mutate(l1.rgDE = lag(rgDE), l1.rgNL = lag(rgNL), l1.ryDE = lag(ryDE), l1.ryNL = lag(ryNL)) %>%
  mutate(l1.ryES = lag(ES$ryES), l1.ryFR = lag(FR$ryFR), l1.ryIT = lag(IT$ryIT)) %>%
  mutate(l1.lrgDE = log(l1.rgDE), l1.lrgNL = log(l1.rgNL), l1.lryDE = log(l1.ryDE), l1.lryNLc = log(l1.ryNL)) %>%
  mutate(shockDE2 = (shockDE / l1.ryNL) / sd((shockDE / l1.ryNL), na.rm = TRUE), 
         shockFR2 = (shockFR / l1.ryFR) / sd((shockFR / l1.ryFR), na.rm = TRUE),
         shockIT2 = (shockIT / l1.ryIT) / sd((shockIT / l1.ryIT), na.rm = TRUE), 
         shockES2 = (shockES / l1.ryES) / sd((shockES / l1.ryES), na.rm = TRUE),
         shockNL2 = (shockNL / l1.ryDE) / sd((shockNL / l1.ryDE), na.rm = TRUE)) %>% 
  mutate(shockIT3 = shockIT2 / 100, shockES3 = shockES2 / 100, shockFR3 = shockFR2 / 100, 
         shockDE3 = shockDE2 / 100, shockNL3 = shockNL2 / 100) %>%
  mutate(shockIT4 = shockIT2 * 100, shockES4 = shockES2 * 100, shockFR4 = shockFR2 * 100, 
         shockDE4 = shockDE2 * 100, shockNL4 = shockNL2 * 100) %>%
  mutate(ryDE.l1 = lead(ryDE), ryDE.l2 = lead(ryDE, n = 2), ryDE.l3 = lead(ryDE, n = 3), 
         ryDE.l4 = lead(ryDE, n = 4), ryDE.l5 = lead(ryDE, n = 5), ryDE.l6 = lead(ryDE, n = 6),
         ryDE.l7 = lead(ryDE, n = 7), ryDE.l8 = lead(ryDE, n = 8), ryDE.l9 = lead(ryDE, n = 9),
         ryDE.l10 = lead(ryDE, n = 10), ryDE.l11 = lead(ryDE, n = 11), ryDE.l12 = lead(ryDE, n = 12)) %>%
  mutate(ryNL.l1 = lead(ryNL), ryNL.l2 = lead(ryNL, n = 2), ryNL.l3 = lead(ryNL, n = 3), 
         ryNL.l4 = lead(ryNL, n = 4), ryNL.l5 = lead(ryNL, n = 5), ryNL.l6 = lead(ryNL, n = 6),
         ryNL.l7 = lead(ryNL, n = 7), ryNL.l8 = lead(ryNL, n = 8), ryNL.l9 = lead(ryNL, n = 9),
         ryNL.l10 = lead(ryNL, n = 10), ryNL.l11 = lead(ryNL, n = 11), ryNL.l12 = lead(ryNL, n = 12)) %>%
  mutate(rgDE.l1 = lead(rgDE), rgDE.l2 = lead(rgDE, n = 2), rgDE.l3 = lead(rgDE, n = 3), 
         rgDE.l4 = lead(rgDE, n = 4), rgDE.l5 = lead(rgDE, n = 5), rgDE.l6 = lead(rgDE, n = 6),
         rgDE.l7 = lead(rgDE, n = 7), rgDE.l8 = lead(rgDE, n = 8), rgDE.l9 = lead(rgDE, n = 9),
         rgDE.l10 = lead(rgDE, n = 10), rgDE.l11 = lead(rgDE, n = 11), rgDE.l12 = lead(rgDE, n = 12)) %>%
  mutate(rgNL.l1 = lead(rgNL), rgNL.l2 = lead(rgNL, n = 2), rgNL.l3 = lead(rgNL, n = 3), 
         rgNL.l4 = lead(rgNL, n = 4), rgNL.l5 = lead(rgNL, n = 5), rgNL.l6 = lead(rgNL, n = 6),
         rgNL.l7 = lead(rgNL, n = 7), rgNL.l8 = lead(rgNL, n = 8), rgNL.l9 = lead(rgNL, n = 9),
         rgNL.l10 = lead(rgNL, n = 10), rgNL.l11 = lead(rgNL, n = 11), rgNL.l12 = lead(rgNL, n = 12)) %>%
  mutate(l1.debtDE = lag(debtDE, n = 1), l1.intDE = lag(intDE, n = 1), l1.lrtrDE = lag(lrtrDE, n = 1),
         l1.lrgDE = lag(lrgDE, n = 1), l1.lryDEc = lag(lryDEc, n = 1), l2.debtDE = lag(debtDE, n = 2),
         l2.intDE = lag(intDE, n = 2), l2.lrtrDE = lag(lrtrDE, n = 2), l2.lrgDE = lag(lrgDE, n = 2),
         l2.lryDEc = lag(lryDEc, n = 2), l3.debtDE = lag(debtDE, n = 3), l3.intDE = lag(intDE, n = 3),
         l3.lrtrDE = lag(lrtrDE, n = 3), l3.lrgDE = lag(lrgDE, n = 3), l3.lryDEc = lag(lryDEc, n = 3),
         l4.debtDE = lag(debtDE, n = 4), l4.intDE = lag(intDE, n = 4), l4.lrtrDE = lag(lrtrDE, n = 4),
         l4.lrgDE = lag(lrgDE, n = 4), l4.lryDEc = lag(lryDEc, n = 4)) %>%
  mutate(l1.debtNL = lag(debtNL, n = 1), l1.intNL = lag(intNL, n = 1), l1.lrtrNL = lag(lrtrNL, n = 1),
         l1.lrgNL = lag(lrgNL, n = 1), l1.lryNLc = lag(lryNLc, n = 1), l2.debtNL = lag(debtNL, n = 2),
         l2.intNL = lag(intNL, n = 2), l2.lrtrNL = lag(lrtrNL, n = 2), l2.lrgNL = lag(lrgNL, n = 2),
         l2.lryNLc = lag(lryNLc, n = 2), l3.debtNL = lag(debtNL, n = 3), l3.intNL = lag(intNL, n = 3),
         l3.lrtrNL = lag(lrtrNL, n = 3), l3.lrgNL = lag(lrgNL, n = 3), l3.lryNLc = lag(lryNLc, n = 3),
         l4.debtNL = lag(debtNL, n = 4), l4.intNL = lag(intNL, n = 4), l4.lrtrNL = lag(lrtrNL, n = 4),
         l4.lrgNL = lag(lrgNL, n = 4), l4.lryNLc = lag(lryNLc, n = 4))

# -- OLS regressions

# -- Equation 5
lhsNLDE50 <- (data2$ryDE - data2$l1.ryDE) / data2$l1.ryDE
lhsNLDE5 <- lapply(1:h, function(x) (data2[, 66+x] - data2$l1.ryDE) / data2$l1.ryDE)
lhsNLDE5 <- data.frame(lhsNLDE5)
names(lhsNLDE5) = paste("lhsNLDE5", 1:h, sep = "")
data3 <- cbind(data2, lhsNLDE50, lhsNLDE5)
NLDE5 <- lapply(1:13, function(x) lm(data3[, 151+x] ~ shockNL2 + l1.debtDE + l1.intDE + l1.lrtrDE + l1.lrgDE + l1.lryDEc + l2.debtDE + l2.intDE + l2.lrtrDE + l2.lrgDE + l2.lryDEc + l3.debtDE + l3.intDE + l3.lrtrDE + l3.lrgDE + l3.lryDEc + l4.debtDE + l4.intDE + l4.lrtrDE + l4.lrgDE + l4.lryDEc + shockDE2 + shockFR2 + shockES2 + shockIT2, data = data3))
summariesNLDE5 <- lapply(NLDE5, summary)
NLDE5conf95 <- lapply(NLDE5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
NLDE5conf68 <- lapply(NLDE5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
NLDE5up95 <- lapply(1:13, function(x) NLDE5conf95[[x]][2,2])
NLDE5low95 <- lapply(1:13, function(x) NLDE5conf95[[x]][2,1])
NLDE5up68 <- lapply(1:13, function(x) NLDE5conf68[[x]][2,2])
NLDE5low68 <- lapply(1:13, function(x) NLDE5conf68[[x]][2,1])
betaNLDE <- lapply(summariesNLDE5, function(x) x$coefficients[2,1])
names(betaNLDE) <- paste("betaNLDE", 0:h, sep = "")

# -- Equation 6
lhsNLDE60 <- (data3$rgNL - data3$l1.rgNL) / data3$l1.ryDE
lhsNLDE6 <- lapply(1:h, function(x) (data3[, 102+x] - data3$l1.rgNL) / data3$l1.ryDE)
lhsNLDE6 <- data.frame(lhsNLDE6)
names(lhsNLDE6) = paste("lhsNLDE6", 1:h, sep = "")
data3 <- cbind(data3, lhsNLDE60, lhsNLDE6)
NLDE6 <- lapply(1:13, function(x) lm(data3[, 164+x] ~ shockNL3 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc + shockDE3 + shockFR3 + shockES3 + shockIT3, data = data3))
summariesNLDE6 <- lapply(NLDE6, summary)
NLDE6conf95 <- lapply(NLDE6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
NLDE6conf68 <- lapply(NLDE6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
NLDE6up95 <- lapply(1:13, function(x) NLDE6conf95[[x]][2,2])
NLDE6low95 <- lapply(1:13, function(x) NLDE6conf95[[x]][2,1])
NLDE6up68 <- lapply(1:13, function(x) NLDE6conf68[[x]][2,2])
NLDE6low68 <- lapply(1:13, function(x) NLDE6conf68[[x]][2,1])
gammaNLDE <- lapply(summariesNLDE6, function(x) x$coefficients[2,1])
names(gammaNLDE) <- paste("gammaNLDE", 0:h, sep = "")

# -- Cumulative multiplier
mNLDEc <- lapply(1:13, function(x) cumsum(as.numeric(betaNLDE))[x] / cumsum(as.numeric(gammaNLDE))[x])
mNLDEc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaNLDE) / as.numeric(gammaNLDE))[x])

# -- Generate IRF graph (as in Fig. 1 of Alloza et al.)
v1 <- data.frame(cbind(betaNLDE = unlist(betaNLDE), NLDE5up95 = unlist(NLDE5up95), NLDE5low95 = unlist(NLDE5low95), 
                       NLDE5up68 = unlist(NLDE5up68), NLDE5low68 = unlist(NLDE5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfNLDE <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfNLDE <- irfNLDE + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfNLDE1 <- irfNLDE + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.0004, 0.0008, 0.00002)) +
  ggtitle("DE - GDP change (GOV shock in NL)")
irfNLDE2 <- irfNLDE1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfNLDE2

# -- Generate IRF graph of GOV response
v1 <- data.frame(cbind(gammaNLDE = unlist(gammaNLDE), NLDE6up95 = unlist(NLDE6up95), NLDE6low95 = unlist(NLDE6low95), 
                       NLDE6up68 = unlist(NLDE6up68), NLDE6low68 = unlist(NLDE6low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfNLDE3 <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfNLDE3 <- irfNLDE3 + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfNLDE4 <- irfNLDE3 + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.1, 0.15, 0.05)) +
  ggtitle("NL - GOV change (GOV shock in NL)")
irfNLDE5 <- irfNLDE4 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfNLDE5


# --- Effect of German govt spending shock on Netherlands

# -- Equation 5
lhsDENL50 <- (data3$ryNL - data3$l1.ryNL) / data3$l1.ryNL
lhsDENL5 <- lapply(1:h, function(x) (data3[, 78+x] - data3$l1.ryNL) / data3$l1.ryNL)
lhsDENL5 <- data.frame(lhsDENL5)
names(lhsDENL5) = paste("lhsDENL5", 1:h, sep = "")
data3 <- cbind(data3, lhsDENL50, lhsDENL5)
DENL5 <- lapply(1:13, function(x) lm(data3[, 177+x] ~ shockDE3 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc + shockIT3 + shockFR3 + shockES3 + shockNL3, data = data3))
summariesDENL5 <- lapply(DENL5, summary)
DENL5conf95 <- lapply(DENL5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
DENL5conf68 <- lapply(DENL5, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
DENL5up95 <- lapply(1:13, function(x) DENL5conf95[[x]][2,2])
DENL5low95 <- lapply(1:13, function(x) DENL5conf95[[x]][2,1])
DENL5up68 <- lapply(1:13, function(x) DENL5conf68[[x]][2,2])
DENL5low68 <- lapply(1:13, function(x) DENL5conf68[[x]][2,1])
betaDENL <- lapply(summariesDENL5, function(x) x$coefficients[2,1])
names(betaDENL) <- paste("betaDENL", 0:h, sep = "")

# -- Equation 6
lhsDENL60 <- (data3$rgDE - data3$l1.rgDE) / data3$l1.ryNL
lhsDENL6 <- lapply(1:h, function(x) (data3[, 90+x] - data3$l1.rgDE) / data3$l1.ryNL)
lhsDENL6 <- data.frame(lhsDENL6)
names(lhsDENL6) = paste("lhsDENL6", 1:h, sep = "")
data3 <- cbind(data3, lhsDENL60, lhsDENL6)
DENL6 <- lapply(1:13, function(x) lm(data3[, 190+x] ~ shockDE3 + l1.debtNL + l1.intNL + l1.lrtrNL + l1.lrgNL + l1.lryNLc + l2.debtNL + l2.intNL + l2.lrtrNL + l2.lrgNL + l2.lryNLc + l3.debtNL + l3.intNL + l3.lrtrNL + l3.lrgNL + l3.lryNLc + l4.debtNL + l4.intNL + l4.lrtrNL + l4.lrgNL + l4.lryNLc + shockIT3 + shockFR3 + shockES3 + shockNL3, data = data3))
summariesDENL6 <- lapply(DENL6, summary)
DENL6conf95 <- lapply(DENL6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.95)
DENL6conf68 <- lapply(DENL6, coefci, vcov = NeweyWest, lag = 0, prewhite = FALSE, level = 0.68)
DENL6up95 <- lapply(1:13, function(x) DENL6conf95[[x]][2,2])
DENL6low95 <- lapply(1:13, function(x) DENL6conf95[[x]][2,1])
DENL6up68 <- lapply(1:13, function(x) DENL6conf68[[x]][2,2])
DENL6low68 <- lapply(1:13, function(x) DENL6conf68[[x]][2,1])
gammaDENL <- lapply(summariesDENL6, function(x) x$coefficients[2,1])
names(gammaDENL) <- paste("gammaDENL", 0:h, sep = "")

# -- Cumulative multiplier
mDENLc <- lapply(1:13, function(x) cumsum(as.numeric(betaDENL))[x] / cumsum(as.numeric(gammaDENL))[x])
mDENLc2 <- lapply(1:13, function(x) cumsum(as.numeric(betaDENL) / as.numeric(gammaDENL))[x])

# -- Generate IRF graph (as in Fig. 1 of Alloza et al.)
v1 <- data.frame(cbind(betaDENL = unlist(betaDENL), DENL5up95 = unlist(DENL5up95), DENL5low95 = unlist(DENL5low95), 
                       DENL5up68 = unlist(DENL5up68), DENL5low68 = unlist(DENL5low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfDENL <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfDENL <- irfDENL + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfDENL1 <- irfDENL + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.004, 0.008, 0.004)) +
  ggtitle("NL - GDP change (GOV shock in DE)")
irfDENL2 <- irfDENL1 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfDENL2

# -- Generate IRF graph of GOV response
v1 <- data.frame(cbind(gammaDENL = unlist(gammaDENL), DENL6up95 = unlist(DENL6up95), DENL6low95 = unlist(DENL6low95), 
                       DENL6up68 = unlist(DENL6up68), DENL6low68 = unlist(DENL6low68)))
quarter <- data.frame(0:12)
df.v1 <- cbind(quarter, v1)
colnames(df.v1) <- c("quarters", "percent", "up95", "low95", "up68", "low68")
irfDENL3 <- ggplot(df.v1, aes(x = quarters, y = percent)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed") + geom_line()
irfDENL3 <- irfDENL3 + geom_ribbon(aes(ymin = low95, ymax = up95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin = low68, ymax = up68), linetype=2, alpha=0.1)
irfDENL4 <- irfDENL3 + coord_cartesian(xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(breaks = seq(-0.8, 0.4, 0.2)) +
  ggtitle("DE - GOV change (GOV shock in DE)")
irfDENL5 <- irfDENL4 + theme(plot.background = element_rect(fill = "white", color = "white"),
                             panel.background = element_rect(fill = "white"), 
                             panel.border = element_rect(linetype = "solid", fill = NA))
irfDENL5
